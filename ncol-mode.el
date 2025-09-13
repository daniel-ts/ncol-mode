;; -*- lexical-binding: t -*-

(require 'dash)
(require 'window)

(defvar ncol-previous-display-buffer-state nil
  "The value of `display-buffer-base-action` to restore when ncol-mode is
deactivated.")

(defvar ncol-column-min-width 80
  "The minimal width a column can have after a vertical split. Otherwise the split will be done horizontally.")

(defun ncol--window-tree () (car (window-tree)))

(defun ncol--find-topmost-vertical-split (tree)
  (cond
   ;; I'm seeing a window without a split
   ((windowp tree)
    (list tree))

   ;; I'm seeing a split
   ((listp tree)
    (if (car tree) ; yields t or nil
        ;; found horizontal split: descend to first found split
        (ncol--find-topmost-vertical-split (-find (lambda (node) (listp node)) (cddr tree)))
      ;; found vertical split: find first window and return it's parent
      tree))))

(defun ncol--find-first-window (tree)
  (cond ((windowp tree)
         tree)
        ((listp tree)
         (-find (lambda (node) (windowp node)) (cddr tree)))))

(defun ncol--find-last-window (tree)
  (cond ((windowp tree)
         tree)
        ((listp tree)
         (-find (lambda (node) (windowp node)) (nreverse (cddr tree))))))

(defun ncol--size-of-split (tree)
  (cond ((windowp tree)
         1)
        ((listp tree)
         (length (cddr tree)))))

(defun ncol--window-ancestor (window n)
  (if (= n 0)
      window
    (ncol--window-ancestor (window-parent window) (1-  n))))

(defun ncol--split-descend-iter (window-or-split depth)
  (cond (;; is a window, unwind
         (windowp window-or-split)
         (ncol--window-ancestor window-or-split depth))

        (;; is a split: descend
         (listp window-or-split)
         (ncol--split-descend-iter (caddr window-or-split) (1+ depth)))

        (;; error
         t
         (error "ncol--split-to-window: unexpected object."))))

(defun ncol--windows-of-split (split)
  "Convert a SPLIT into a list of child windows. A subsplit is converted to the parent of the first child."
  (-map (lambda (item)
          (cond (;; first
                 (window-live-p item)
                 item)

                (;; asdf
                 (listp item)
                 (ncol--split-descend-iter item 0))

                (;; error
                 t
                 ("ncol--window-of-split: unexpected object"))))
        (cddr split)))

(defun ncol--window-descendent-p (window parent)
  "Check if WINDOW is descendent of PARENT. A window is a descendent of itself. The windows don't have to be live."
  (cond ((null window)
         nil)

        ((eq window parent)
         t)

        (;; recur
         t
         (ncol--window-descendent-p (window-parent window) parent))))

(defun ncol--rebalance-n-columns ()
  "Rebalance top-level column window group as defined by
`ncol--display-buffer-n-columns'. Intended for `window-state-change-hook'."
  (balance-windows
   (window-parent
    (ncol--find-first-window
     (ncol--find-topmost-vertical-split (ncol--window-tree))))))

(defun ncol-display-buffer-n-columns (buffer alist)
  "Split the current column to the right if the new column would have width
exceeding `ncol-column-min-width'."
  (let* ((top-columns
          (ncol--windows-of-split
           (ncol--find-topmost-vertical-split
            (ncol--window-tree))))
         (column-count (length top-columns))
         (resulting-width (/ (frame-width) (+ column-count 1))))

    (when (>= resulting-width ncol-column-min-width)
      (let* ((col-to-split
              (-find  (lambda (col) (ncol--window-descendent-p (selected-window) col))
                      top-columns))
             (new-window (split-window col-to-split nil 'right)))

        (set-window-buffer new-window buffer)
        (balance-windows (window-parent new-window))
        (select-window new-window))
      )))

(defun ncol-display-buffer-split-below (buffer alist)
  (let ((new-window (split-window (selected-window) nil 'below)))
    (add-hook 'window-state-change-hook #'ncol--rebalance-n-columns)
    (set-window-buffer new-window buffer)
    (balance-windows (window-parent new-window))
    (select-window new-window)))

(defun ncol-display-buffer-replace-ibuffer (buffer alist)
  "If the current buffer is the ibuffer then display BUFFER in the ibuffer
window."
  (when (string= (buffer-name (current-buffer)) "*Ibuffer*")
    (let ((window (get-buffer-window (current-buffer))))
      (set-window-buffer window buffer)
      (select-window window))))

(defcustom display-buffer-ncol-action '((display-buffer-reuse-window
                                         ncol-display-buffer-replace-ibuffer
                                         ncol-display-buffer-n-columns
                                         ncol-display-buffer-split-below)
                                        . ((reusable-frames . nil)))
  "Structured like `display-buffer-base-action' etc. You can customize of
ncol-mode displays buffers by interleaving your own desired functions."
  :type display-buffer--action-custom-type ;; from window.el
  :risky t
  :group 'ncol-mode)

(defvar ncol-mode-map
  (let ((-map (make-sparse-keymap)))
    (define-key -map (kbd "C-x C-f") #'find-file-other-window)
    (define-key -map (kbd "C-x d") (lambda ()
                                     (interactive)
                                     (dired-other-window default-directory)))
    -map)
  "Keymap for `my/display-buffer-mode'.")

(define-minor-mode ncol-mode
  "Toggle a special buffer display configuration."
  :global t
  :lighter " ncol"
  (if ncol-mode
      ;; Enabling
      (progn
        (setq ncol-previous-display-buffer-state display-buffer-base-action)
        (setq display-buffer-base-action display-buffer-ncol-action)
        (add-hook 'window-state-change-hook #'ncol--rebalance-n-columns))
    ;; Disabling
    (progn
      (setq display-buffer-base-action ncol-previous-display-buffer-state)
      (remove-hook 'window-state-change-hook #'ncol--rebalance-n-columns))))
