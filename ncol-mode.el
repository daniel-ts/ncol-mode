;;; ncol-mode.el --- Window management in vertical columns -*- lexical-binding: t -*-

;;; Commentary:
;; ncol-mode: This packages implements display-buffer functions for window
;; management.  The windows are arranged in vertical columns until the
;; column width falls below the minimal width, then each new window splits
;; a column horizontally.


;;; Code:
(require 'cl-lib)
(require 'window)

(defgroup ncol-mode nil
  "Window management in vertical columns until minimum width is reached.
Then split horizontally."
  :group 'windows
  :prefix "ncol-")

(defvar ncol-previous-display-buffer-state nil
  "The previous value of `display-buffer-base-action`.
The value is restored when ncol-mode is deactivated.")

(defun ncol--window-v-split-p (window)
  "Returns non-nil if WINDOW has a vertical split.  Nil otherwise."
  (if (window-left-child window) t nil))

(defun ncol--window-h-split-p (window)
  "Returns non-nil if WINDOW has a horizontal split.  Nil otherwise."
  (if (window-top-child window) t nil))

(defun ncol--window-split-p (window)
  "Returns non-nil if WINDOW has any split.  Nil otherwise."
  (if (window-child window) t nil))

(defun ncol--window-v-splittable-p (window min-width)
  "Calculates if a vertical split of WINDOW give each split MIN-WIDTH.
t if that is the case, nil otherwise."
  (>= (/ (window-total-width window)
         (if (ncol--window-v-split-p window)
             (1+ (window-child-count window))
           2))
      min-width))

(defun ncol--window-h-splittable-p (window min-height)
  "Calculates if a horizontal split of WINDOW give each split MIN-HEIGHT.
t if that is the case, nil otherwise."
  (>= (/ (window-total-height window)
         (if (ncol--window-h-split-p window)
             (1+ (window-child-count window))
           2))
      min-height))

(defun ncol--side-window-p (window &optional side)
  "Tests if WINDOW is a side window.
When optional SIDE is supplied, test if the window is a side window and
on that side."
  (if side
      (eq (window-parameter window 'window-side) side)
    (not (null (window-parameter window 'window-side)))))

(defun ncol--find-cur-main-column (window)
  "Finds the main column for WINDOW."

  (cond (;; I'm in the root window
         (eq (frame-root-window) window)
         (window-main-window))

        (;; I'm in the main window
         (eq (window-main-window) window)
         window)

        (;; I'm in a side window
         (ncol--side-window-p window)
         (window-main-window))

        (;; I'm in the a child of the main window
         (and (eq (window-parent window) (window-main-window))
              (ncol--window-v-split-p (window-main-window)))
         window)

        (;; I'm in a normal window: go up
         t
         (ncol--find-cur-main-column (window-parent window)))))


(defun ncol--find-main (window &optional row)
  "Find the main column WINDOW is placed at.
When ROW is non-nil, search for the row."
  (cond (;; I'm in the root window
         (eq (frame-root-window) window)
         (window-main-window))

        (;; I'm in the main window
         (eq (window-main-window) window)
         window)

        (;; I'm in a side window
         (ncol--side-window-p window)
         (window-main-window))

        (;; I'm in the minibuffer
         (window-minibuffer-p window)
         (window-main-window))

        (;; I'm in the main col or row if ROW is non-nil
         (and (eq (window-parent window) (window-main-window))
              (funcall
               (if row #'ncol--window-h-split-p #'ncol--window-v-split-p)
               (window-main-window)))
         window)

        (;; I'm in a normal window: go up
         t
         (ncol--find-cur-main-column (window-parent window)))))


(defun ncol--try-create-window (min-width min-height)
  "Tries to create a window.
It prefers t splitt of a new main column if it would get atleast MIN-WIDTH.
If that fails, it tries to split the current main column horizontally, provided
that the new window gets atleast MIN-HEIGHT.
Else it assumes that the frame is in \"portrait mode\" and splits off a row.
If that fails it gives up and returns nil."

  (cl-labels
      (;; helper functions
       (try-create-main-col (current-window)
         (when (ncol--window-v-splittable-p (window-main-window) min-width)

           (cond (;; if we're in the left side window, split a new left column
                  (ncol--side-window-p current-window 'left)
                  (split-window
                   (window-child (window-main-window)) t 'left))

                 (;; if we're in any other side window, split a right column
                  (ncol--side-window-p current-window)
                  (split-window
                   (window-last-child (window-main-window)) t 'right))

                 (;; else split this column to the right
                  t
                  (split-window
                   (ncol--find-main current-window) t 'right)))))

       (try-create-col-split (current-window)
         (when (and (ncol--window-v-split-p (window-main-window))
                    (not (ncol--side-window-p current-window))
                    (ncol--window-h-splittable-p
                     (ncol--find-main current-window) min-height))
           (split-window current-window t 'below)))

       (try-col-split-from-side-window (current-window)
         (when (and (ncol--window-v-split-p (window-main-window))
                    (not (ncol--window-v-splittable-p (window-main-window) min-width)))
           (cond ((ncol--side-window-p current-window 'left)
                  (try-create-col-split (window-in-direction 'right)))

                 ((ncol--side-window-p current-window 'right)
                  (try-create-col-split (window-in-direction 'left))
                  )

                 ((ncol--side-window-p current-window 'top)
                  (try-create-col-split (window-in-direction 'below))
                  )

                 ((ncol--side-window-p current-window 'bottom)
                  (try-create-col-split (window-in-direction 'above))))))

       (try-create-main-row (current-window)
         (when (and (ncol--window-h-splittable-p (window-main-window) min-height)
                    (not (ncol--window-v-split-p (window-main-window))))

           (cond ((ncol--side-window-p current-window 'top)
                  (split-window
                   ;; window
                   (if (ncol--window-h-split-p (window-main-window))
                       (window-top-child (window-main-window))
                     (window-main-window))
                   t 'top))

                 ((ncol--side-window-p current-window 'bottom)
                  (split-window
                   ;; window
                   (if (ncol--window-h-split-p (window-main-window))
                       (window-last-child (window-main-window))
                     (window-main-window))
                   t 'bottom))

                 (;; we're currently in a left or right side window
                  (ncol--side-window-p current-window)
                  (split-window
                   ;; window
                   (if (ncol--window-h-split-p (window-main-window))
                       (window-last-child (window-main-window))
                     (window-main-window))
                   t 'bottom)
                  )

                 (;; we are not in a side window: make a new row below
                  t
                  (split-window (ncol--find-main current-window t) t 'below))))))

    (or
     ;; try to split off a new main column
     (try-create-main-col (selected-window))

     ;; next: try to split spit the current main column down
     (try-create-col-split (selected-window))

     ;; next: handle case when we're in a side window
     (try-col-split-from-side-window (selected-window))

     ;; else assume row mode and try to split off rows
     (try-create-main-row (selected-window))
     )))

(defun ncol-display-buffer (buffer alist)
  "Find the top-most window split and attempt to display BUFFER inside it.
If the split is a single window or row-based and a new column to the right
would exceed `min-width' in ALIST, BUFFER will be displayed in a new column
to the right, else in a new row below.

If the split is columnar, a new column is displayed to the right, same
as above.  But if the new column with falls below
`min-width', the current column (not the root split!) is
split below."
  (let* ((min-width (alist-get 'min-width alist fill-column))
         (min-height (alist-get 'min-height alist 16))
         (new-window (ncol--try-create-window min-width min-height)))

    (when new-window
      (set-window-buffer new-window buffer)
      (balance-windows (window-main-window))
      (select-window new-window))
    ))

(defun ncol--rebalance ()
  "Rebalance top-level column window group.
The group is a split as defined by `ncol--display-buffer-n-columns'.
Intended for `window-state-change-hook'."
  (balance-windows (window-main-window)))

(defun ncol-display-buffer-replace-ibuffer (buffer _)
  "Display BUFFER in the current window if it displays `ibuffer', replacing it."
  (when (string= (buffer-name (current-buffer)) "*Ibuffer*")
    (let ((window (get-buffer-window (current-buffer))))
      (set-window-buffer window buffer)
      (select-window window))))

(defcustom display-buffer-ncol-action '((display-buffer-reuse-window
                                         ncol-display-buffer-replace-ibuffer
                                         ncol-display-buffer)
                                        . ((reusable-frames . nil)))
  "Defines a list of `display-buffer' actions.
It is structured like `display-buffer-base-action' etc. You can customize how
ncol-mode displays buffers by interleaving your own desired functions."
  :type display-buffer--action-custom-type ;; from window.el
  :risky t
  :group 'ncol-mode)

(defvar ncol-mode-map
  (let ((-map (make-sparse-keymap)))
    (define-key -map (kbd "C-x C-f") #'find-file-other-window)
    (define-key -map (kbd "C-x b") #'pop-to-buffer)
    (define-key -map (kbd "C-x d") (lambda ()
                                     (interactive)
                                     (dired-other-window default-directory)))
    -map)
  "Keymap for `ncol-mode'.")

(define-minor-mode ncol-mode
  "Toggle a special buffer display configuration."
  :global t
  :group 'ncol-mode
  :lighter " ncol"
  (if ncol-mode
      ;; Enabling
      (progn
        (setq ncol-previous-display-buffer-state display-buffer-base-action)
        (setq display-buffer-base-action display-buffer-ncol-action)
        (add-hook 'window-state-change-hook #'ncol--rebalance))
    ;; Disabling
    (progn
      (setq display-buffer-base-action ncol-previous-display-buffer-state)
      (remove-hook 'window-state-change-hook #'ncol--rebalance))))

(provide 'ncol-mode)
;;; ncol-mode.el ends here.
