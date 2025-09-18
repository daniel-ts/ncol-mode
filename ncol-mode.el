;;; ncol-mode.el --- Window management in vertical columns -*- lexical-binding: t -*-

;;; Commentary:
;; ncol-mode: This packages implements display-buffer functions for window
;; management.  The windows are arranged in vertical columns until the
;; column width falls below the minimal width, then each new window splits
;; a column horizontally.

;;; Code:
(require 'cl-lib)
(require 'dash)
(require 'window)

(defgroup ncol-mode nil
  "Window management in vertical columns until minimum width is reached.
Then split horizontally."
  :group 'windows
  :prefix "ncol-")

(defconst ncol--tag-magic-number
  (random)
  "A random integer that serves as the value for the window ncol-tag.")

(defcustom ncol-column-min-width 80
  "The minimal width a column must have after a vertical split.
If a vertical split would make columns narrower than this,
then the split is performed horizontally instead.

This is a user option and can be customized."
  :type 'integer
  :group 'ncol-mode)

(defvar ncol-previous-display-buffer-state nil
  "The previous value of `display-buffer-base-action`.
The value is restored when ncol-mode is deactivated.")

(defun ncol--window-tree ()
  "Wrap `window-tree' function to skip metadata."
  (car (window-tree)))

(defalias 'ncol--descend 'cddr
  "Descend into the split of TREE.")

(defun ncol--window-split-p (tree)
  "Return t if TREE is an actual window tree split.
That is: a list of 4 elements with a boolean car, a listp cadr and
windows or splits in the rest, like:

\(nil (0 0 160 73)
     #<window 3 on *scratch*>
     (t (80 0 160 73) #<window 35 on *Help*> #<window 37 on *Help*>))"
  (cl-labels ((list-or-window-p (obj)
                (or (and (listp obj) obj)
                    (windowp obj))))
    (when (listp tree)
      (let ((2nd (cadr tree))
            (rest (ncol--descend tree)))
        (and (booleanp (car tree))
             (and (listp 2nd)
                  (= (length 2nd) 4)
                  (-all-p #'numberp 2nd))
             (and rest
                  (-all-p #'list-or-window-p rest)))))))

(defun ncol--window-tree-p (tree)
  "Return t if TREE is an actual window tree split.
That is: it's either a window or a window split as defined by
`ncol--window-split-p'"
  (or (windowp tree)
      (ncol--window-split-p tree)))

(defun ncol--find-topmost-split (tree &optional direction)
  "Find the topmost split inside TREE.

The split will be horizontal if DIRECTION \\='horizontal', vertical when
\\='vertical' and any split when \\='any'.

The found split is the one that is managed by this package.
In case there either not HORIZONTAL or vertical splits, return nil."
  (cl-labels ((iter (tree dir)
                (cond (;; Seeing a window without a split
                       (windowp tree)
                       tree)

                      (;; Seeing a split
                       (ncol--window-split-p tree)
                       (if (eq (car tree) dir)
                           tree ;; found split-dir split: return

                         ;; found inverse split: descend to first found split
                         (let ((subtree
                                (-find #'ncol--window-split-p
                                       (ncol--descend tree))))
                           (if (null subtree)
                               ;; There are no further splits and I have only
                               ;; seen splits inverse to split-dir: return
                               nil
                             ;; descend
                             (iter subtree dir)))
                         )))))

    (cond (;; no splits
           (windowp tree)
           tree)

          (;; not a tree but direction any
           (eq direction 'any)
           tree)

          (t
           (iter tree
                 (cond ((eq direction 'horizontal)
                        t)

                       ((eq direction 'vertical)
                        nil)

                       (t (error "%s is not a valid direction"
                                 (symbol-name direction)))))))))

;; (defun ncol--find-first-window (tree)
;;   "Find the first window in a (sub) TREE."
;;   (cond ((windowp tree)
;;          tree)
;;         ((ncol--window-split-p tree)
;;          (-find #'windowp (ncol--descend tree)))))

(defun ncol--find-first-window (tree)
  "Find the first window in a (sub) TREE via depth-first search.
Each split contains a window."
  (if (windowp tree)
      tree
    (ncol--find-first-window (car (ncol--descend tree)))))

;; (defun ncol--find-last-window (tree)
;;   "Find the last window in a (sub) TREE."
;;   (cond ((windowp tree)
;;          tree)
;;         ((ncol--window-split-p tree)
;;          (-find (lambda (node) (windowp node)) (nreverse (ncol--descend tree))))))

(defun ncol--size-of-split (tree)
  "Count the splits in a (sub) TREE."
  (cond ((windowp tree)
         1)
        ((ncol--window-split-p tree)
         (length (ncol--descend tree)))))

(defun ncol--window-ancestor (window n)
  "Find the ancestor of WINDOW of degree N.
0 is the window itself, 1 is the parent, 2 the grandparent and so on."
  (if (= n 0)
      window
    (ncol--window-ancestor (window-parent window) (1-  n))))

(defun ncol--split-descend-iter (window-or-split depth)
  "Descend into a WINDOW-OR-SPLIT until a leaf is encountered.
Counting the DEPTH, return the ancestor of the window of degree DEPTH."
  (cond (;; is a window, unwind
         (windowp window-or-split)
         (ncol--window-ancestor window-or-split depth))

        (;; is a split: descend
         (ncol--window-split-p window-or-split)
         (ncol--split-descend-iter (caddr window-or-split) (1+ depth)))

        (;; error
         t
         (error "Enountered an object of unexpected type"))))

(defun ncol--windows-of-split (split)
  "Convert a SPLIT into a list of windows.
A subsplit is converted to the parent of the first child."
  (cond (;; is either a window
         (windowp split)
         (list split))
        (;; or a split of windows
         (ncol--window-split-p split)
         (-map (lambda (item)
                 (if (window-live-p item)
                     item
                   (ncol--split-descend-iter item 0)))
               (ncol--descend split)))
        (;; everything else is an error
         t (error
            "Enountered an object of unexpected type: %s"
            (type-of split)))))

(defun ncol--window-descendent-p (window parent)
  "Check if WINDOW is descendent of PARENT.
A window is a descendent of itself.  The windows don't have to be live."
  (cond ((null window)
         nil)

        ((eq window parent)
         t)

        (;; recur
         t
         (ncol--window-descendent-p (window-parent window) parent))))

(defun window-of-split (split)
  "Return the split window of SPLIT."
  (cl-labels (;; walk the split to find a window and walk up its descendents
              (iter (cur rest depth)
                (if (windowp cur)
                    (ncol--window-ancestor cur depth)
                  (let ((-rest (nconc rest (ncol--descend cur))))
                    (iter (car -rest) (cdr -rest) (1+ depth))))))
    (if (not (ncol--window-split-p split))
        (error "Bad object received by window-of-split: %s" (type-of split))
      (let ((rest (ncol--descend split)))
        (iter (car rest) (cdr rest) 0)))))

(defun ncol--find-managed-root ()
  "Find the root window that is managed by ncol-mode and return it.
This window is tagged with \\='ncol-root'.  This window is a live window
only in the special case when it is the only window of the frame.  If no
window is found, nil is returned."

  (cl-labels (;; helper functions
              (has-tag (window)
                (= ncol--tag-magic-number
                   (window-parameter window 'ncol-tag)))
              (side-window-p (w)
                (window-parameter w 'window-side))
              (search (cur trees)
                (cond (;; currently at side window
                       (side-window-p cur)
                       (if trees
                           (iter (car trees) (cdr trees))
                         ;; what do do?
                         nil))

                      (;; currently at live window
                       (windowp cur)
                       (cond (;; it has a tag: return it
                              (has-tag cur)
                              cur)

                             (;; no tag but still items to check
                              (consp trees)
                              (iter (car trees) (cdr trees)))

                             (;; no tag, no more items to check
                              ;; should this even be possible?
                              t nil)))

                      (;; currently at split
                       (ncol--window-split-p cur)
                       (let ((w (window-of-split cur))
                             (remaining (nconc trees (ncol--descend cur))))
                         (if (has-tag w)
                             w
                           (iter (car remaining) (cdr remaining)))))

                      t ;; this should not happen
                      (error "Bad object encountered during search: %s"
                             (type-of cur)))))
    (search (ncol--window-tree) nil)))

(defun ncol--rebalance-n-columns ()
  "Rebalance top-level column window group.
The group is a split as defined by `ncol--display-buffer-n-columns'.
Intended for `window-state-change-hook'."
  (when (ncol--window-split-p (ncol--window-tree))
    (balance-windows
     (window-parent
      (ncol--find-first-window
       (ncol--window-tree))))))

(defun ncol-display-buffer-n-columns (buffer _)
  "Split the current column to the right and display BUFFER.
If the new column would have width exceeding `ncol-column-min-width'.
This function conforms to `display-buffer'."
  (let* ((topmost (ncol--find-topmost-split
                   (ncol--window-tree) 'vertical))
         (top-columns
          (if topmost
              (ncol--windows-of-split topmost)
            nil))
         (column-count
          (if topmost
              ;; there is a topmost vertical split: count the columns
              (length top-columns)
            ;; there is no topmost vertical split: there is only one column
            1))
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

(defun ncol-display-buffer-in-rows (buffer _)
  "Split the current row below and display BUFFER.
But only if the topmost split is horizontal.
This function conforms to `display-buffer'."
  (let* ((topmost (ncol--find-topmost-split (ncol--window-tree) 'horizontal))
         (top-rows
          (if topmost
              (ncol--windows-of-split topmost)
            nil)))

    (when (and (ncol--window-split-p topmost) ;; not a single window
               (car topmost)) ;; t means horizontal
      ;; split only if the topmost split is horizontal
      (let* ((row-to-split
              ;; find the row in top-rows the current window is in
              (-find (lambda (row)
                       (ncol--window-descendent-p (selected-window) row))
                      top-rows))
             (new-window (split-window row-to-split nil 'below)))

        (set-window-buffer new-window buffer)
        (balance-windows (window-parent new-window))
        (select-window new-window)))))

(defun ncol-display-buffer-split-below (buffer _)
  "Split the current column horizontally and display BUFFER.
This function conforms to `display-buffer'."
  (let ((new-window (split-window (selected-window) nil 'below)))
    (add-hook 'window-state-change-hook #'ncol--rebalance-n-columns)
    (set-window-buffer new-window buffer)
    (balance-windows (window-parent new-window))
    (select-window new-window)))

(defun ncol-display-buffer-replace-ibuffer (buffer _)
  "Display BUFFER in the current window if it displays `ibuffer', replacing it."
  (when (string= (buffer-name (current-buffer)) "*Ibuffer*")
    (let ((window (get-buffer-window (current-buffer))))
      (set-window-buffer window buffer)
      (select-window window))))

(defcustom display-buffer-ncol-action '((display-buffer-reuse-window
                                         ncol-display-buffer-replace-ibuffer
                                         ncol-display-buffer-n-columns
                                         ncol-display-buffer-in-rows
                                         ncol-display-buffer-split-below)
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
        (add-hook 'window-state-change-hook #'ncol--rebalance-n-columns))
    ;; Disabling
    (progn
      (setq display-buffer-base-action ncol-previous-display-buffer-state)
      (remove-hook 'window-state-change-hook #'ncol--rebalance-n-columns))))

(provide 'ncol-mode)
;;; ncol-mode.el ends here
