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

(defvar ncol-previous-display-buffer-state nil
  "The previous value of `display-buffer-base-action`.
The value is restored when ncol-mode is deactivated.")

(defun ncol--window-tree ()
  "Wrap `window-tree' function to skip metadata."
  (car (window-tree)))

(defalias 'ncol--children-of-split 'cddr
  "Descend into the split of TREE.")

(defun ncol--side-window-p (w)
  "Return p if W is a side-window, nil otherwise."
  (not (null (window-parameter w 'window-side))))

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
            (rest (ncol--children-of-split tree)))
        (and (booleanp (car tree))
             (and (listp 2nd)
                  (= (length 2nd) 4)
                  (-all-p #'numberp 2nd))
             (and rest
                  (-all-p #'list-or-window-p rest)))))))

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
               (ncol--children-of-split split)))
        (;; everything else is an error
         t (error
            "Enountered an object of unexpected type: %s"
            (type-of split)))))

(defun ncol--window-of-split (split)
  "Return the split window of SPLIT."
  (cl-labels (;; walk the split to find a window and walk up its descendents
              (iter (cur rest depth)
                (if (windowp cur)
                    (ncol--window-ancestor cur depth)
                  (let ((-rest (nconc rest (ncol--children-of-split cur))))
                    (iter (car -rest) (cdr -rest) (1+ depth))))))
    (if (windowp split)
        split
      (let ((rest (ncol--children-of-split split)))
        (iter (car rest) (cdr rest) 1)))))

(defun ncol--find-topmost-split (tree)
  "Find the top-most split or window in TREE that is not a side-window split.
A side-window split contains side windows."
  (if (ncol--side-window-p (car (ncol--windows-of-split tree)))
      ;; proceed with the 2nd child
      (cadr (ncol--children-of-split tree))
    tree))

(defun ncol-display-buffer (buffer alist)
  "Find the top-most window split and attempt to display BUFFER inside it.
If the split is a single window or row-based and a new column to the right
would exceed `min-width' in ALIST, BUFFER will be displayed in a new column
to the right, else in a new row below.

If the split is columnar, a new column is displayed to the right, same
as above.  But if the new column with falls below
`ncol-column-min-width', the current column (not the root split!) is
split below."

  (let* ((min-width (alist-get 'min-width alist 80))
         (min-height (alist-get 'min-height alist 16))
         (root-split (ncol--find-topmost-split (ncol--window-tree)))
         (rootw (ncol--window-of-split root-split))
         (wcount (if (windowp root-split)
                     1
                   (length (ncol--windows-of-split root-split)))))

    (cl-labels
        (;; helper functions
         (split-orient (split)
           "Return the orientation of a split."
           (cond (;; is a window and has no "split" orientation
                  (windowp split) 'n)
                 (;; split horizontal? Evaluates to t
                  (car split) 'h)
                 (;; must be vertical
                  t 'v)))

         (within-dimensions (w h)
           "Check if W and H are exceeding `min-width' and `min-height'."
           (and (>= w min-width)
                (>= h min-height)))

         (sibling-count (w)
           "Count sibling windows of W, itself included."
           (let ((parentw (window-parent w)))
             (cond (;; w is the root: it has only itself as a sibling
                    (null parentw) 1)

                   (;; w is within a vertical split 0|0
                    (window-combined-p w t)
                    (floor (/ (window-total-width parentw)
                              (window-total-width w))))

                   (;; w is within a horizontal split
                    (window-combined-p w nil)
                    (floor (/ (window-total-height parentw)
                              (window-total-height w)))))))

         (find-daddy (window &optional dadw)
           "Go up an ancestor of WINDOW until WINDOW is a child of DADW, which
defaults to rootw."
           (cond ((null (window-parent window)) rootw)
                 ((eq (window-parent window) (or dadw rootw)) window)
                 (t (find-daddy (window-parent window) dadw))))

         (try-column-splitoff (w)
           "Try splitting a new column off rootw. Return the new window or nil."
           (let ((resulting-width (/ (window-total-width rootw) (1+ wcount))))
             (when (within-dimensions resulting-width
                                      (window-total-height rootw))
               (split-window w nil 'right))))

         (try-row-splitoff (w)
           "Try splitting a new row off rootw. Return the new window or nil."
           (let ((resulting-height (/ (window-total-height rootw) (1+ wcount))))
             (when (within-dimensions (window-total-width rootw)
                                      resulting-height)
               (split-window w nil 'below))))

         (try-split-col-horizontal (w dadw)
           "Try splitting a column below."
           (if (eq w dadw)
               ;; w and dadw equal: the column has no splits yet
               (when (within-dimensions (window-total-width w)
                                        (/ (window-total-height w) 2))
                 (split-window w nil 'below))
             ;; split below the current row of the column
             (let* ((window (find-daddy w dadw))
                    (wcount (sibling-count window)))
               (when (within-dimensions (window-total-width dadw)
                                        (/ (window-total-height dadw)
                                           wcount))
                 (split-window w nil 'below)))))

         (compute-new-window ()
           "Try splitting off a column.  If that does not work, try splitting off
either a row below (when rootw is row-based, else try splitting of a new
row on the current).  Might return nil."
           (let* ((orient (split-orient root-split)))
             (cond ((eq orient 'n)
                    (or (try-column-splitoff rootw)
                        (try-row-splitoff rootw)))

                   ((eq orient 'v)
                    (or (try-column-splitoff (find-daddy (selected-window)))
                        (try-split-col-horizontal
                         (selected-window)
                         (find-daddy (selected-window)))))

                   ((eq orient 'h)
                    (or (try-column-splitoff rootw)
                        (try-row-splitoff (find-daddy (selected-window)))))
                   )))

         (display (window)
           "Display the buffer in WINDOW.  The buffer is closed over."
           (set-window-buffer window buffer)
           (balance-windows rootw)
           (select-window window)))

      ;; try computing a new window and in case of succes, display it
      (let ((new-window (compute-new-window)))
        (when new-window
          (display new-window)))
      )))

(defun ncol--rebalance ()
  "Rebalance top-level column window group.
The group is a split as defined by `ncol--display-buffer-n-columns'.
Intended for `window-state-change-hook'."
  (balance-windows
     (ncol--window-of-split
      (ncol--find-topmost-split
       (ncol--window-tree)))))

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
;;; ncol-mode.el ends here
