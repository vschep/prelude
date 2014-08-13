(add-hook 'before-make-frame-hook
    #'(lambda ()
        (add-to-list 'default-frame-alist '(left . 0))
        (add-to-list 'default-frame-alist '(top . 0))
        (add-to-list 'default-frame-alist '(height . 70))
        (add-to-list 'default-frame-alist '(width . 120))
        (add-to-list 'default-frame-alist '(font .  "Monaco-13"))))

(setq mac-command-modifier 'meta
      mac-option-modifier 'none
      default-input-method "MacOSX")

(setq whitespace-line-column 120)


;; functions

(defun toggle-line-spacing ()
  "Toggle line spacing between no extra space to extra half line height."
  (interactive)
  (if (eq line-spacing nil)
      (setq-default line-spacing 2) ; add 0.5 height between lines
    (setq-default line-spacing nil)   ; no extra heigh between lines
    )
  (redraw-display))

(global-set-key (kbd "<f7>") 'toggle-line-spacing)

;; Source: http://www.emacswiki.org/emacs/CopyingWholeLines
(defun copy-line (arg)
    "Copy lines (as many as prefix argument) in the kill ring.
      Ease of use features:
      - Move to start of next line.
      - Appends the copy on sequential calls.
      - Use newline as last char even on the last line of the buffer.
      - If region is active, copy its lines."
    (interactive "p")
    (let ((beg (line-beginning-position))
          (end (line-end-position arg)))
      (when mark-active
        (if (> (point) (mark))
            (setq beg (save-excursion (goto-char (mark)) (line-beginning-position)))
          (setq end (save-excursion (goto-char (mark)) (line-end-position)))))
      (if (eq last-command 'copy-line)
          (kill-append (buffer-substring beg end) (< end beg))
        (kill-ring-save beg end)))
    (kill-append "\n" nil)
    (beginning-of-line (or (and arg (1+ arg)) 2))
    (if (and arg (not (= 1 arg))) (message "%d lines copied" arg)))

(global-set-key (kbd "<f8>") 'copy-line)

;; source: https://groups.google.com/forum/#!msg/gnu.emacs.help/dd2R_UV0LVQ/F06ihLb7hKcJ
(defun move-text-internal (arg)
   (cond
    ((and mark-active transient-mark-mode)
     (if (> (point) (mark))
            (exchange-point-and-mark))
     (let ((column (current-column))
              (text (delete-and-extract-region (point) (mark))))
       (forward-line arg)
       (move-to-column column t)
       (set-mark (point))
       (insert text)
       (exchange-point-and-mark)
       (setq deactivate-mark nil)))
    (t
     (beginning-of-line)
     (when (or (> arg 0) (not (bobp)))
       (forward-line)
       (when (or (< arg 0) (not (eobp)))
            (transpose-lines arg))
       (forward-line -1)))))

(defun move-text-down (arg)
   "Move region (transient-mark-mode active) or current line
  arg lines down."
   (interactive "*p")
   (move-text-internal arg))

(defun move-text-up (arg)
   "Move region (transient-mark-mode active) or current line
  arg lines up."
   (interactive "*p")
   (move-text-internal (- arg)))

(global-set-key [\M-\S-up] 'move-text-up)
(global-set-key [\M-\S-down] 'move-text-down)
