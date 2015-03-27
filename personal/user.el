;; My configuration

(add-hook 'after-init-hook
    '(lambda ()
       (add-to-list 'default-frame-alist '(height . 50))
        (add-to-list 'default-frame-alist '(width . 120))
        (add-to-list 'default-frame-alist '(font .  "Source Code Pro-14"))))

(setq mac-command-modifier 'meta
      mac-option-modifier 'none
      default-input-method "MacOSX")

(global-visual-line-mode t)

;; whitespace, disable highlighting lines that exceed a certain length limit
(setq whitespace-style '(face tabs empty trailing))

(disable-theme 'zenburn)
(load-theme 'leuven t)
(set-face-attribute 'org-level-1 nil :background nil)
(set-face-attribute 'org-level-1 nil :foreground "dim gray")
(set-face-attribute 'org-level-2 nil :background nil)
(set-face-attribute 'org-level-2 nil :foreground "dodger blue")


;;(load-theme 'material t)

;; show line numbers
(global-linum-mode)

;; # C/C++ configuration BEGIN
;; http://tuhdo.github.io/c-ide.html

;; projectile
(projectile-global-mode)
(setq projectile-completion-system 'helm)

(global-unset-key (kbd "C-M-SPC"))

;; function-args
(fa-config-default)

;; CEDET

(require 'cc-mode)
(require 'semantic)

(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)
(global-semantic-stickyfunc-mode 1)

(semantic-mode 1)

(semantic-add-system-include "~/Projects/DSAudiD5/ClearCaseVOB/AfsDriveSW/ImplementationSet/Include")

;; company mode
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

;;(define-key c-mode-map  [(tab)] 'company-complete)
;;(define-key c++-mode-map  [(tab)] 'company-complete)

;; irony
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

;; replace the `completion-at-point' and `complete-symbol' bindings in
;; irony-mode's buffers by irony-mode's function
(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))
(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))

;; ------ C/C++ configuration END -------

; start yasnippet with emacs
(require 'yasnippet)
(yas-global-mode 1)

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
(toggle-line-spacing)


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
