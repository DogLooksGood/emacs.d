;;; -*- lexical-binding: t -*-

(add-to-list 'load-path (expand-file-name "themes" user-emacs-directory))

(require 'joker-theme)
(require 'storybook-theme)
(require 'printed-theme)

(defvar +current-theme nil
  "Name for current theme")

(defvar +theme-list
  '(joker storybook printed))

(set-display-table-slot standard-display-table
                        'vertical-border
                        (make-glyph-code ?┃))

(defun +change-theme ()
  (interactive)
  (let ((theme (car +theme-list)))
	(disable-theme theme)
	(setq +theme-list (append (cdr +theme-list) (list theme)))
    (load-theme (car +theme-list) t)))

(defun +reload-theme ()
  (interactive)
  (load-theme (car +theme-list) t))

(+change-theme)

(global-set-key (kbd "C-x ~") '+change-theme)

;; Fonts

(let ((font "Victor Mono-11"))
  (set-frame-font font)
  (add-to-list 'default-frame-alist (cons 'font font)))

;; Mode Line

(setq-default frame-title-format '("Emacs" (:eval (+project-name))))

(defun +simple-mode-line-render (left right)
  "Return a string of `window-width' length containing LEFT, and RIGHT
 aligned respectively."
  (let* ((available-width (- (window-width) (length left) 2)))
    (format (format " %%s %%%ds " available-width) left right)))

(setq-default mode-line-format
              '((:eval
                 (+simple-mode-line-render
                  ;; left
                  (format-mode-line
                   '((:eval (when (featurep 'meow) (meow-minimal-indicator)))
                     " %l:%C "
                     (:eval (when (bound-and-true-p rime-mode) (concat (rime-lighter) " ")))
                     (:eval (when (bound-and-true-p flycheck-mode) flycheck-mode-line))))
                  ;; right
                  (format-mode-line
                   '((:eval (when (functionp #'+smart-file-name) (+smart-file-name)))
                     "%* %m"
                     (vc-mode vc-mode)
                     " "))))))


(provide 'init-look-and-feel)
