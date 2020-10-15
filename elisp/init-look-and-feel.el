;;; -*- lexical-binding: t -*-

(add-to-list 'load-path (expand-file-name "themes" user-emacs-directory))

(require 'joker-theme)
(require 'storybook-theme)

(defvar +current-theme nil
  "Name for current theme")

(defvar +theme-list
  '(storybook joker))

(defun +change-theme ()
  (interactive)
  (let ((theme (car +theme-list)))
	(disable-theme theme)
	(setq +theme-list (append (cdr +theme-list) (list theme)))
	(load-theme (car +theme-list) t)))

(+change-theme)

(global-set-key (kbd "C-x C-\\") '+change-theme)

;; Fonts

(let ((font "Victor Mono-11"))
  (set-frame-font font)
  (add-to-list 'default-frame-alist (cons 'font font)))

;; Mode Line

(setq-default frame-title-format '("Emacs" (:eval (+project-name))))

(setq-default mode-line-format '((:eval (meow-minimal-indicator))
                                 "%l:%C "
                                 (:eval (when (bound-and-true-p rime-mode) (concat (rime-lighter) " ")))
                                 (:eval (+smart-file-name))
                                 "%* %m "
                                 (vc-mode vc-mode)
                                 ""))

(provide 'init-look-and-feel)
