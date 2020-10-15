;;; -*- lexical-binding: t -*-

(add-to-list 'load-path (expand-file-name "themes" user-emacs-directory))
(require 'joker-theme)
(require 'storybook-theme)

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


(load-theme 'storybook t)

(provide 'init-look-and-feel)
