;;; -*- lexical-binding: t -*-

(straight-use-package 'web-mode)
(straight-use-package 'emmet-mode)

;;; web-mode

(setq-default
 js-indent-level 2
 css-indent-offset 2)

(setq
 web-mode-markup-indent-offset 2
 web-mode-code-indent-offset 2
 web-mode-css-indent-offset 2)

(autoload #'web-mode "web-mode")

(add-to-list 'auto-mode-alist '("\\.eex\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.leex\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.sface\\'" . web-mode))

;;; emmet-mode

(autoload #'emmet-mode "emmet-mode")

(add-hook 'web-mode-hook 'emmet-mode)
(add-hook 'html-mode-hook 'emmet-mode)

(with-eval-after-load "emmet-mode"
  (define-key emmet-mode-keymap (kbd "M-e") 'emmet-expand-line))

(provide 'init-web)
