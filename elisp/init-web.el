;;; -*- lexical-binding: t -*-

(use-package web-mode
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-css-indent-offset 2))

(setq-default
 js-indent-level 2
 css-indent-offset 2)

(use-package emmet-mode
  :hook
  (web-mode . emmet-mode)
  (html-mode . emmet-mode)
  :bind
  ((:map emmet-mode-keymap
         ("M-e" . 'emmet-expand-line))))

(provide 'init-web)
