;; -*- lexical-binding: t; -*-

(custom-set-variables
 '(markdown-fontify-code-blocks-natively t))

(straight-use-package 'markdown-mode)

(with-eval-after-load "markdown-mode"
  (add-hook 'markdown-mode-hook 'markdown-toggle-markup-hiding))

(provide 'init-markdown)
