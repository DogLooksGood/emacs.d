;; -*- lexical-binding: t; -*-

(setq
 markdown-fontify-code-blocks-natively t)

(straight-use-package 'markdown-mode)

(with-eval-after-load "markdown-mode"
  (set-face-attribute 'markdown-table-face nil :family +mono-ufont-family)
  (set-face-attribute 'markdown-code-face nil :family +fixed-pitch-family)

  (add-hook 'markdown-mode-hook #'markdown-toggle-markup-hiding)
  (add-hook 'markdown-mode-hook #'variable-pitch-mode))

(provide 'init-markdown)
