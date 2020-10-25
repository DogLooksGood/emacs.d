(use-package markdown-mode
  :custom
  (markdown-fontify-code-blocks-natively t)
  :init
  (add-hook 'markdown-mode-hook 'markdown-toggle-markup-hiding))

(provide 'init-markdown)
