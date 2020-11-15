(use-package treemacs
  :commands
  (treemacs treemacs-select-window)
  :bind
  ("<f5>" . 'treemacs-select-window)
  (:map treemacs-mode-map
        ("<f5>" . 'treemacs))
  :custom
  (treemacs-no-png-images t))

(provide 'init-sidebar)
