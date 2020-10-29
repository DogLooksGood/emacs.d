(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  :bind
  (:map dashboard-mode-map
        ("n" . 'dashboard-next-line)
        ("p" . 'dashboard-previous-line)))

(provide 'init-dashboard)
