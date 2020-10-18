(use-package writeroom-mode
  :bind
  ("M-RET" . global-writeroom-mode)
  :custom
  (writeroom-width 100)
  (writeroom-major-modes '(text-mode conf-mode prog-mode special-mode fundamental-mode)))

(provide 'init-visual)
