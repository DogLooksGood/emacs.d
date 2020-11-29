;;; -*- lexical-binding: t -*-

(use-package writeroom-mode
  :bind
  ("M-RET" . global-writeroom-mode)
  :custom
  (writeroom-width 100)
  (writeroom-major-modes '(text-mode conf-mode prog-mode special-mode fundamental-mode)))

(setq-default display-line-numbers-widen 4)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(provide 'init-visual)
