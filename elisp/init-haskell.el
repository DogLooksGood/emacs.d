;; -*- lexical-binding: t; -*-

(straight-use-package 'haskell-mode)
(straight-use-package 'dante)

;;; haskell-mode

(with-eval-after-load "haskell-mode"
  (add-hook 'haskell-mode-hook 'dante-mode)
  (add-hook 'haskell-mode-hook 'electric-pair-local-mode))

;;; dante

(autoload #'dante-mode "dante")

(provide 'init-haskell)
