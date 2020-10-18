;;; -*- lexical-binding: t -*-

(use-package paredit
  :bind
  (:map paredit-mode-map
		(";" . 'paredit-semicolon))
  :hook
  (emacs-lisp-mode . paredit-mode))

(provide 'init-lisp)
