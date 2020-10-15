;;; -*- lexical-binding: t -*-

(use-package nox
  :straight
  (nox :type git
       :host github
       :repo "manateelazycat/nox")
  :custom
  (nox-server-programs
   '((elixir-mode . ("/home/tianshu/source/elixir-ls/release/language_server.sh"))))
  :bind
  (:map nox-mode-map
		("M-h" . nox-show-doc)))

(provide 'init-lsp)
