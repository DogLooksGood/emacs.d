;;; -*- lexical-binding: t -*-

(use-package nox
  :straight
  (nox :type git
       :host github
       :repo "DogLooksGood/nox")
  :custom
  (nox-optimization-p nil)
  (nox-server-programs
   '((elixir-mode . ("/home/tianshu/source/elixir-ls/release/language_server.sh"))))
  :bind
  (:map nox-mode-map
		("M-h" . nox-show-doc)))

;; (use-package eglot
;;   :hook
;;   ((rust-mode c-mode elixir-mode) . eglot-ensure)
;;   :custom
;;   (eglot-stay-out-of '(flymake))
;;   (eglot-ignored-server-capabilites '(:documentHighlightProvider))
;;   :config
;;   (add-to-list 'eglot-server-programs
;; 			   '(elixir-mode "/home/tianshu/source/elixir-ls/release/language_server.sh"))
;;   (add-to-list 'eglot-server-programs
;; 			   '(rust-mode "rust-analyzer")))

(provide 'init-lsp)
