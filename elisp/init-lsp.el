;; -*- lexical-binding: t -*-

(use-package popup)

(use-package flymake
  :bind
  (:map flymake-mode-map
        ("M-n" . 'flymake-goto-next-error)
        ("M-p" . 'flymake-goto-prev-error)))

(use-package eglot
  :hook
  ((rust-mode c-mode elixir-mode) . eglot-ensure)
  :custom
  (eglot-stay-out-of '())
  (eglot-ignored-server-capabilites '(:documentHighlightProvider))
  :config
  (add-to-list 'eglot-server-programs
			   '(elixir-mode "/home/tianshu/source/elixir-ls/release/language_server.sh"))
  (add-to-list 'eglot-server-programs
			   '(rust-mode "rust-analyzer")))

(provide 'init-lsp)
