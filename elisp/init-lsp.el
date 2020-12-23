;; -*- lexical-binding: t -*-

(use-package flymake
  :commands (flymake-mode)
  :bind
  (:map flymake-mode-map
        ("M-n" . 'flymake-goto-next-error)
        ("M-p" . 'flymake-goto-prev-error)))

(use-package eglot
  :commands (eglot-ensure eglot)
  :hook
  ((rust-mode c-mode elixir-mode python-mode) . eglot-ensure)
  :custom
  (eglot-stay-out-of '())
  (eglot-ignored-server-capabilites '(:documentHighlightProvider))
  :config
  (add-to-list 'eglot-server-programs
			   '(elixir-mode "/home/tianshu/source/elixir-ls/release/language_server.sh"))
  (add-to-list 'eglot-server-programs
			   '(rust-mode "rust-analyzer")))

(provide 'init-lsp)
