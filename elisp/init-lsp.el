;;; -*- lexical-binding: t -*-

(use-package popup)

(use-package nox
  :hook (((rust-mode elixir-mode js-mode) . nox-ensure))
  :straight
  (nox :type git
       :host github
       :repo "DogLooksGood/nox")
  :custom
  (nox-optimization-p nil)
  (nox-doc-major-mode 'markdown-mode)
  (nox-server-programs
   '((elixir-mode . ("/home/tianshu/source/elixir-ls/release/language_server.sh"))
     ((js-mode typescript-mode) . ("typescript-language-server" "--stdio"))))
  :bind
  (:map nox-mode-map
		("M-d" . nox-show-doc)
        ("M-h" . nox-show-signature)))

(defun +nox-try-show-signature (&optional _ignored)
  (when (or (looking-back "(" 1)
            (looking-back ", ?" 2))
    (nox-show-signature)))

(defun +setup-nox ()
  (add-hook 'company-completion-finished-hook '+nox-try-show-signature t t)
  (add-hook 'post-self-insert-hook '+nox-try-show-signature t t))

(add-hook 'nox-managed-mode-hook '+setup-nox)

;; (use-package eglot
;;   :hook
;;   ((rust-mode c-mode elixir-mode) . eglot-ensure)
;;   :custom
;;   (eglot-stay-out-of '())
;;   (eglot-ignored-server-capabilites '(:documentHighlightProvider))
;;   :config
;;   (add-to-list 'eglot-server-programs
;; 			   '(elixir-mode "/home/tianshu/source/elixir-ls/release/language_server.sh"))
;;   (add-to-list 'eglot-server-programs
;; 			   '(rust-mode "rust-analyzer")))


;; (use-package flycheck)
;;
;; (use-package lsp-mode
;;   :hook
;;   ((rust-mode c-mode elixir-mode) . lsp-deferred)
;;   :commands
;;   (lsp lsp-deferred)
;;   :custom
;;   (lsp-completion-provider :capf)
;;   (lsp-enable-file-watchers nil)
;;   (lsp-keymap-prefix "C-l")
;;   (lsp-enable-symbol-highlighting nil)
;;   (lsp-lens-enable nil)
;;   (lsp-headerline-breadcrumb-enable nil)
;;   (lsp-modeline-code-actions-enable nil)
;;   (lsp-signature-auto-activate t)
;;   (lsp-modeline-diagnostics-enable nil)
;;   (lsp-signature-render-documentation nil)
;;   (lsp-completion-show-detail nil)
;;   (lsp-completion-show-kind t)
;;   (lsp-clients-elixir-server-executable "/home/tianshu/source/elixir-ls/release/language_server.sh"))

(provide 'init-lsp)
