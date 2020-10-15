(defface +elixir-dim-face
  '((((class color) (background dark))
     (:foreground "grey60"))
    (((class color) (background light))
     (:foreground "grey40")))
  "Elixir dim face.")

(defun +elixir-post-self-insert-hook-setup ()
  (add-hook 'post-self-insert-hook '+elixir-handle-input nil t))

(use-package inf-iex
  :hook
  (elixir-mode . inf-iex-minor-mode)
  :straight
  (inf-iex :type git
	   :host github
	   :repo "DogLooksgood/inf-iex"))

(use-package elixir-mode
  :hook (elixir-mode . nox-ensure)
  :bind
  (:map elixir-mode-map
        ("C-c C-f" . 'elixir-format))
  :config
  (font-lock-add-keywords 'elixir-mode
                          '(("\\([_a-zA-Z0-9!?]+\\):" 1 'default)
                            (":[_a-zA-Z0-9\"!?]+" . font-lock-constant-face)
                            ("defmacro \\([a-zA-Z0-9!?_]+\\)" 1 font-lock-function-name-face)
                            ("\\_<@[_a-zA-Z0-9!?]+\\_>" . 'default)
                            ("\\_<true\\_>" . font-lock-constant-face)
                            ("\\_<false\\_>" . font-lock-constant-face)
                            ("\\_<nil\\_>" . font-lock-constant-face)
                            ("\\_<_[a-zA-Z0-9]*\\_>" . '+elixir-dim-face)))
  (modify-syntax-entry ?& "'" elixir-mode-syntax-table)
  (add-hook 'elixir-mode-hook '+elixir-post-self-insert-hook-setup))

(provide 'init-elixir)
