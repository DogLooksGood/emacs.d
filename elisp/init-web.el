
(use-package web-mode
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-css-indent-offset 2))

(setq-default
 js-indent-level 2
 css-indent-offset 2)

(use-package emmet-mode
  :hook
  (web-mode . emmet-mode)
  (html-mode . emmet-mode)
  :bind
  ((:map emmet-mode-keymap
         ("M-e" . 'emmet-expand-line))))

(use-package mmm-mode)

(mmm-add-classes
 '((elixir-web
    :submode web-mode
    :front "~H\"\"\"\n *"
    :back "\"\"\"")))

(setq mmm-global-mode 'maybe)

(mmm-add-mode-ext-class
 'elixir-mode "\\.ex\\'" 'elixir-web)

(provide 'init-web)
