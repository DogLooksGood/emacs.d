(use-package iedit
  :commands (iedit-mode)
  :bind
  (:map iedit-mode-occurrence-keymap
        ("M-d" . 'iedit-restrict-function)
        ("M-l" . 'iedit-restrict-current-line)))

(provide 'init-edit)
