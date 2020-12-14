;;; -*- lexical-binding: t -*-

(use-package ace-window
  :commands
  (ace-swap-window ace-window)
  :custom
  (aw-keys '(?a ?o ?e ?u ?i)))

(windmove-default-keybindings 'meta)

(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

(provide 'init-window)
