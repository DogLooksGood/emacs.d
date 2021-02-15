;;; -*- lexical-binding: t -*-

(straight-use-package 'ace-window)

;;; ace-window

(setq
 aw-keys '(?a ?o ?e ?u ?i)
 aw-scope 'frame)

(autoload #'ace-window "ace-window")
(autoload #'ace-swap-window "ace-window")

(windmove-default-keybindings 'meta)

(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

(provide 'init-window)
