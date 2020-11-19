;;; -*- lexical-binding: t -*-

(use-package ace-window
  :commands
  (ace-swap-window ace-window)
  :custom
  (aw-keys '(?d ?h ?t ?n ?s)))

(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

(provide 'init-window)
