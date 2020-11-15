;;; -*- lexical-binding: t -*-

(use-package magit
  :commands (magit))

(use-package diff-hl
  :hook
  ((dired-mode . diff-hl-dired-mode)
   (prog-mode . diff-hl-mode)
   (conf-mode . diff-hl-mode))
  :init
  (diff-hl-margin-mode t))

(provide 'init-git)
