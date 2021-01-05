;;; -*- lexical-binding: t -*-

(use-package magit
  :commands (magit))

(use-package diff-hl
  :hook
  ((dired-mode . diff-hl-dired-mode)
   (prog-mode . diff-hl-mode)
   (conf-mode . diff-hl-mode)))

(use-package dired-git-info
  :bind
  (:map dired-mode-map
        ("v" . dired-git-info-mode))
  :custom
  (dgi-auto-hide-details-p nil))

(provide 'init-git)
