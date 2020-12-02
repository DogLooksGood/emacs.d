;;; -*- lexical-binding: t -*-

(defun +treemacs-scale-font-size ()
  (face-remap-add-relative 'default :height 0.8))

(use-package treemacs
  :commands
  (treemacs treemacs-select-window)
  :bind
  ("<f5>" . 'treemacs-select-window)
  (:map treemacs-mode-map
        ("<f5>" . 'treemacs))
  :custom
  (treemacs-no-png-images t)
  (treemacs-width 40)
  :init
  (add-hook 'treemacs-mode-hook '+treemacs-scale-font-size))

(provide 'init-sidebar)
