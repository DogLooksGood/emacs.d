;;; -*- lexical-binding: t -*-

(straight-use-package 'treemacs)

(defun +treemacs-scale-font-size ()
  (face-remap-add-relative 'default :height 0.8))

(setq
 treemacs-no-png-images t
 treemacs-width 40)

(autoload #'treemacs "treemacs")
(autoload #'treemacs-select-window "treemacs")

(global-set-key (kbd "<f1>") 'treemacs-select-window)

(with-eval-after-load "treemacs"
  (define-key treemacs-mode-map (kbd "<f1>") 'treemacs)
  (add-hook 'treemacs-mode-hook '+treemacs-scale-font-size))

(provide 'init-sidebar)
