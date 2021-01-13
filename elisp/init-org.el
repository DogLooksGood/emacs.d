;;; -*- lexical-binding: t -*-

(defvar-local +org-last-in-latex nil)

(defun +org-post-command-hook ()
  (ignore-errors
    (let ((in-latex (rime-predicate-org-latex-mode-p)))
      (if (and +org-last-in-latex (not in-latex))
          (progn (org-latex-preview)
                 (setq +org-last-in-latex nil)))

      (when-let ((ovs (overlays-at (point))))
        (when (->> ovs
                   (--map (overlay-get it 'org-overlay-type))
                   (--filter (equal it 'org-latex-overlay)))
          (org-latex-preview)
          (setq +org-last-in-latex t)))

      (when in-latex
        (setq +org-last-in-latex t)))))

(define-minor-mode org-latex-auto-toggle
  "Auto toggle latex overlay when cursor enter/leave."
  nil
  nil
  nil
  (if org-latex-auto-toggle
      (add-hook 'post-command-hook '+org-post-command-hook nil t)
    (remove-hook 'post-command-hook '+org-post-command-hook t)))

;;; Org babel

(defun +org-redisplay-inline-images ()
  (when org-inline-image-overlays
    (org-redisplay-inline-images)))

(defun +org-babel-setup ()
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)))
  (setq org-confirm-babel-evaluate nil)
  (add-hook 'org-babel-after-execute-hook '+org-redisplay-inline-images))

(use-package org
  :straight (:type built-in)
  :bind
  (:map org-mode-map
        ("<f8>" . org-latex-auto-toggle))
  :config
  (require 'org-tempo)
  (+org-babel-setup))

;;; Update latex options after change theme.

(defun +org-update-latex-option-by-theme (theme)
  (when (bound-and-true-p org-format-latex-options)
    (setq org-format-latex-options
          (plist-put org-format-latex-options :theme theme))))

(add-hook '+after-change-theme-hook '+org-update-latex-option-by-theme)

;;; Org Roam

(use-package org-roam
  :hook
  (after-init . org-roam-mode)
  :config
  (setq org-format-latex-options
        (plist-put org-format-latex-options :scale 4.0))
  :custom
  (org-roam-directory
   (let ((p (expand-file-name "~/Org")))
     (unless (file-directory-p p) (make-directory p))
     p))
  (org-roam-capture-templates
   '(("d" "default" plain (function org-roam--capture-get-point)
      "%?"
      :file-name "%<%Y%m%d%H%M%S>-${slug}"
      :head "#+title: ${title}\n"
      :unnarrowed t)))
  :bind
  (:map org-roam-mode-map
        ("C-x C-r l" . org-roam)
        ("C-x C-r f" . org-roam-find-file)
        ("C-x C-r g" . org-roam-graph))
  (:map org-mode-map
        ("<f7>" . org-roam-insert)
        ("C-x C-r i" . org-roam-insert)
        ("C-x C-r I" . org-roam-insert-immediate))
  :config
  ;; https://www.orgroam.com/manual.html#Roam-Protocol
  (require 'org-roam-protocol))

;;; install latex with
;;; pacman -S texlive-bin texlive-most
;;; install xdot
;;; pacman -S xdot

(use-package org-superstar
  :hook (org-mode . org-superstar-mode))


(provide 'init-org)
