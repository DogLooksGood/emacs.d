;;; -*- lexical-binding: t -*-

;;; Latex support
;;; install latex with
;;; pacman -S texlive-bin texlive-most
;;; install xdot
;;; pacman -S xdot

(defvar-local +org-last-in-latex nil)

(defun +org-post-command-hook ()
  (ignore-errors
    (let ((in-latex (and (derived-mode-p  'org-mode)
                         (or (org-inside-LaTeX-fragment-p)
                             (org-inside-latex-macro-p)))))
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

;;; Update latex options after change theme.

(defun +org-update-latex-option-by-theme (theme)
  (when (bound-and-true-p org-format-latex-options)
    (setq org-format-latex-options
          (plist-put org-format-latex-options :theme theme))))

(+org-update-latex-option-by-theme (car +theme-list))
(add-hook '+after-change-theme-hook '+org-update-latex-option-by-theme)

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
  (+org-babel-setup)
  :custom
  (org-html-checkbox-type 'unicode))

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
        ("C-x C-r g" . org-roam-graph)
        ("C-x C-r c" . org-roam-db-build-cache))
  (:map org-mode-map
        ("<f7>" . org-roam-insert)
        ("C-x C-r i" . org-roam-insert)
        ("C-x C-r I" . org-roam-insert-immediate))
  :config
  ;; https://www.orgroam.com/manual.html#Roam-Protocol
  (require 'org-roam-protocol))

(use-package org-roam-server
  :ensure t
  :config
  (setq org-roam-server-host "0.0.0.0"
        org-roam-server-port 8080
        org-roam-server-authenticate nil
        org-roam-server-export-inline-images t
        org-roam-server-serve-files t
        org-roam-server-served-file-extensions '("pdf" "mp4" "ogv")
        org-roam-server-network-poll t
        org-roam-server-network-arrows nil
        org-roam-server-network-label-truncate t
        org-roam-server-network-label-truncate-length 60
        org-roam-server-network-label-wrap-length 20))

(use-package org-superstar
  :hook (org-mode . org-superstar-mode))

(use-package htmlize
  :custom
  (htmlize-face-overrides '(clojure-keyword-face (:foreground "var(--clr-constant)" :background "var(--bg-constant)"))))

(use-package org-html-themify
  :straight
  (org-html-themify
   :type git
   :host github
   :repo "DogLooksGood/org-html-themify"
   :files ("*.el" "*.js" "*.css"))
  :hook (org-mode . org-html-themify-mode)
  :custom
  (org-html-themify-themes
   '((dark . joker)
     (light . storybook))))

(provide 'init-org)
