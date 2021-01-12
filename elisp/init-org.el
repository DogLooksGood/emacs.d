;;; -*- lexical-binding: t -*-

(use-package org
  :straight (:type built-in))

(defun +org-post-command-hook ()
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
      (setq +org-last-in-latex t))))

(defun +org-latex-auto-toggle-setup ()
  (add-hook 'post-command-hook '+org-post-command-hook nil t))

(add-hook 'org-mode-hook '+org-latex-auto-toggle-setup)

(defun +org-update-latex-option-by-theme (theme)
  (setq org-format-latex-options
        (plist-put org-format-latex-options :theme theme)))

(add-hook '+after-change-theme-hook '+org-update-latex-option-by-theme)

(use-package org-roam
  :hook
  (after-init . org-roam-mode)
  :custom
  (org-format-latex-options (plist-put org-format-latex-options :scale 4.0))

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
        ("C-c n l" . org-roam)
        ("C-c n f" . org-roam-find-file)
        ("C-c n g" . org-roam-graph))
  (:map org-mode-map
        ("C-c n i" . org-roam-insert)
        ("C-c n I" . org-roam-insert-immediate))
  :config
  ;; https://www.orgroam.com/manual.html#Roam-Protocol
  (require 'org-roam-protocol))

;;; install latex with
;;; pacman -S texlive-bin texlive-most

(use-package org-superstar
  :hook (org-mode . org-superstar-mode))

(defvar-local +org-last-in-latex nil)

(provide 'init-org)
