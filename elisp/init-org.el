;;; -*- lexical-binding: t -*-

(use-package org
  :straight (:type built-in))

(defun +org-toggle-emphasis ()
  "Toggle hiding/showing of org emphasize markers."
  (interactive)
  (if org-hide-emphasis-markers
      (setq org-hide-emphasis-markers nil)
    (setq org-hide-emphasis-markers t)))

(use-package org-roam
  :hook
  (after-init . org-roam-mode)
  :custom
  (org-format-latex-options (plist-put org-format-latex-options :scale 4.0))
  (org-roam-directory (expand-file-name "~/Org"))
  (org-roam-capture-templates
   '(("d" "default" plain (function org-roam--capture-get-point)
      "%?"
      :file-name "%<%Y%m%d%H%M%S>-${slug}"
      :head "#+title: ${title}\n"
      :unnarrowed t)))
  :bind
  (:map org-mode-map
        ("C-c C-x RET" . +org-toggle-emphasis))
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

(provide 'init-org)
