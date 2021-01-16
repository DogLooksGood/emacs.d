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
  (+org-babel-setup)
  :custom
  (org-html-preamble nil)
  (org-html-postamble nil)
  (org-html-checkbox-type 'unicode))

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

;;; install latex with
;;; pacman -S texlive-bin texlive-most
;;; install xdot
;;; pacman -S xdot

(use-package org-superstar
  :hook (org-mode . org-superstar-mode))

;;; Export with inline CSS

(defvar +org-css-themes
  '((dark . joker)
    (light . storybook)))

(defvar +org-css-this-theme nil)

(defun +org-css-parse-interpolate-clause (clause)
  (-let* (((th f a k) (split-string clause ":"))
          (fs (intern f))
          (as (intern (concat ":" a)))
          (req-theme (alist-get (intern th) +org-css-themes))
          (_ (unless (equal req-theme +org-css-this-theme)
               (disable-theme +org-css-this-theme)
               (load-theme req-theme t)
               (setq +org-css-this-theme req-theme))))
    (let ((v (face-attribute fs as)))
      (unless (equal v 'unspecified)
        (if k
            (plist-get v (intern (concat ":" k)))
          v)))))

(defun +org-css-find-interpolate-value (s)
  (let* ((clauses (-> s
                      (string-trim-left "#{")
                      (string-trim-right "}")
                      (split-string "|")))
         (vals (-keep #'+org-css-parse-interpolate-clause clauses)))
        (or (car vals) "initial")))

(defun +org-css-face-interpolate ()
  (let ((inhibit-redisplay t))
    (goto-char (point-min))
    (setq +org-css-this-theme (car +theme-list))
    (while (re-search-forward "#{.+?}" nil t)
      (-let* ((beg (match-beginning 0))
              (end (match-end 0))
              (s (buffer-substring-no-properties beg end))
              (v (+org-css-find-interpolate-value s)))
        (delete-region beg end)
        (insert v)))
    (disable-theme +org-css-this-theme)
    (load-theme (car +theme-list) t)))

(defun +org-export-inline-css (exporter)
  "Insert custom inline css"
  (when (eq exporter 'html)
    (let ((css-path (expand-file-name "assets/org.css" user-emacs-directory))
          (js-path (expand-file-name "assets/org.js" user-emacs-directory)))
      (setq org-html-preamble
            "<div id=\"toggle-theme\">dark theme</div>")
      (setq org-html-head-include-default-style nil)
      (setq org-html-head (concat
                           "<style type=\"text/css\">\n"
                           "<!--/*--><![CDATA[/*><!--*/\n"
                           (with-temp-buffer
                             (insert-file-contents css-path)
                             (+org-css-face-interpolate)
                             (buffer-string))
                           "/*]]>*/-->\n"
                           "</style>\n"))
      (setq org-html-postamble
            (concat
             "<script type=\"text/javascript\">\n"
             "<!--/*--><![CDATA[/*><!--*/\n"
             (with-temp-buffer
               (insert-file-contents js-path)
               (buffer-string))
             "/*]]>*/-->\n"
             "</script>")))))

(add-hook 'org-export-before-processing-hook '+org-export-inline-css)

(use-package htmlize
  :custom
  (htmlize-face-overrides
   '(
     font-lock-keyword-face (:foreground "var(--clr-keyword)" :background "var(--bg-keyword)")
     font-lock-constant-face (:foreground "var(--clr-constant)" :background "var(--bg-constant)")
     font-lock-comment-face (:foreground "var(--clr-comment)" :background "var(--bg-comment)")
     font-lock-comment-delimiter-face (:foreground "var(--clr-comment-delimiter)" :background "var(--bg-comment-delimiter)")
     font-lock-function-name-face (:foreground "var(--function-clr-name)" :background "var(--function-bg-name)")
     font-lock-variable-name-face (:foreground "var(--clr-variable)" :background "var(--bg-variable)")
     font-lock-preprocessor-face (:foreground "var(--clr-preprocessor)" :background "var(--bg-preprocessor)")
     font-lock-doc-face (:foreground "var(--clr-doc)" :background "var(--bg-doc)")
     font-lock-builtin-face (:foreground "var(--clr-builtin)" :background "var(--bg-builtin)")
     font-lock-string-face (:foreground "var(--clr-string)" :background "var(--bg-string)"))))

(provide 'init-org)
