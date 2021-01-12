;;; -*- lexical-binding: t -*-

(add-to-list 'load-path (expand-file-name "themes" user-emacs-directory))

(defvar +after-change-theme-hook nil
  "Hooks called after theme is changed.")

(require 'joker-theme)
(require 'storybook-theme)
(require 'printed-theme)

(let ((margin 24))
  (add-to-list 'default-frame-alist (cons 'internal-border-width margin))
  (set-frame-parameter nil 'internal-border-width margin))

(defvar +current-theme nil
  "Name for current theme")

(defvar +theme-list
  '(storybook joker printed))

(set-display-table-slot standard-display-table
                        'vertical-border
                        (make-glyph-code ?┃))

(defun +change-theme (&optional no-msg)
  (interactive)
  (let ((theme (car +theme-list)))
	(disable-theme theme)
	(setq +theme-list (append (cdr +theme-list) (list theme)))
    (let ((this-theme (car +theme-list)))
      (load-theme this-theme t)
      (unless no-msg
        (message "Load theme: %s" this-theme))
      (run-hook-with-args '+after-change-theme-hook this-theme))))

(defun +highlight-prog-mode-function-name ()
  (face-remap-add-relative 'font-lock-function-name-face :underline t :extend t))

(add-hook 'prog-mode-hook '+highlight-prog-mode-function-name)

(defun +reload-font-and-theme ()
  (interactive)
  (+load-font)
  (load-theme (car +theme-list) t))

(+change-theme t)

(provide 'init-laf)
