;;; -*- lexical-binding: t -*-

(defvar +after-change-theme-hook nil
  "Hooks called after theme is changed.")

(add-to-list 'load-path (expand-file-name "themes" user-emacs-directory))

(require 'printed-theme)
(use-package joker-theme
  :straight
  (joker-theme :type git
               :host github
               :repo "DogLooksGood/joker-theme"))
(require 'storybook-theme)

(let ((margin 0))
  (add-to-list 'default-frame-alist (cons 'internal-border-width margin))
  (set-frame-parameter nil 'internal-border-width margin))

(let ((alpha 90))
  (add-to-list 'default-frame-alist (cons 'alpha alpha))
  (set-frame-parameter nil 'alpha alpha))

(defvar +theme-list
  '(storybook joker printed))

(set-display-table-slot standard-display-table
                        'vertical-border
                        (make-glyph-code ?┃))

(defun +change-theme (&optional no-msg)
  (interactive)
  (mapc #'disable-theme custom-enabled-themes)
  (setq +theme-list (append (cdr +theme-list) (list (car +theme-list))))
  (let ((this-theme (car +theme-list)))
    (load-theme this-theme t)
    (unless no-msg
      (message "Load theme: %s" this-theme))
    (run-hook-with-args '+after-change-theme-hook this-theme)))

(defun +highlight-prog-mode-function-name ()
  (face-remap-add-relative 'font-lock-function-name-face :underline t :extend t))

(add-hook 'prog-mode-hook '+highlight-prog-mode-function-name)

(defun +reload-font-and-theme ()
  (interactive)
  (+load-font)
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme (car +theme-list) t))

(+change-theme t)


(add-hook 'after-make-frame-functions
          (lambda (frame)
            (set-window-fringes
             (minibuffer-window frame) 0 0 nil t)))

(provide 'init-laf)
