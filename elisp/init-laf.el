;;; -*- lexical-binding: t -*-

(straight-use-package
 '(joker-theme :type git
               :host github
               :repo "DogLooksGood/joker-theme"))

;;; No scroll bar
(scroll-bar-mode -1)

;;; No tool bar
(tool-bar-mode -1)

;;; No menu bar
(menu-bar-mode -1)

;;; No cursor blink
(add-hook 'after-init-hook (lambda () (blink-cursor-mode -1)))

;;; Nice window divider

(set-display-table-slot standard-display-table
                        'vertical-border
                        (make-glyph-code ?┃))

;;; No fringe in minibuffer

(add-hook 'after-make-frame-functions
          (lambda (frame)
            (set-window-fringes
             (minibuffer-window frame) 0 0 nil t)))

;;; Margin

(let ((margin 0))
  (add-to-list 'default-frame-alist (cons 'internal-border-width margin)))

;;; Transparency

(let ((alpha 100))
  (add-to-list 'default-frame-alist (cons 'alpha alpha)))

;;; Fonts

(defvar +font-family "Fira Code")
(defvar +ufont-family "WenQuanYi Micro Hei Mono")
(defvar +mono-ufont-family "Sarasa Mono SC")
(defvar +font-size 11)
(defvar +ufont-scale 1)

(let* ((font-spec (format "%s-%d" +font-family +font-size)))
  (add-to-list 'default-frame-alist `(font . ,font-spec)))

(defun +load-font ()
  (let* ((font-spec (format "%s-%d" +font-family +font-size)))
    (set-frame-font font-spec))
  (setq-default face-font-rescale-alist `((,+ufont-family . ,+ufont-scale)))
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font)
                      charset
                      (font-spec :family +ufont-family))))

(add-hook 'after-init-hook '+load-font)

;;; Theme

(defvar +after-change-theme-hook nil
  "Hooks called after theme is changed.")

(add-to-list 'load-path (expand-file-name "themes" user-emacs-directory))

(defvar +theme-list
  (if window-system
      '(joker storybook printed)
    '(joker)))

(defun +change-theme (&optional init)
  (interactive)
  (mapc #'disable-theme custom-enabled-themes)
  (let ((theme (car +theme-list)))
    (require (intern (concat (symbol-name theme) "-theme")))
    (load-theme theme t)
    (setq +theme-list (append (cdr +theme-list) (list (car +theme-list))))
    (unless init
      (+load-font)
      (message "Load theme: %s" theme)
      (run-hook-with-args '+after-change-theme-hook theme))))

(defun +highlight-prog-mode-function-name ()
  (face-remap-add-relative 'font-lock-function-name-face :underline t :extend t))

(add-hook 'prog-mode-hook '+highlight-prog-mode-function-name)

(+change-theme t)

(provide 'init-laf)
