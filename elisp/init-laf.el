;;; -*- lexical-binding: t -*-

(straight-use-package
 '(joker-theme :type git
               :host github
               :repo "DogLooksGood/joker-theme"))

(setq joker-theme-main-color "#B762DE")
(require 'joker-theme)
(require 'storybook-theme)
(require 'printed-theme)

;;; No scroll bar
(scroll-bar-mode -1)

;;; No tool bar
(tool-bar-mode -1)

;;; No menu bar
(menu-bar-mode -1)

(window-divider-mode 1)

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

;;; No window decoration

(add-to-list 'default-frame-alist (cons 'undecorated t))

;;; Fonts

(defvar +font-family "Fira Code")
(defvar +ufont-family "WenQuanYi Micro Hei Mono")
(defvar +fixed-pitch-family "Fira Code")
(defvar +variable-pitch-family "Fira Sans")
(defvar +mono-ufont-family "Sarasa Mono SC")
(defvar +font-size 10)
(defvar +ufont-scale 1)

;;; (+load-font)

(let* ((font-spec (format "%s-%d" +font-family +font-size)))
  (add-to-list 'default-frame-alist `(font . ,font-spec)))

(defun +load-font ()
  (let* ((font-spec (format "%s-%d" +font-family +font-size))
         (vp-font-spec (format "%s-%d" +variable-pitch-family +font-size)))
    (set-frame-font font-spec)
    (set-face-attribute 'variable-pitch nil :font vp-font-spec)
    (set-face-attribute 'fixed-pitch nil :font vp-font-spec))
  (setq-default face-font-rescale-alist `((,+ufont-family . ,+ufont-scale)))
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font)
                      charset
                      (font-spec :family +ufont-family))))

(add-hook 'after-init-hook '+load-font)

;;; Theme

(defvar +after-change-theme-hook nil
  "Hooks called after theme is changed.")

(defvar +theme-list '(joker printed storybook))

(defun +change-theme (&optional init)
  (interactive)
  (mapc #'disable-theme custom-enabled-themes)
  (let ((theme (car +theme-list)))
    (load-theme theme t)
    (setq +theme-list (append (cdr +theme-list) (list (car +theme-list))))
    (unless init
      (+load-font)
      (message "Load theme: %s" theme)
      (run-hook-with-args '+after-change-theme-hook theme))))

(+change-theme t)

(provide 'init-laf)
