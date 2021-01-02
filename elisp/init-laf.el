;;; -*- lexical-binding: t -*-

(add-to-list 'load-path (expand-file-name "themes" user-emacs-directory))

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
        (message "Load theme: %s" this-theme)))))

(defun +highlight-prog-mode-function-name ()
  (face-remap-add-relative 'font-lock-function-name-face :underline t :extend t))

(add-hook 'prog-mode-hook '+highlight-prog-mode-function-name)

(defun +reload-font-and-theme ()
  (interactive)
  (+load-font)
  (load-theme (car +theme-list) t))

(+change-theme t)

;; Mode Line

;;; bench mark modeline.
;; (+measure-time (format-mode-line mode-line-format))

(defun +simple-mode-line-render (left right)
  "Return a string of `window-width' length.
Containing LEFT, and RIGHT aligned respectively."
  (let ((available-width
         (- (window-total-width)
            (+ (length (format-mode-line left))
               (length (format-mode-line right)))
            1)))
    (append left
            (list (format (format "%%%ds" available-width) ""))
            right)))

(setq-default mode-line-format
              '((:eval
                 (+simple-mode-line-render
                  ;; left
                  '("%l:%C "
                    (:propertize (-3 "%p") face +modeline-dim-face)
                    (:eval (propertize " " 'display '(height 1.2)))
                    (:eval (when (bound-and-true-p rime-mode) (rime-lighter))))
                  ;; right
                  '((:propertize " %m " face font-lock-keyword-face)
                    (:eval (when (functionp '+smart-file-name-with-propertize) (+smart-file-name-with-propertize)))
                    " ")))))

(provide 'init-laf)
