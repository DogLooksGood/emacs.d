;;; -*- lexical-binding: t -*-

(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(blink-cursor-mode -1)
(fringe-mode -1)
(setq window-divider-default-places 'right-only
      window-divider-default-right-width 1)
(window-divider-mode 1)

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

(add-hook 'after-make-frame-functions
          (lambda (frame)
            (set-window-scroll-bars
             (minibuffer-window frame) 0 nil 0 nil t)))

(defun +highlight-prog-mode-function-name ()
  (face-remap-add-relative 'font-lock-function-name-face :underline t :extend t))

(add-hook 'prog-mode-hook '+highlight-prog-mode-function-name)

(defun +reload-theme ()
  (interactive)
  (load-theme (car +theme-list) t))

(+change-theme t)

;; Mode Line

(setq-default frame-title-format '("Emacs" (:eval (+project-name))))

(defun +simple-mode-line-render (left right)
  "Return a string of `window-width' length.
Containing LEFT, and RIGHT aligned respectively."
  (let ((available-width
         (- (window-total-width)
            (+ (length (format-mode-line left))
               (length (format-mode-line right))))))
    (append left
            (list (format (format "%%%ds" available-width) ""))
            right)))

(setq-default mode-line-format
              '((:eval
                 (+simple-mode-line-render
                  ;; left
                  '(" "
                    (:eval (when (featurep 'meow) (meow-minimal-indicator)))
                    "%l:%c "
                    (-3 "%p")
                    " "
                    (:eval (when (bound-and-true-p rime-mode) (concat (rime-lighter) " ")))
                    (:eval (when (bound-and-true-p flycheck-mode) flycheck-mode-line)))
                  ;; right
                  '((:eval (when (functionp #'+smart-file-name) (+smart-file-name)))
                    "%* %m"
                    (vc-mode vc-mode)
                    " ")))))

(provide 'init-laf)
