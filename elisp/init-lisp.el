;;; -*- lexical-binding: t -*-

(defun +lisp-semicolon ()
  "Will insert a semicolon if we are at the beginning of the line,
otherwise will insert a colon."
  (interactive)
  (if (or (+in-comment-p)
          (+in-string-p)
          (equal (point) (line-beginning-position)))
      (call-interactively #'self-insert-command)
    (insert ":")))

(use-package paredit
  :straight
  (paredit :type git
           :host github
           :repo "emacsmirror/paredit")
  :bind
  (:map paredit-mode-map
		(";" . '+lisp-semicolon))
  :hook
  (emacs-lisp-mode . paredit-mode)
  (lisp-mode . paredit-mode))

(provide 'init-lisp)
