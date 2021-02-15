;;; -*- lexical-binding: t; -*-

(defun setup-org-font ()
  (set-face-attribute 'org-table nil :family +mono-ufont-family))

(defun setup-markdown-font ()
  (set-face-attribute 'markdown-table-face nil :family +mono-ufont-family))

(with-eval-after-load "org"
  (add-hook 'org-mode-hook 'setup-org-font))

(with-eval-after-load "markdown"
  (add-hook 'markdown-mode-hook 'setup-markdown-font))

(straight-use-package
 '(ligature :type git
	        :host github
	        :repo "mickeynp/ligature.el"))

;;; ligature

(require 'ligature)

(global-ligature-mode t)

(with-eval-after-load "ligature"
  (ligature-set-ligatures 'emacs-lisp-mode
                          '("->" "->>" "<=" ">="))
  (ligature-set-ligatures 'elixir-mode
                          '("->" "=>" "|>" "<-" ">=" "<=" "!=" "!==" "===" "==" "::" "++" "&&" "||" "<<" ">>"))
  (ligature-set-ligatures 'clojure-mode
                          '("->" "->>" ">=" "<="  ".-"))
  (ligature-set-ligatures 'web-mode
                          '("</" "<!--" "-->" "/>")))

(provide 'init-font)
