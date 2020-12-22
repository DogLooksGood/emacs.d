;;; -*- lexical-binding: t -*-

(let* ((font-family "Cascadia Code")
       (font-size 10)
       (font-spec (format "%s-%d" font-family font-size)))
  (when (member font-family (font-family-list))
    (set-face-attribute 'default nil :font font-spec)
    (set-frame-font font-spec t nil)))

(let ((cn-font-family "Sarasa Mono SC"))
  (when (member cn-font-family (font-family-list))
    (setq-default face-font-rescale-alist `((,cn-font-family . 1)))
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font)
                        charset
                        (font-spec :family cn-font-family)))))

(use-package ligature
  :straight
  (ligature :type git
	        :host github
	        :repo "mickeynp/ligature.el")
  :config
  (ligature-set-ligatures 'emacs-lisp-mode
                          '("->" "->>"))
  (ligature-set-ligatures 'elixir-mode
                          '("->" "=>" "|>" "<-" ">=" "<=" "!=" "!==" "===" "==" "::" "++" "&&" "||" "<<" ">>" "#{"))
  (ligature-set-ligatures 'clojure-mode
                          '("->" "->>" ">=" "<=" "#(" "#_" "~@" "#:" "#?" ".-" "#{"))
  (ligature-set-ligatures 'web-mode
                          '("</" "<!--" "-->" "/>"))
  (global-ligature-mode t))

(provide 'init-font)
