;;; -*- lexical-binding: t -*-

(defvar +font-family "Fira Code")
(defvar +ufont-family "Sarasa Mono SC")
(defvar +font-size 10)

(let* ((font-spec (format "%s-%d" +font-family +font-size)))
  (if (not (member +font-family (font-family-list)))
      (message "Font '%s' not available!" font-family)
    (set-face-attribute 'default nil :font font-spec)
    (set-frame-font font-spec t nil)))

(let ((ufont-family +ufont-family))
  (if (not (member ufont-family (font-family-list)))
      (message "Font '%s' not available!" ufont-family)
    (setq-default face-font-rescale-alist `((,ufont-family . 0.95)))
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font)
                        charset
                        (font-spec :family ufont-family)))))

(use-package ligature
  :straight
  (ligature :type git
	        :host github
	        :repo "mickeynp/ligature.el")
  :config
  (ligature-set-ligatures 'emacs-lisp-mode
                          '("->" "->> <= >="))
  (ligature-set-ligatures 'elixir-mode
                          '("->" "=>" "|>" "<-" ">=" "<=" "!=" "!==" "===" "==" "::" "++" "&&" "||" "<<" ">>" "#{"))
  (ligature-set-ligatures 'clojure-mode
                          '("->" "->>" ">=" "<=" "#(" "#_" "~@" "#:" "#?" ".-" "#{"))
  (ligature-set-ligatures 'web-mode
                          '("</" "<!--" "-->" "/>"))
  (global-ligature-mode t))

(provide 'init-font)
