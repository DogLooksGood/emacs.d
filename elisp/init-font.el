;;; -*- lexical-binding: t -*-

(defvar +font-family "Fira Code")
(defvar +ufont-family "WenQuanYi Micro Hei Mono")
(defvar +mono-ufont-family "Sarasa Mono SC")
(defvar +font-size 11)
(defvar +ufont-scale 1)

(defun +load-font ()
  (let* ((font-spec (format "%s-%d" +font-family +font-size)))
    (if (not (member +font-family (font-family-list)))
        (message "Font '%s' not available!" +font-family)
      (set-face-attribute 'default nil :font font-spec)
      (set-frame-font font-spec t nil)))

  (if (not (member +ufont-family (font-family-list)))
      (message "Font '%s' not available!" +ufont-family)
    (setq-default face-font-rescale-alist `((,+ufont-family . ,+ufont-scale)))
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font)
                        charset
                        (font-spec :family +ufont-family)))))

(+load-font)

(defun setup-org-font ()
  (set-face-attribute 'org-table nil :family +mono-ufont-family))

(defun setup-markdown-font ()
  (set-face-attribute 'markdown-table-face nil :family +mono-ufont-family))

(add-hook 'org-mode-hook 'setup-org-font)
(add-hook 'markdown-mode-hook 'setup-markdown-font)

(use-package ligature
  :straight
  (ligature :type git
	        :host github
	        :repo "mickeynp/ligature.el")
  :config
  (ligature-set-ligatures 'emacs-lisp-mode
                          '("->" "->>" "<=" ">="))
  (ligature-set-ligatures 'elixir-mode
                          '("->" "=>" "|>" "<-" ">=" "<=" "!=" "!==" "===" "==" "::" "++" "&&" "||" "<<" ">>"))
  (ligature-set-ligatures 'clojure-mode
                          '("->" "->>" ">=" "<="  ".-"))
  (ligature-set-ligatures 'web-mode
                          '("</" "<!--" "-->" "/>"))
  (global-ligature-mode t))

(provide 'init-font)
