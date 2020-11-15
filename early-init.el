(add-hook 'after-init-hook
		  (lambda () (setq gc-cons-threshold (* 1024 1024))))

(setq gc-cons-threshold most-positive-fixnum
	  gc-cons-percentage 0.6)

(add-to-list 'load-path (expand-file-name "elisp" user-emacs-directory))

(require 'init-laf)
