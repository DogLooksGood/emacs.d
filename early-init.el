(add-hook 'after-init-hook
		  (lambda () (setq gc-cons-threshold (* 256 1024))))

(setq gc-cons-threshold most-positive-fixnum
	  gc-cons-percentage 0.6)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

(add-to-list 'load-path (expand-file-name "elisp" user-emacs-directory))
(require 'init-look-and-feel)
