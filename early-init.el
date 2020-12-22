(setq gc-cons-threshold most-positive-fixnum
	  gc-cons-percentage 0.6
      file-name-handler-alist-original file-name-handler-alist)

(add-hook 'after-init-hook
		  (lambda ()
            (setq gc-cons-threshold (* 1024 1024 1024)
                  file-name-handler-alist file-name-handler-alist-original)))

(add-to-list 'load-path (expand-file-name "elisp" user-emacs-directory))

(require 'init-defaults)
(require 'init-laf)
