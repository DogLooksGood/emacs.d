(setq gc-cons-threshold most-positive-fixnum
	  gc-cons-percentage 0.6)

(add-hook 'after-init-hook
		  (lambda ()
            (setq gc-cons-threshold (* 1024 1024 1024))))

(add-to-list 'load-path (expand-file-name "elisp" user-emacs-directory))

(let ((private-conf (expand-file-name "private.el" user-emacs-directory)))
  (when (file-exists-p private-conf)
    (load-file private-conf)))

(require 'init-defaults)
(require 'init-straight)
(require 'init-laf)
