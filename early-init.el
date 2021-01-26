;;; Personal configuration may override some variables
(let ((private-conf (expand-file-name "private.el" user-emacs-directory)))
  (when (file-exists-p private-conf)
    (load-file private-conf)))

(defvar +gc-cons-threshold gc-cons-threshold)

(defun +disable-gc () (setq gc-cons-threshold most-positive-fixnum))
(defun +enable-gc () (setq gc-cons-threshold +gc-cons-threshold))

(add-hook 'after-init-hook #'+enable-gc)
(add-hook 'minibuffer-setup-hook #'+disable-gc)
(add-hook 'minibuffer-exit-hook #'+enable-gc)
(+disable-gc)

(add-to-list 'load-path (expand-file-name "elisp" user-emacs-directory))

(require 'init-defaults)
(require 'init-straight)
(require 'init-laf)
