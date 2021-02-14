;;; -*- lexical-binding: t -*-

(straight-use-package 'yasnippet)
(straight-use-package 'company)
(straight-use-package 'ivy)
(straight-use-package 'counsel)
(straight-use-package 'deadgrep)

(defun +complete ()
  (interactive)
  (or (yas/expand)
      (company-indent-or-complete-common nil)))

;;; yasnippet

(autoload #'yas-minor-mode "yasnippet")

(add-hook 'prog-mode-hook 'yas-minor-mode)

(with-eval-after-load "yasnippet"
  (let ((inhibit-message t))
    (yas-reload-all))

  (define-key yas-keymap [escape] nil)
  (define-key yas-keymap [tab] nil)
  (define-key yas-keymap (kbd "S-<tab>") nil)
  (define-key yas-keymap (kbd "TAB") nil)
  (define-key yas-keymap [return] 'yas-next-field-or-maybe-expand)
  (define-key yas-keymap (kbd "RET") 'yas-next-field-or-maybe-expand)
  (define-key yas-keymap (kbd "S-<return>") 'yas-prev-field))

;;; company

(custom-set-variables
 '(company-tng-auto-configure nil)
 '(company-frontends '(company-tng-frontend
                       company-pseudo-tooltip-frontend
                       company-echo-metadata-frontend))
 '(company-begin-commands '(self-insert-command))
 '(company-idle-delay 0.2)
 '(company-tooltip-limit 10)
 '(company-tooltip-align-annotations t)
 '(company-tooltip-width-grow-only t)
 '(company-tooltip-idle-delay 0.1)
 '(company-minimum-prefix-length 3)
 '(company-dabbrev-downcase nil)
 '(company-abort-manual-when-too-short t)
 '(company-require-match nil)
 '(company-global-modes '(not dired-mode dired-sidebar-mode))
 '(company-tooltip-margin 0))

(autoload #'company-mode "company")

(add-hook 'prog-mode-hook 'company-mode)
(add-hook 'confmode-hook 'company-mode)
(add-hook 'eshell-mode-hook 'company-mode)

(with-eval-after-load "company"
  (require 'company-tng)
  (require 'company-template)
  (add-hook 'company-mode-hook 'company-tng-mode)

  (define-key company-mode-map [tab] '+complete)
  (define-key company-mode-map (kbd "TAB") '+complete)
  (define-key company-active-map [tab] 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
  (define-key company-active-map [escape] nil)
  (define-key company-active-map [return] nil)
  (define-key company-active-map (kbd "RET") nil)
  (define-key company-active-map (kbd "SPC") nil)
  (define-key company-active-map (kbd "SPC") nil)

  (define-key company-template-nav-map (kbd "RET") 'company-template-forward-field)
  (define-key company-template-nav-map [return] 'company-template-forward-field)
  (define-key company-template-nav-map (kbd "TAB") nil)
  (define-key company-template-nav-map [tab] nil))

;;; ivy

(custom-set-variables
 '(ivy-use-selectable-prompt t))

(require 'ivy)

(ivy-mode t)

;;; counsel

(require 'counsel)

(counsel-mode t)

;;; deadgrep

(autoload #'deadgrep "deadgrep")

(with-eval-after-load "deadgrep"
  (define-key deadgrep-mode-map (kbd "w") 'deadgrep-edit-mode)
  (define-key deadgrep-edit-mode-map (kbd "C-x C-s") 'deadgrep-mode))

(provide 'init-completion)
