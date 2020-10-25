;;; -*- lexical-binding: t -*-

(use-package company
  :hook (company-mode . company-tng-mode)
  :bind
  (:map
   company-active-map
   ("<tab>" . 'company-complete-selection)
   ("TAB" . 'company-complete-selection)
   ("<escape>")
   ("RET")
   ("<return>")
   ("SPC")
   :map
   company-template-nav-map
   ("RET" . 'company-template-forward-field)
   ("<return>" . 'company-template-forward-field)
   ("TAB")
   ("<tab>"))
  :init
  (require 'company-template)
  :hook
  ((prog-mode . company-mode)
   (conf-mode . company-mode)
   (eshell-mode . company-mode))
  :custom
  (company-tng-auto-configure nil)
  (company-frontends '(company-tng-frontend
                       company-pseudo-tooltip-frontend
                       company-echo-metadata-frontend))
  (company-begin-commands '(self-insert-command))
  (company-idle-delay 0)
  (company-tooltip-limit 10)
  (company-tooltip-align-annotations t)
  (company-tooltip-offset-display 'lines)
  (company-tooltip-width-grow-only t)
  (company-tooltip-idle-delay 0.1)
  (company-minimum-prefix-length 3)
  (company-dabbrev-downcase nil)
  (company-abort-manual-when-too-short t)
  (company-require-match nil)
  (company-global-modes '(not dired-mode dired-sidebar-mode))
  (company-tooltip-margin 0))

(when window-system
  (use-package posframe)
  (use-package company-posframe
	:init
	(company-posframe-mode 1)
	:custom
    (company-posframe-show-at-prefix )
	(company-posframe-quickhelp-delay nil)
	(company-posframe-show-indicator nil)
	(company-posframe-show-metadata nil)))

(use-package ctrlf
  :init
  (ctrlf-mode 1))

(use-package selectrum
  :init
  (selectrum-mode 1))
(use-package prescient)
(use-package selectrum-prescient
  :init
  (selectrum-prescient-mode 1))

;; (use-package ivy
;;   :init
;;   (ivy-mode 1)
;;   :custom
;;   (ivy-use-selectable-prompt t))
;;
;; (use-package counsel
;;   :init
;;   (counsel-mode 1))

(defun +yas-next ()
  (interactive)
  (yas-next-field-or-maybe-expand))

(defun +yas-init ()
  (yas-reload-all))

(advice-add '+yas-init :around #'+make-silent)

(use-package yasnippet
  :bind
  (:map
   yas-keymap
   ("<escape>")
   ("RET" . 'yas-next-field-or-maybe-expand)
   ("<return>" . 'yas-next-field-or-maybe-expand)
   ("M-<return>" . 'newline-and-indent)
   ("S-<return>" . 'yas-prev-field))
  :config
  (+yas-init)
  (unbind-key "<return>" yas-keymap)
  (unbind-key "S-<return>" yas-keymap)
  (unbind-key "<tab>" yas-keymap)
  (unbind-key "TAB" yas-keymap)
  (unbind-key "S-TAB" yas-keymap)
  :init
  (add-hook 'prog-mode-hook #'yas-minor-mode))

(use-package deadgrep
  :bind
  (:map deadgrep-mode-map
		("w" . 'deadgrep-edit-mode))
  (:map deadgrep-edit-mode-map
		("C-x C-s" . 'deadgrep-mode)))

(provide 'init-completion)
