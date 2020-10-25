;;; -*- lexical-binding: t -*-

;; Support Find Thing
(use-package meow
  :straight
  (meow :type git
	:host github
	:repo "DogLooksGood/meow"
        :branch "develop")
  :init
  (meow-global-mode 1)
  :custom
  (meow-esc-delay 0.001)
  :config
  (add-to-list 'meow-normal-state-mode-list 'inf-iex-mode)
  (add-to-list 'meow-normal-state-mode-list 'inf-iex-mode)
  (add-to-list 'meow-normal-state-mode-list 'help-mode)
  (add-to-list 'meow-normal-state-mode-list 'deadgrep-edit-mode)
  (meow-leader-define-key
   '("d" . nox-show-doc)
   '("L" . display-line-numbers-mode)
   '("k" . kill-buffer)
   '("l" . meow-keypad-start)
   '("o" . other-window)
   '("q" . delete-other-windows)
   '("v" . magit)
   '("$" . +reload-theme)
   '("-" . split-window-below)
   '("\\" . split-window-right)
   '("p" . project-find-file)
   '("b" . switch-to-buffer)
   '("g" . deadgrep)
   '("f" . find-file)
   '("i" . imenu))
  (meow-normal-define-key))

(provide 'init-modal)
