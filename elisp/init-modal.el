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
  :config
  (add-to-list 'meow-normal-state-mode-list 'inf-iex-mode)
  (meow-leader-define-key
   '("L" . display-line-numbers-mode)
   '("k" . kill-buffer)
   '("h" . meow-keypad-start)
   '("l" . goto-line)
   '("o" . other-window)
   '("q" . delete-other-windows)
   '("m" . magit)
   '("-" . split-window-below)
   '("\\" . split-window-right)
   '("p" . project-find-file)
   '("b" . switch-to-buffer)
   '("g" . project-find-regexp)
   '("f" . find-file)))

(provide 'init-modal)
