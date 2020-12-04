;;; -*- lexical-binding: t -*-

(use-package rime
  :commands (toggle-input-method)
  :bind
  (:map
   rime-active-mode-map
   ("<tab>" . 'rime-inline-ascii)
   :map rime-mode-map
   ("C-`" . 'rime-send-keybinding)
   ("M-j" . 'rime-force-enable))
  :custom
  ((rime-disable-predicates '(meow-normal-mode-p
                              meow-motion-mode-p
                              meow-keypad-mode-p))
   (rime-inline-predicates '(rime-predicate-space-after-cc-p
                             rime-predicate-current-uppercase-letter-p))
   (rime-translate-keybindings '("C-f" "C-b" "C-n" "C-p" "C-g"))
   (rime-inline-ascii-holder ?a)
   (default-input-method "rime")
   (rime-cursor "˰")
   (rime-show-candidate 'posframe)
   (rime-title "RIME")
   (rime-posframe-fixed-position t)))

(provide 'init-input-method)
