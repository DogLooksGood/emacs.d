(use-package yascroll
  :init
  (global-yascroll-bar-mode 1))

(defun +update-window-divider ()
  "Only display window divider when there are more than two windows."
  (let ((n (length (window-list))))
    (if (or (meow-keypad-mode-p) (> n 1))
        (window-divider-mode 1)
      (window-divider-mode -1))))

(add-hook 'window-configuration-change-hook '+update-window-divider)
(add-hook 'meow-keypad-mode-hook '+update-window-divider)

(provide 'init-misc)
