(straight-use-package 'yascroll)

(require 'yascroll)

(add-hook 'prog-mode-hook 'yascroll-bar-mode)
(add-hook 'conf-mode-hook 'yascroll-bar-mode)

(provide 'init-misc)
