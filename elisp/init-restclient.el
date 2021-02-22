
(straight-use-package 'restclient)

(add-to-list 'auto-mode-alist '("\\.restclient\\'" . restclient-mode))

(straight-use-package 'ob-restclient)
(require 'ob-restclient)

(with-eval-after-load "ob-restclient" (org-babel-do-load-languages 'org-babel-load-languages '((restclient . t))))

(provide 'init-restclient)
