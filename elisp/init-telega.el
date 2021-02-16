;;; -*- lexical-binding: t; -*-

(straight-use-package 'telega)

(setq
 telega-use-images t
 telega-open-file-function 'org-open-file
 telega-proxies '((:server "localhost" :port 1089 :enable t :type (:@type "proxyTypeSocks5"))))

(autoload #'telega "telega")

(global-set-key (kbd "<f6>") 'telega)

(with-eval-after-load "telega"
  (add-hook 'telega-chat-mode-hook 'company-mode)
  (setcdr (assq t org-file-apps-gnu) 'browse-url-xdg-open))

(provide 'init-telega)
