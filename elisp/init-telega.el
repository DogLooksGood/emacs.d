(use-package telega
  :bind
  ("<f6>" . telega)
  :config
  (add-hook 'telega-chat-mode 'company-mode)
  (setcdr (assq t org-file-apps-gnu) 'browse-url-xdg-open)
  :custom
  (telega-open-file-function 'org-open-file)
  (telega-proxies
   '((:server "localhost" :port 1089 :enable t :type (:@type "proxyTypeSocks5")))))

(provide 'init-telega)
