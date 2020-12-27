(use-package telega
  :bind
  ("<f6>" . telega)
  :custom
  (telega-proxies
   '((:server "localhost" :port 1089 :enable t :type (:@type "proxyTypeSocks5")))))

(provide 'init-telega)
