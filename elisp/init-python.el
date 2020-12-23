(use-package python-mode)


(use-package conda
  :commands (conda-env-activate conda-env-list)
  :config
  (conda-env-initialize-interactive-shells)
  (conda-env-initialize-eshell)
  :custom
  (conda-anaconda-home "/opt/anaconda/")
  (conda-env-home-directory (expand-file-name "~/.conda")))

(provide 'init-python)
