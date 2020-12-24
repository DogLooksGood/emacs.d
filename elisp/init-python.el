
(use-package conda
  :commands (conda-env-activate conda-env-list)
  :config
  (conda-env-initialize-interactive-shells)
  (conda-env-initialize-eshell)
  :custom
  (conda-anaconda-home
   (if (file-directory-p "/opt/anaconda/")
       "/opt/anaconda/"
     "/opt/miniconda3/"))
  (conda-env-home-directory (expand-file-name "~/.conda")))

(provide 'init-python)
