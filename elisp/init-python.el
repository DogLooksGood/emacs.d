
(defun +python-semicolon ()
  (interactive)
  (if (or (+in-comment-p) (+in-string-p))
      (call-interactively #'self-insert-command)
    (self-insert-command 1 ?:)))


(defun +python-minus ()
  "Will insert a minus if we are after whitespace and not at the indentation,otherwise will insert a underscore."
  (interactive)
  (if (and (or (+in-comment-p)
               (+in-string-p)
               (and (equal 32 (char-before))
		    (let ((pos (point)))
		      (not (equal pos
				  (save-mark-and-excursion
				    (back-to-indentation)
				    (point))))))))
      (call-interactively #'self-insert-command)
    (self-insert-command 1 ?_)))

(defun +init-python-keybinding ()
  (bind-key ";" '+python-semicolon python-mode-map)
  (bind-key "-" '+python-minus python-mode-map))

(add-hook 'python-mode-hook #'+init-python-keybinding)

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
