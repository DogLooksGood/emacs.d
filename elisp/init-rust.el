(defun +rust-whitespace ()
  (interactive)
  (if (or (+in-string-p)
          (+in-comment-p))
      (call-interactively #'self-insert-command)
    (if (equal 59 (char-before))
        (progn
          (delete-char -1)
          (self-insert-command 1 ?:))
      (call-interactively #'self-insert-command))))

(defun +rust-lessthan ()
  (interactive)
  (if (or (+in-string-p)
          (+in-comment-p))
      (call-interactively #'self-insert-command)
    (if (equal 32 (char-before))
        (self-insert-command 1 ?<)
      (call-interactively #'self-insert-command))))

(defun +rust-semicolon ()
  (interactive)
  (cond
   ((or (+in-string-p)
        (+in-comment-p))
    (call-interactively #'self-insert-command))

   ((equal 58 (char-before))
    (self-insert-command 1 ?:))

   ((equal 59 (char-before))
    (progn
      (delete-char -1)
      (insert "::")))

   ((not (equal (point) (line-end-position)))
    (self-insert-command 1 ?:))

   (t (call-interactively #'self-insert-command))))

(defun +rust-minus ()
  "Will insert a minus if we are after whitespace and not at the indentation,otherwise will insert a underscore."
  (interactive)
  (if (and (or (+in-string-p)
               (+in-comment-p)
               (and (equal 32 (char-before))
		    (let ((pos (point)))
		      (not (equal pos
				  (save-mark-and-excursion
				    (back-to-indentation)
				    (point))))))))
      (call-interactively #'self-insert-command)
    (self-insert-command 1 ?_)))

(use-package rust-mode
  :bind
  (:map
   rust-mode-map
   ("-" . '+rust-minus)
   ("<" . '+rust-lessthan)
   ("SPC" . '+rust-whitespace)
   (";" . '+rust-semicolon)))

(use-package cargo
  :bind
  (:map
   rust-mode-map
   ("C-c C-c" . 'cargo-process-run)
   ("C-c C-t t" . 'cargo-process-current-test)
   ("C-c C-t f" . 'cargo-process-current-file-tests)
   ("C-c C-t p" . 'cargo-process-test)
   ("C-c C-k" . 'cargo-process-check)))

(provide 'init-rust)
