;;; -*- lexical-binding: t -*-


(defvar-local +smart-file-name-cache nil
  "Cache for the smart file name of current buffer.")

(defvar-local +project-name-cache nil
  "Cache for current project name.")

(defun +in-string-p ()
  "Returns non-nil if inside string, else nil.
Result depends on syntax table's string quote character."
  (interactive)
  (or (nth 3 (syntax-ppss))
      (member 'font-lock-string-face
              (text-properties-at (point)))))

(defun +in-comment-p ()
  "Returns non-nil if inside comment, else nil.
Result depends on syntax table's comment character."
  (interactive)
  (nth 4 (syntax-ppss)))

(defun +smart-file-name ()
  "Get current file name, if we are in project, the return relative path to the project root, otherwise return absolute file path.
This function is slow, so we have to use cache."
  (cond
   (+smart-file-name-cache +smart-file-name-cache)
   ((and (buffer-file-name (current-buffer))
         (project-current))
    (setq-local +smart-file-name-cache
                (file-relative-name
                 (buffer-file-name (current-buffer))
                 (project-root (project-current)))))
   (t (setq-local +smart-file-name-cache (buffer-name)))))

(defun +project-name ()
  "Get project name, which is used in title format."
  (cond
   (+project-name-cache +project-name-cache)
   ((project-current)
    (setq-local +project-name-cache
                (format " : %s " (project-root (project-current)))))
   (t (setq-local +project-name-cache ""))))

(defun +setup-delete-trailing-whitespace ()
  (add-hook 'before-save-hook 'delete-trailing-whitespace t t))

(defun +make-silent (func &rest args)
  (cl-letf (((symbol-function 'message)
             (lambda (&rest args) nil)))
    (apply func args)))

(provide 'init-util)
