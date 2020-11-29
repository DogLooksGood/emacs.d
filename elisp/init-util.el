;;; -*- lexical-binding: t -*-

(use-package dash)
(require 'subr-x)

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

(defun +make-silent (func &rest args)
  (cl-letf (((symbol-function 'message)
             (lambda (&rest args) nil)))
    (apply func args)))

;;; Case transform

(defun +to-pascal-case (s)
  (let* ((words (split-string s "-\\|_"))
         (capwords (mapcar #'capitalize words)))
    (string-join capwords "")))

(defun +color-blend (c1 c2 alpha)
  "Blend two colors C1 and C2 with ALPHA.
C1 and C2 are hexidecimal strings.
ALPHA is a number between 0.0 and 1.0 which corresponds to the
influence of C1 on the result."
  (ignore-errors
    (apply #'(lambda (r g b)
               format "#%02x%02x%02x"
               (ash r -8)
               (ash g -8)
               (ash b -8))
           (cl-mapcar
            (lambda (x y)
              (round (+ (* x alpha) (* y (- 1 alpha)))))
            (color-values c1) (color-values c2)))))

(provide 'init-util)
