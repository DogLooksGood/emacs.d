;;; -*- lexical-binding: t -*-

(use-package dash)
(require 'subr-x)

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
  (let ((vc-dir (vc-root-dir))
        (bfn (buffer-file-name (current-buffer))))
    (cond
     ((and bfn vc-dir)
      (file-relative-name bfn vc-dir))
     (bfn bfn)
     (t (buffer-name)))))

(defmacro +measure-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (message "%.06fs" (float-time (time-since time)))))

(defface +modeline-dim-face
  '((((class color) (background dark))
     (:foreground "grey40"))
    (((class color) (background light))
     (:foreground "grey60")))
  "Dim face in mode-line")

(defvar-local +smart-file-name-with-propertize-cache nil
  "Cache for performance, is a cons of (buffer-name . cached-value).")

(defun +smart-file-name-cached ()
  (-when-let ((buf-name p f) +smart-file-name-with-propertize-cache)
    (when (string-equal buf-name (buffer-file-name))
      (let ((face (cond
                   ((buffer-modified-p) 'font-lock-string-face)
                   (buffer-read-only 'font-lock-comment-face)
                   (t nil))))
        (concat (propertize p 'face '+modeline-dim-face) (propertize f 'face face))))))

(defun +smart-file-name-cached-no-propertize ()
  (-when-let ((buf-name p f) +smart-file-name-with-propertize-cache)
    (when (string-equal buf-name (buffer-file-name))
      (string-truncate-left (concat p f) 30))))

(defun +smart-file-name-with-propertize ()
  (if-let ((cached (+smart-file-name-cached)))
      cached
    (let ((vc-dir (vc-root-dir))
          (bfn (buffer-file-name (current-buffer))))
      (cond
       ((and bfn vc-dir)
        (let ((fname (file-relative-name bfn vc-dir))
              (p (file-name-directory fname))
              (f (file-name-nondirectory fname)))
          (setq-local +smart-file-name-with-propertize-cache (list (buffer-file-name) p f))
          (+smart-file-name-cached)))
       (bfn bfn)
       (t (buffer-name))))))

(defun +smart-file-name-truncated ()
  (if-let ((cached (+smart-file-name-cached-no-propertize)))
      cached
    (let* ((fname (+smart-file-name))
           (slist (split-string fname "/"))
           (p (concat (string-join (-butlast slist) "/") "/"))
           (f (-last-item slist)))
      (setq-local +smart-file-name-with-propertize-cache (list (buffer-file-name) p f))
      (+smart-file-name-cached-no-propertize))))

(defun +file-vc-state-with-propertize ()
  (when-let ((sym (vc-state (buffer-file-name (current-buffer)))))
    (format "%s" sym)))

(defun +vc-branch ()
  (car (vc-git-branches)))

(defun +project-name ()
  "Get project name, which is used in title format."
  (cond
   (+project-name-cache +project-name-cache)
   ((project-current)
    (setq-local +project-name-cache
                (-> (project-root (project-current))
                    (string-trim-right "/")
                    (file-name-base))))
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

(defun +my-throw-error (&rest args)
  (when (equal (car args) "Text is read-only")
    a))

;; (advice-add 'message :after #'+my-throw-error)
;; (advice-remove 'message #'+my-throw-error)

(provide 'init-util)
