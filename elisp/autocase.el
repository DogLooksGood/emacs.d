
;; camelCase
;;
;; kebab-case
;;
;; snake_case
;;
;; PascalCase

(defvar-local autocase-cases
  '(PascalCase kebab-case)
  "The cases used to switching in current buffer.")

(defvar autocase--enter-key-alist
  '((?- . snake)
    (?, . camel)
    (?+ . pascal)
    (?; . constant)))

(defvar autocase-confirm-key 'return)

(defvar autocase--last-last-input-event nil)

(defvar-local autocase--activate nil)
(defvar-local autocase--overlay nil)

(defface autocase-face
  '((((class color) (background dark))
     (:inverse-video t))
    (((class color) (background light))
     (:inverse-video t)))
  "Autocase overlay face.")

(defun autocase--make-overlay (beg end)
  (let ((ov (make-overlay beg end)))
    (overlay-put ov 'face 'autocase-face)
    (setq autocase--overlay ov)))

(defun autocase--start-snake ()
  (setq autocase--activate 'snake)
  (delete-region (- (point) 2) (1- (point)))
  (autocase--make-overlay (1- (point)) (point)))

(defun autocase--start-camel ()
  (setq autocase--activate 'camel)
  (delete-region (- (point) 2) (1- (point)))
  (autocase--make-overlay (1- (point)) (point)))

(defun autocase--extend-snake ()
  (let ((beg (overlay-start autocase--overlay))
        (end (point)))
    (delete-overlay autocase--overlay)
    (goto-char beg)
    (while (< (point) end)
      (if (equal (char-after) ? )
          (progn (delete-char 1)
                 (insert "_"))
        (forward-char 1)))
    (autocase--make-overlay beg end)))

(defun autocase--extend-camel ()
  (let ((beg (overlay-start autocase--overlay))
        (end (point)))
    (delete-overlay autocase--overlay)
    (when (equal (char-before (1- (point))) ? )
      (delete-region (- (point) 2) (1- (point)))
      (upcase-char -1)
      (cl-decf end))
    (autocase--make-overlay beg end)))

(defun autocase--end ()
  (delete-char -1)
  (when autocase--overlay
    (delete-overlay autocase--overlay))
  (setq autocase--activate nil
        autocase--overlay nil
        autocase--last-last-input-event nil))

(defun autocase--cancel ()
  (when autocase--overlay
    (delete-overlay autocase--overlay))
  (setq autocase--activate nil
        autocase--overlay nil
        autocase--last-last-input-event nil))

(defun autocase--post-self-insert-function ()
  (cond
   ((and autocase--activate (equal last-input-event autocase-confirm-key))
    (autocase--end))

   ((equal autocase--activate 'snake)
    (autocase--extend-snake))

   ((equal autocase--activate 'camel)
    (autocase--extend-camel))

   ((and (equal autocase--last-last-input-event ?-)
         (not (equal last-input-event ? )))
    (autocase--start-snake))

   ((and (equal autocase--last-last-input-event ?,)
         (not (equal last-input-event ? )))
    (autocase--start-camel)))
  (setq autocase--last-last-input-event last-input-event))

(defun autocase--post-command-function ()
  (when (and autocase--overlay
             (let ((beg (overlay-start autocase--overlay))
                   (end (overlay-end autocase--overlay)))
               (or (< (point) beg)
                   (> (point) end)
                   (= beg end))))
    (autocase--cancel)))

(defun autocase--init ()
  (add-hook 'post-self-insert-hook #'autocase--post-self-insert-function nil t)
  (add-hook 'post-command-hook #'autocase--post-command-function nil t))

(defun autocase--uninit ()
  (remove-hook 'post-self-insert-hook #'autocase--post-self-insert-function t)
  (remove-hook 'post-command-hook #'autocase--post-command-function t)
  (when autocase--overlay
    (delete-overlay autocase--overlay))
  (setq autocase--activate nil
        autocase--overlay nil
        autocase--last-last-input-event nil))

(define-minor-mode autocase-mode
  "TBD"
  nil
  "autocase"
  nil
  (if autocase-mode
      (autocase--init)
    (autocase--uninit)))

(define-key global-map (kbd "<f9>") #'autocase-mode)
