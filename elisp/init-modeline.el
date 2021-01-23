;; Mode Line  -*- lexical-binding: t; -*-

;;; bench mark modeline.
;; (+measure-time (format-mode-line mode-line-format))

(defun +simple-mode-line-render (left right)
  "Return a string of `window-width' length.
Containing LEFT, and RIGHT aligned respectively."
  (let ((available-width
         (- (window-total-width)
            (+ (string-width (format-mode-line left))
               (string-width (format-mode-line right)))
            1)))
    (append left
            (list (format (format "%%%ds" available-width) ""))
            right)))

;; (setq-default mode-line-format
;;               '((:eval
;;                  (+simple-mode-line-render
;;                   ;; left
;;                   '((:eval (meow-indicator))
;;                     " %l:%C "
;;                     (:propertize (-3 "%p") face +modeline-dim-face)
;;                     (:eval (propertize " " 'display '(height 1.2)))
;;                     (:eval (rime-lighter)))
;;                   ;; right
;;                   '((:propertize mode-name face font-lock-keyword-face)
;;                     " "
;;                     (:eval (+smart-file-name-with-propertize))
;;                     " ")))))

;;; Use EXWM
(setq-default display-time-format "%H:%M")
(display-time-mode 1)
(display-battery-mode 1)

(setq-default mode-line-format
              '((:eval
                 (+simple-mode-line-render
                  ;; left
                  '((:eval (+exwm-current-workspace))
                    (:eval (meow-indicator))
                    " %l:%C "
                    (:propertize (-3 "%p") face +modeline-dim-face)
                    " "
                    ;; (:eval (propertize " " 'display '(height 1.2)))
                    (:eval (rime-lighter)))
                  ;; right
                  '((:propertize mode-name face font-lock-keyword-face)
                    " "
                    (:eval (+smart-file-name-with-propertize))
                    " BAT"
                    (:propertize battery-mode-line-string face font-lock-constant-face)
                    " "
                    display-time-string)))))

(provide 'init-modeline)
