;; -*- lexical-binding: t; -*-
;; Add emacs to DM
;; cp ~/.emacs.d/emacs.desktop /usr/share/xsessions/

(setq exwm-mode-map
  (let ((map (make-sparse-keymap)))
    map))

(defun +exwm-windmove-up-or-down ()
  (interactive)
  (or (windowp (ignore-errors (windmove-down)))
      (ignore-errors (windmove-up))))

(defun +exwm-setup ()
  "Default configuration of EXWM."
  ;; Set the initial workspace number.
  (unless (get 'exwm-workspace-number 'saved-value)
    (setq exwm-workspace-number 9))
  ;; Make class name the buffer name
  (add-hook 'exwm-update-class-hook
            (lambda ()
              (exwm-workspace-rename-buffer exwm-class-name)))
  ;; Global keybindings.
  (unless (get 'exwm-input-global-keys 'saved-value)
    (setq exwm-input-global-keys
          `(([?\s-x] . meow-keypad-start)
            ([?\s-m] . meow-keypad-start)
            ([?\s-g] . meow-keypad-start)
            ([?\s-c] . meow-keypad-start)
            ([?\s-\ ] . ,meow-leader-keymap)
            ([?\s-f] . exwm-floating-toggle-floating)
            ([?\s-r] . exwm-layout-toggle-fullscreen)
            ([s-return] . +exwm-open-terminal)
            ([?\s-d] . +exwm-launch)
            ([?\s-b] . switch-to-buffer)
            ([?\s-w] . ace-window)
            ([?\s-q] . meow-quit)
            ([?\s-o] . delete-other-windows)
            ([?\s-k] . kill-buffer)
            ([?\s-\\] . split-window-right)
            ([?\s--] . split-window-below)
            ([?\s-h] . windmove-left)
            ([?\s-t] . windmove-right)
            ([?\s-n] . +exwm-windmove-up-or-down)
            ([s-right] . enlarge-window-horizontally)
            ([s-left] . shrink-window-horizontally)
            ([s-up] . enlarge-window)
            ([s-down] . shrink-window)
            ,@(mapcar (lambda (i)
                        `(,(kbd (format "<s-f%s>" (1+ i))) .
                          (lambda ()
                            (interactive)
                            (exwm-workspace-switch-create ,i))))
                      (number-sequence 0 8))
            ,@(mapcar (lambda (i)
                        `(,(kbd (format "<S-s-f%s>" (1+ i))) .
                          (lambda ()
                            (interactive)
                            (exwm-workspace-move-window ,i))))
                      (number-sequence 0 8))))))

(defun +exwm-open-terminal ()
  (interactive)
  (start-process-shell-command "urxvt" nil "urxvt"))

(defvar +systemtray-start nil)
(defvar +systemtray-apps nil)
(mapc (lambda (a) (add-to-list '+systemtray-apps a))
      '("nm-applet"
        "picom"
        "xset r rate 200 40"
        "fcitx5"
        "volumeicon"
        "setxkbmap -option ctrl:swapcaps"))

(defun +exwm-systemtray-reload (&rest args)
  (exwm-systemtray--refresh-all))

(defun +exwm-systemtray-setup ()
  (unless +systemtray-start
    (mapc (lambda (a) (start-process-shell-command a nil a))
          +systemtray-apps)
    (setq +systemtray-start t)))

(defface exwm-workspace-face
  '((((class color) (background dark))
     (:inverse-video t))
    (((class color) (background light))
     (:inverse-video t)))
  "Face for workspace indicator in modeline.")

(defun +exwm-current-workspace ()
  (propertize
   (format " %d:%s " (1+ (or (exwm-workspace--position (selected-frame)) 0))
           (or exwm-title "Emacs"))
   'face
   'exwm-workspace-face))

(defun +exwm-launch (command)
  (interactive (list (read-shell-command "$ ")))
  (start-process-shell-command command nil command))

(defun +exwm-leader (&rest args)
  (interactive)
  (set-transient-map meow-leader-keymap))

(use-package exwm
  :straight
  (exwm :type git
        :host github
        :repo "ch11ng/exwm")
  :init
  (setq exwm-input-prefix-keys '(escape))
  (setq exwm-systemtray--icon-min-size 32)
  :config
  (require 'exwm-config)
  (require 'exwm-systemtray)
  (+exwm-setup)
  (+exwm-systemtray-setup)
  (exwm-enable)
  (exwm-systemtray-enable)
  (add-hook '+after-change-theme-hook #'+exwm-systemtray-reload))

(provide 'init-exwm)
