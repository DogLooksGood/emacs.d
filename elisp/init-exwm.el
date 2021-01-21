;; -*- lexical-binding: t; -*-

(setq exwm-mode-map
  (let ((map (make-sparse-keymap)))
    map))

(defun +exwm-setup ()
  "Default configuration of EXWM."
  ;; Set the initial workspace number.
  (unless (get 'exwm-workspace-number 'saved-value)
    (setq exwm-workspace-number 4))
  ;; Make class name the buffer name
  (add-hook 'exwm-update-class-hook
            (lambda ()
              (exwm-workspace-rename-buffer exwm-class-name)))
  ;; Global keybindings.
  (unless (get 'exwm-input-global-keys 'saved-value)
    (setq exwm-input-global-keys
          `(
            ;; 's-r': Reset (to line-mode).
            ([?\s-r] . exwm-reset)
            ;; 's-w': Switch workspace.
            ([?\s-w] . exwm-workspace-switch)
            ;; 's-&': Launch application.
            ([?\s-&] . (lambda (command)
                         (interactive (list (read-shell-command "$ ")))
                         (start-process-shell-command command nil command)))
            ;; 's-<F*>': Switch to certain workspace.
            ,@(mapcar (lambda (i)
                        `(,(kbd (format "s-<f%s>" (1+ i))) .
                          (lambda ()
                            (interactive)
                            (exwm-workspace-switch-create ,i))))
                      (number-sequence 0 9))))))

(defvar +system-tray-start nil)
(defun +exwm-system-tray-setup ()
  (unless +system-tray-start
    (start-process-shell-command "nm-applet" nil "nm-applet")
    (start-process-shell-command "picom" nil "picom -b")
    (start-process-shell-command "xset" nil "xset r rate 200 40")
    (start-process-shell-command "fcitx5" nil "fcitx5")
    (start-process-shell-command "qv2ray" nil "qv2ray")
    (start-process-shell-command "cbatticon" nil "cbatticon")
    (start-process-shell-command "volumeicon" nil "volumeicon")
    (start-process-shell-command "setxkbmap" nil "setxkbmap -option ctrl:swapcaps")
    (setq +system-tray-start t)))

(defface exwm-workspace-face
  '((((class color) (background dark))
     (:inverse-video t))
    (((class color) (background light))
     (:inverse-video t)))
  "Face for workspace indicator in modeline.")

(defun +exwm-current-workspace ()
  (propertize
   (format "[%d]" (1+ (or (exwm-workspace--position (selected-frame)) 0)))
   'face
   'exwm-workspace-face))

(use-package exwm
  :straight
  (exwm :type git
        :host github
        :repo "ch11ng/exwm")
  :init
  (setq exwm-input-prefix-keys '(?\s-x ?\s-c ?\s-m ?\s-g ?\s-h ?\s-\ ?\s-f ?\C-\\))
  (setq exwm-systemtray--icon-min-size 48)
  :bind
  ("s-x" . 'meow-keypad-start)
  ("s-m" . 'meow-keypad-start)
  ("s-g" . 'meow-keypad-start)
  ("s-c" . 'meow-keypad-start)
  ("s-h" . 'meow-keypad-start)
  ("s-f" . 'exwm-floating-toggle-floating)
  :config
  (global-set-key (kbd "s-SPC") meow-leader-keymap)
  (require 'exwm-config)
  (require 'exwm-systemtray)
  (+exwm-setup)
  (+exwm-system-tray-setup)
  (exwm-systemtray-enable)
  (exwm-enable))

(provide 'init-exwm)
