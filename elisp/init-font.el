(set-face-attribute 'default nil :font "IBM Plex Mono-10")

(setq face-font-rescale-alist '(("等距更纱黑体 SC" . 1)))
(dolist (charset '(kana han symbol cjk-misc bopomofo))
  (set-fontset-font (frame-parameter nil 'font)
                    charset
                    (font-spec :family "等距更纱黑体 SC")))

(provide 'init-font)
