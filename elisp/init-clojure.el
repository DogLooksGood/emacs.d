;;; -*- lexical-binding: t -*-

(straight-use-package 'clojure-mode)
(straight-use-package 'clj-refactor)
(straight-use-package 'cider)
(straight-use-package 'inf-clojure)
(straight-use-package 'flycheck)
(straight-use-package 'flycheck-clj-kondo)

;;; clojure-mode

(setq
 clojure-toplevel-inside-comment-form t)

(autoload #'clojure-mode "clojure-mode")

(with-eval-after-load "clojure-mode"
  (modify-syntax-entry ?: "w" clojure-mode-syntax-table)
  (require 'init-clojure-highlight-fix)

  (add-hook 'clojure-mode-hook 'paredit-mode)
  (add-hook 'clojure-mode-hook 'clj-refactor-mode)
  (add-hook 'clojure-mode-hook 'flycheck-mode)

  (define-key clojure-mode-map (kbd "C-c C-i") 'cider-inspect-last-result)

  (require 'flycheck-clj-kondo))

(with-eval-after-load "flycheck"
  (define-key flycheck-mode-map (kbd "M-n") 'flycheck-next-error)
  (define-key flycheck-mode-map (kbd "M-p") 'flycheck-previous-error)
  (setq flycheck-idle-change-delay 1))

;;; clj-refactor

(setq
 cljr-warn-on-eval t
 cljr-suppress-middleware-warnings t)

(autoload #'clj-refactor-mode "clj-refactor")

(with-eval-after-load "clj-refactor"
  (define-key clj-refactor-map (kbd "/") nil)
  (cljr-add-keybindings-with-prefix "C-c C-r"))

;;; cider

(setq
 cider-font-lock-dynamically nil
 cider-font-lock-reader-conditionals nil
 cider-use-fringe-indicators t
 cider-prompt-for-symbol nil
 cider-save-file-on-load t
 cider-enhanced-cljs-completion-p nil
 cider-offer-to-open-cljs-app-in-browser nil)

(autoload #'cider-jack-in "cider" nil t)
(autoload #'cider-jack-in-cljs "cider" nil t)
(autoload #'cider-jack-in-clj&cljs "cider" nil t)
(autoload #'cider "cider" nil t)

(with-eval-after-load "cider"
  (define-key cider-mode-map (kbd "C-c M-s") #'cider-browse-spec)
  (define-key cider-mode-map (kbd "C-c C-f") #'cider-format-buffer)
  (define-key cider-mode-map (kbd "C-c f") #'cider-pprint-eval-defun-at-point))

;;; inf-clojure

(autoload #'inf-clojure-connect "inf-clojure" nil t)

(provide 'init-clojure)
