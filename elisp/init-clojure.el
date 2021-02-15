;;; -*- lexical-binding: t -*-

(straight-use-package 'clojure-mode)
(straight-use-package 'clj-refactor)
(straight-use-package 'cider)

;;; clojure-mode

(setq
 clojure-toplevel-inside-comment-form t)

(autoload #'clojure-mode "clojure-mode")

(with-eval-after-load "clojure-mode"
  (modify-syntax-entry ?: "w" clojure-mode-syntax-table)
  (require 'init-clojure-highlight-fix)

  (add-hook 'clojure-mode-hook 'paredit-mode)
  (add-hook 'clojure-mode-hook 'clj-refactor-mode)

  (define-key clojure-mode-map (kbd "C-c C-i") 'cider-inspect-last-result))

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

(autoload #'cider-jack-in "cider")
(autoload #'cider-jack-in-cljs "cider")
(autoload #'cider-jack-in-clj&cljs "cider")
(autoload #'cider "cider")

(provide 'init-clojure)
