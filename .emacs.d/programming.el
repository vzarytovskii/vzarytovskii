;;; programming.el --- Programming-related packages configuration (such as editorconfig, code folding, lsp, etc.).
;;; Commentary:
;; Core packages config, for main config, see config.el

;;; -*- lexical-binding: t -*-

;;; Code:

(use-package editorconfig
  :delight
  :config
  (editorconfig-mode 1))

(use-package origami
  :hook (prog-mode-hook . origami-mode)
  :preface
  (defun my/origami-elisp-parser (create)
    (origami-lisp-parser create
                         "(\\(def\\|cl-def\\|use-package\\|bind-keys\\)\\w*\\s-*\\(\\s_\\|\\w\\|[:?!]\\)*\\([ \\t]*(.*?)\\)?"))
  :config
  (setq origami-parser-alist
        (cons '(emacs-lisp-mode . my/origami-elisp-parser)
              (assq-delete-all 'emacs-lisp-mode origami-parser-alist))
        origami-show-fold-header t)
  :init
  (global-origami-mode))

(use-package origami-predef
  :after origami)

;; Company mode config:
(use-package company
  :straight (:host github :repo "company-mode/company-mode" :branch "master")
  :hook (prog-mode-hook . global-company-mode))

;; LSP Configuration:

;; Language-specific configs:
  
(provide 'programming)
;;; programming.el ends here
