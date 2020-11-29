;;; programming.el --- Programming-related packages configuration (such as editorconfig, code folding, lsp, etc.).
;;; Commentary:
;; Core packages config, for main config, see config.el

;;; -*- lexical-binding: t -*-

;; General config
(use-package eldoc
  :straight nil
  :delight)

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

;; Flymake and flymake configs:

(use-package flycheck
  :hook (after-init-hook . global-flycheck-mode)
  :config
  (setq flycheck-highlighting-mode 'sexps))

(use-package flycheck-inline
  :after flycheck
  :hook (flycheck-mode-hook . flycheck-inline-mode))

;; Company mode config:
(use-package company
  :delight
  :straight (:host github :repo "company-mode/company-mode" :branch "master")
  :hook (prog-mode-hook . global-company-mode)
  :bind (:map company-active-map
              ("C-w" . 'backward-kill-word))
  :config
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.01)
  (push 'company-capf company-backends))

(use-package company-quickhelp
  :after company
  (company-quickhelp-mode))

(use-package company-box
  :delight
  :after company
  :hook (company-mode-hook . company-box-mode))

(use-package company-prescient
  :after (:all company prescient))

(use-package company-posframe
  :delight
  :after company
  :config
  (company-posframe-mode 1))

;; LSP Configuration:
(use-package lsp-mode)

;; Language-specific configs:
;; .NET
(use-package fsharp-mode)

(provide 'programming)
;;; programming.el ends here
