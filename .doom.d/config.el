(setq user-full-name "Vlad Zarytovskii"
      user-mail-address "vzaritovsky@hotmail.com"
      doom-themes-treemacs-line-spacing 0
      doom-themes-treemacs-enable-variable-pitch t
      doom-themes-treemacs-theme "doom-colors"
      doom-modeline-height 22
      doom-theme 'doom-vibrant
      doom-font                   (font-spec :family "JetBrains Mono 1.1" :size 14)
      doom-big-font               (font-spec :family "JetBrains Mono 1.1" :size 24)
      doom-variable-pitch-font    (font-spec :family "Noto Sans" :size 14)
      doom-unicode-font           "Noto Color Emoji"
      all-the-icons-scale-factor 1)

(setq org-directory "~/org/")
(setq display-line-numbers-type 'relative)

(setq split-width-threshold nil)
(setq split-width-threshold 160)

(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)
(setq initial-scratch-message nil)

(setq uniquify-buffer-name-style 'post-forward-angle-brackets) ; Show path if names are same
(setq adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*")
(setq adaptive-fill-first-line-regexp "^* *$")
(setq sentence-end "\\([。、！？]\\|……\\|[,.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
(setq sentence-end-double-space nil)
(setq delete-by-moving-to-trash t)    ; Deleting files go to OS's trash folder
(setq make-backup-files nil)          ; Forbide to make backup files
(setq auto-save-default nil)          ; Disable auto save
(setq set-mark-command-repeat-pop t)  ; Repeating C-SPC after popping mark pops it again
(setq track-eol t)      ; Keep cursor at end of lines.
(setq line-move-visual nil)    ; To be required by track-eol
(setq-default kill-whole-line t)  ; Kill line including '\n'
(setq-default indent-tabs-mode nil)   ; use space
(defalias 'yes-or-no-p #'y-or-n-p)

(defun display-startup-echo-area-message ()
  (message ""))

(set-popup-rule! "*backtrace\*"      :size 0.5            :side 'bottom :select t :quit t :modeline t)
(set-popup-rule! "*doom:scratch"     :size 0.25 :vslot -4 :side 'bottom :select t :quit t :ttl nil :modeline nil)

;;(use-package server
;;  :ensure nil
;;  :hook (after-init . server-mode))

(use-package! dap-mode
  :defer 2
  :ensure t
  :after lsp-mode
  :config
  (dap-mode t)
  (dap-ui-mode t))

(use-package! lsp-mode
  :defer 2
  :ensure t
  :config
  (setq lsp-navigation 'both
        lsp-signature-render-all t
        lsp-enable-symbol-highlighting t
        lsp-enable-snippet t
        lsp-enable-folding t
        lsp-enable-indentation t
        lsp-enable-on-type-formatting t
        lsp-enable-file-watchers t
        lsp-enable-xref t
        lsp-semantic-highlighting t
        lsp-eldoc-enable-hover t
        lsp-eldoc-enable-signature-help nil
        lsp-document-sync-method 'incremental
        lsp-signature-render-all t)
  :custom
  (lsp-auto-guess-root t)
  (lsp-document-sync-method 'incremental) ;; none, full, incremental, or nil
  (lsp-response-timeout 10)
  (lsp-prefer-flymake nil)
  (lsp-mode t)
  (lsp-ui-mode t))

(use-package! lsp-ui
  :defer 2
  :ensure t
  :after lsp-mode
  :hook (add-hook 'lsp-ui-mode-hook #'+lsp/dim-lsp-sideline)
  :config
  (set-lookup-handlers! 'lsp-ui-mode :async t
    :definition #'lsp-ui-peek-find-definitions
    :references #'lsp-ui-peek-find-references)
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-header t
        lsp-ui-doc-border "green"
        lsp-ui-doc-max-height 30
        lsp-ui-doc-max-width 120
        lsp-ui-doc-include-signature nil
        lsp-ui-doc-delay 0.0
        lsp-ui-doc-position 'at-point
        lsp-ui-doc-use-childframe t
        lsp-ui-doc-use-webkit t
        lsp-ui-flycheck-enable t
        lsp-ui-peek-enable t
        lsp-ui-peek-fontify 'on-demand
        lsp-ui-peek-peek-height 20
        lsp-ui-peek-list-width 50
        lsp-ui-sideline-enable t
        lsp-ui-sideline-show-symbol t
        lsp-ui-sideline-show-diagnostics t
        lsp-ui-sideline-show-hover t
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-sideline-delay 0.0
        lsp-ui-sideline-code-actions-prefix ""
        lsp-ui-imenu-enable t
        lsp-ui-imenu-kind-position 'top))

(use-package! magit
  :defer 2
  :ensure t
  :after magit
  :hook (add-hook 'git-commit-setup-hook #'git-commit-turn-on-flyspell)
  :config
  (magit-todos-mode))

(use-package! imenu-list
  :config
  (setq imenu-list-auto-resize t)
  (setq imenu-list-focus-after-activation t)
  (setq imenu-list-after-jump-hook nil)
  (add-hook 'imenu-list-after-jump-hook #'recenter-top-bottom))

(use-package! company
  :defer 2
  :ensure t
  :after company
  :config
  (setq company-idle-delay 0.5
        company-minimum-prefix-length 2
        company-transformers nil
        company-show-numbers t))
