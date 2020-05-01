(setq user-full-name "Vlad Zarytovskii"
      user-mail-address "vzaritovsky@hotmail.com"
      doom-themes-treemacs-line-spacing 0
      doom-themes-treemacs-enable-variable-pitch t
      doom-themes-treemacs-theme "doom-colors"
      doom-modeline-height 22
      doom-theme 'doom-tomorrow-night
      doom-font (font-spec :family "monospace" :size 17)
      all-the-icons-scale-factor 1)

(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
(setq package-check-signature nil)
(package-initialize)

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

;; Auto theme switch
;; -- Automatically switch between ligh and dark theme based on time of day
(setq theme-autoswitch t)
(setq theme-autoswitch/light-theme 'doom-tomorrow-day)
(setq theme-autoswitch/dark-theme 'doom-tomorrow-night)
(setq theme-autoswitch/day-start-hour 7)
(setq theme-autoswitch/day-end-hour 19)
(setq theme-autoswitch/sync-timer 300)
(if (and theme-autoswitch (display-graphic-p))
    (progn
      (defun sync-theme-with-time ()
        (setq theme-autoswitch/hour (string-to-number (substring (current-time-string) 11 13)))
        (if (member theme-autoswitch/hour (number-sequence theme-autoswitch/day-start-hour theme-autoswitch/day-end-hour))
            (setq theme-autoswitch/now theme-autoswitch/light-theme)
          (setq theme-autoswitch/now theme-autoswitch/dark-theme))
        (unless (and (boundp 'current-theme) (eq theme-autoswitch/now current-theme))
          (progn
            (setq current-theme theme-autoswitch/now)
            (load-theme theme-autoswitch/now t))))
      (sync-theme-with-time)
      (run-with-timer 0 theme-autoswitch/sync-timer #'sync-theme-with-time))
  (load-theme theme-autoswitch/dark-theme t))

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
        lsp-ui-doc-header nil
        lsp-ui-doc-border "green"
        lsp-ui-doc-max-height 30
        lsp-ui-doc-max-width 120
        lsp-ui-doc-include-signature t
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
  (add-to-list 'magit-section-initial-visibility-alist '(unpulled . show))
  (add-to-list 'magit-section-initial-visibility-alist '(unpushed . show))
  (add-to-list 'magit-section-initial-visibility-alist '(untracked . show))
  (add-to-list 'magit-section-initial-visibility-alist '(unstaged . show))
  (add-to-list 'magit-section-initial-visibility-alist '(todos . show))
  (add-to-list 'magit-section-initial-visibility-alist '(recent . show))
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
  (company-tng-configure-default)
  (setq company-idle-delay 0.0
        company-minimum-prefix-length 2
        company-transformers nil
        company-show-numbers nil)

  (setq company-frontends
    '(company-tng-frontend company-pseudo-tooltip-frontend company-echo-metadata-frontend))
  (set-company-backend! '(prog-mode)
   '(:separate company-lsp company-tabnine company-files (company-dabbrev company-dabbrev-code) company-keywords company-yasnippet))
  (setq +lsp-company-backend '(company-lsp :with company-tabnine :separate company-files company-keywords company-yasnippet)))

(use-package! company-lsp
  :defer 2
  :ensure t
  :after company
  :commands company-lsp
  :preface
  (defun push-company-lsp-backends ()
   "Push company-lsp to the backends."
   (general-pushnew
     '(company-lsp
       company-tabnine
       company-files
       (company-dabbrev company-dabbrev-code)
       company-keywords
       company-yasnippet)
     company-backends))
  (defun company-lsp-init-h ()
    "Make sure that `company-capf' is disabled since it is incompatible with
`company-lsp' (see lsp-mode#884)."
    (if (not (bound-and-true-p company-mode))
        (add-hook 'company-mode-hook #'company-lsp-init-h t t)
      (setq-local company-backends
                  (cons 'company-lsp
                        (remq 'company-capf company-backends)))
      (remove-hook 'company-mode-hook #'company-lsp-init-h t)))
  :hook ((lsp-mode . company-lsp-init-h))
  :init
  (setq company-lsp-async               t
        company-lsp-enable-recompletion t
        company-lsp-enable-snippet      t
        company-lsp-cache-candidates    'auto))

(use-package! company-tabnine
 :defer 2
 :ensure t
 :after company
 :config
 (setq company-tabnine--disable-next-transform nil)
 (defun my-company--transform-candidates (func &rest args)
  (if (not company-tabnine--disable-next-transform)
    (apply func args)
    (setq company-tabnine--disable-next-transform nil)
    (car args)))

 (defun my-company-tabnine (func &rest args)
  (when (eq (car args) 'candidates)
    (setq company-tabnine--disable-next-transform t))
  (apply func args))

  (advice-add #'company--transform-candidates :around #'my-company--transform-candidates)
  (advice-add #'company-tabnine :around #'my-company-tabnine)) 

(use-package! treemacs
 :defer 2
 :ensure t
 :config
 (treemacs-follow-mode 1))

(use-package! lsp-treemacs
 :defer 2
 :ensure t
 :commands lsp-treemacs-errors-list
 :after treemacs
 :config
 (lsp-metals-treeview-enable t)
 (setq lsp-metals-treeview-show-when-views-received t))
