
;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
(setq user-full-name "Vlad Zarytovskii"
      user-mail-address "vzaritovsky@hotmail.com")

(setq doom-font (font-spec :family "monospace" :size 14))
(setq doom-theme 'doom-vibrant)
(setq org-directory "~/org/")

(setq display-line-numbers-type 'relative)
(setq show-trailing-whitespace t)

(setq +lsp-company-backend '(company-lsp :with company-tabnine :separate))
(setq highlight-indent-guides-auto-enabled nil)

(use-package! powershell
  :after org
  :config
    (add-to-list 'org-src-lang-modes '("powershell" . powershell))
    (add-to-list 'org-babel-load-languages '(powershell . t))
    (add-to-list 'org-babel-tangle-lang-exts '("powershell" . "ps1"))
    (setq powershell-location-of-exe (or (executable-find "powershell") (executable-find "pwsh")))
  :mode
    ("\\.ps1\\'" . powershell-mode))

(use-package! snails
  :after (doom-themes)
  :config
    (defun snails3()
      (interactive)
      (snails '(snails-backend-awesome-tab-group
                snails-backend-buffer
                snails-backend-recentf
                snails-backend-imenu
                snails-backend-projectile)))
    (add-hook 'snails-mode-hook (lambda () (evil-emacs-state)))
    (global-set-key (kbd "C-S-p") 'snails3))

(use-package! lsp-ui
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :config
    (set-lookup-handlers! 'lsp-ui-mode
                          :definition #'lsp-ui-peek-find-definitions
                          :references #'lsp-ui-peek-find-references)
    (setq lsp-ui-doc-max-height 16
          lsp-ui-doc-max-width 50
          lsp-ui-sideline-ignore-duplicate t
          lsp-ui-doc-enable t
          lsp-ui-sideline-show-hover t)
    (global-set-key (kbd "F12") #'lsp-ui-peek-find-definitions)
    (global-set-key (kbd "C-F12") #'lsp-ui-peek-find-references))

(use-package! company-lsp
  :after lsp-mode
  :config
  (set-company-backend! 'lsp-mode 'company-lsp)
  (setq company-lsp-enable-recompletion t))

(use-package! dap-mode
  :after lsp-mode
  :config
  (dap-mode t)
  (dap-ui-mode t))

(use-package! focus
  :config '((prog-mode . defun) (text-mode . sentence)))

(use-package! dimmer
  :hook (after-init . dimmer-mode)
  :config
    (dimmer-configure-which-key)
    (dimmer-configure-helm)
    (dimmer-mode t))

(use-package! beacon
  :hook (after-init . beacon-mode))

(use-package! sublimity
  :config
  (require 'sublimity-scroll)
  (require 'sublimity-map)
  (require 'sublimity-attractive)
  (setq sublimity-scroll-weight 10
        sublimity-scroll-drift-length 5
        sublimity-map-size 20
        sublimity-map-fraction 0.3
        sublimity-map-text-scale -7)
  (add-hook 'sublimity-map-setup-hook
          (lambda ()
            (setq buffer-face-mode-face '(:family "Monospace"))
            (buffer-face-mode))))
            
(use-package! treemacs
  :config
  (setq treemacs-show-hidden-files t)
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode t))

(use-package! counsel
    :hook
    (after-init . ivy-mode)
    (counsel-grep-post-action . better-jumper-set-jump)
    :diminish ivy-mode
    :config
    (setq counsel-find-file-ignore-regexp "\\(?:^[#.]\\)\\|\\(?:[#~]$\\)\\|\\(?:^Icon?\\)"
          counsel-describe-function-function #'helpful-callable
          ncounsel-describe-variable-function #'helpful-variable
          counsel-rg-base-command "rg -S --no-heading --line-number --color never %s ."
          counsel-ag-base-command "ag -S --nocolor --nogroup %s"
          counsel-pt-base-command "pt -S --nocolor --nogroup -e %s"
          counsel-find-file-at-point t

     (use-package! ivy-rich
       :config
       (ivy-rich-mode 1)
       (setq ivy-format-function #'ivy-format-function-line))))
     ;;[[https://github.com/gilbertw1/better-jumper][gilbertw1/better-jumper: A configurable jump list implementation for Emacs]]

(use-package! company-tabnine
  :when (featurep! :completion company)
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
  (advice-add #'company-tabnine :around #'my-company-tabnine)
  (setq company-idle-delay 0)
  (setq company-show-numbers t)
  (company-tng-configure-default)
  (setq company-frontends
        '(company-tng-frontend
          company-pseudo-tooltip-frontend
          company-echo-metadata-frontend)))

(after! company
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 2
        company-transformers nil)
  (setq company-show-numbers t)
  (define-key company-active-map (kbd "C-n") 'company-select-next-or-abort)
  (define-key company-active-map (kbd "C-j") 'company-select-previous-or-abort))


(set-company-backend! '(c-mode
                        c++-mode
                        ess-mode
                        haskell-mode
                        ;;emacs-lisp-mode
                        lisp-mode
                        sh-mode
                        php-mode
                        python-mode
                        go-mode
                        ruby-mode
                        rust-mode
                        js-mode
                        css-mode
                        web-mode)

  '(:separate company-tabnine
              company-files
              company-yasnippet))

