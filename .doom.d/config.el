;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
(setq user-full-name "Vlad Zarytovskii"
      user-mail-address "vzaritovsky@hotmail.com")

(setq doom-font (font-spec :family "monospace" :size 14))
(setq doom-theme 'doom-one)
(setq org-directory "~/org/")

(setq display-line-numbers-type 'relative)
(setq show-trailing-whitespace t)

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

(use-package! company
  :config
  (global-company-mode)
  (setq company-idle-delay 0
        company-show-numbers t
        company-tooltip-align-annotations t
        global-company-mode t)
  ;; (company-tng-configure-default)
  (setq company-backends '(company-capf)
                      company-ispell
                      company-yasnippet
                      company-etags
                      company-elisp
                      company-files
                      company-gtags)
  (setq company-frontends
        '(company-tng-frontend
          company-pseudo-tooltip-frontend
          company-echo-metadata-frontend)))

(use-package! company-quickhelp
  :after company
  :config
  (company-quickhelp-mode)
  (setq company-quickhelp-delay 0))
