;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; refresh' after modifying this file!


;; These are used for a number of things, particularly for GPG configuration,
;; some email clients, file templates and snippets.
(setq user-full-name "John Doe"
      user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "monospace" :size 14))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. These are the defaults.
(setq doom-theme 'doom-one)

;; If you intend to use org, it is recommended you change this!
(setq org-directory "~/org/")

;; If you want to change the style of line numbers, change this to `relative' or
;; `nil' to disable it:
(setq display-line-numbers-type t)
(setq show-trailing-whitespace t)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', where Emacs
;;   looks when you load packages with `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

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
