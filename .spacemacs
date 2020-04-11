;; -*- mode: emacs-lisp; lexical-binding: t -*-

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-enable-lazy-installation 'unused
   dotspacemacs-ask-for-lazy-installation t
   dotspacemacs-configuration-layer-path '("~/.spacemacs.d")
   dotspacemacs-persistent-server t
   dotspacemacs-configuration-layers
   '(
     yaml
     (auto-completion
      :variables
      auto-completion-return-key-behavior 'complete
      auto-completion-tab-key-behavior 'cycle
      auto-completion-complete-with-key-sequence nil
      auto-completion-complete-with-key-sequence-delay 0.0
      auto-completion-idle-delay 0.0
      auto-completion-private-snippets-directory nil
      auto-completion-enable-snippets-in-popup t
      auto-completion-enable-help-tooltip t
      auto-completion-use-company-box t
      auto-completion-enable-sort-by-usage t
      company-frontends '(company-tng-frontend
                          company-pseudo-tooltip-frontend
                          ;company-preview-frontend
                          company-echo-metadata-frontend))
                          ;company-quickhelp-frontend))
     better-defaults
     helpful
     semantic
     emacs-lisp
     git
     github
     themes-megapack
     (helm
      :variables
      helm-enable-auto-resize t
      helm-no-header nil
      helm-position 'bottom
      helm-use-fuzzy 'source)
     (lsp
      :variables
      lsp-navigation 'both
      lsp-signature-render-all t
      lsp-enable-symbol-highlighting t
      lsp-enable-snippet t
      lsp-enable-folding t
      lsp-enable-xref t
      lsp-eldoc-enable-hover t
      lsp-eldoc-enable-signature-help nil
      lsp-document-sync-method 'incremental
      lsp-signature-render-all t
      lsp-ui-doc-enable t
      lsp-ui-doc-include-signature nil
      lsp-ui-doc-delay 0.0
      lsp-ui-doc-position 'at-point
      lsp-ui-doc-use-childframe t
      lsp-ui-peek-enable t
      lsp-ui-peek-fontify 'on-demand
      lsp-ui-sideline-enable t
      lsp-ui-sideline-show-symbol t
      lsp-ui-sideline-show-diagnostics t
      lsp-ui-sideline-show-hover t
      lsp-ui-sideline-show-code-actions t
      lsp-ui-sideline-delay 0.0)
     dap
     markdown
     (multiple-cursors :variables
                       multiple-cursors-backend 'evil-mc)
     org
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom)
     spell-checking
     (syntax-checking :variables
                      syntax-checking-enable-tooltips t
                      syntax-checking-enable-by-default t)
     version-control
     (treemacs :variables
               treemacs-use-follow-mode t
               treemacs-use-filewatch-mode t
               treemacs-use-git-mode 'deferred
               )
     (gtags :variables
            gtags-enable-by-defaults t)
     (csharp
      :variables
      csharp-backend 'lsp)
     fsharp
     (haskell :variables
              haskell-completion-backend 'lsp)
     (shell-scripts :variables
                    shell-scripts-backend 'lsp)
     windows-scripts
     (python :variables
             python-backend 'lsp
             python-lsp-server 'pyls
             python-pipenv-activate t
             python-test-runner '(pytest nose)
             python-format-on-save t
             python-fill-column 99
             python-sort-imports-on-save t)
     ipython-notebook
     (exwm :variables
           exwm-enable-systray t
           exwm-autostart-xdg-applications t
           exwm-locking-command "i3lock -n"
           exwm-install-logind-lock-handler t
           exwm-terminal-command "st -f \"monospace:pixelsize=14:antialias=true:autohint=true\""
           exwm-custom-init (lambda() (exwm/autostart-process "Dunst OSD" "dunst"))))

   dotspacemacs-additional-packages '(hl-block-mode yasnippet-snippets atom-dark-theme (forge :toggle t) magit-todos solaire-mode)
   dotspacemacs-frozen-packages '()
   dotspacemacs-excluded-packages '(window-purpose)
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  (setq-default
   dotspacemacs-enable-emacs-pdumper nil
   dotspacemacs-emacs-pdumper-executable-file "emacs"
   dotspacemacs-emacs-dumper-dump-file "spacemacs.pdmp"
   dotspacemacs-elpa-https t
   dotspacemacs-elpa-timeout 5
   dotspacemacs-gc-cons '(100000000 0.1)
   dotspacemacs-use-spacelpa nil
   dotspacemacs-verify-spacelpa-archives t
   dotspacemacs-check-for-update nil
   dotspacemacs-elpa-subdirectory 'emacs-version
   dotspacemacs-editing-style 'vim
   dotspacemacs-startup-buffer-show-version t
   dotspacemacs-startup-banner nil
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   dotspacemacs-startup-buffer-responsive t
   dotspacemacs-new-empty-buffer-major-mode 'text-mode
   dotspacemacs-scratch-mode 'text-mode
   dotspacemacs-initial-scratch-message nil
   dotspacemacs-themes '(doom-one alect-black-alt spacemacs-dark)
   dotspacemacs-mode-line-theme '(doom :separator wave :separator-scale 0.0)

   dotspacemacs-colorize-cursor-according-to-state t

   dotspacemacs-default-font '("Fira Code"
                               :size 11.0
                               :weight normal
                               :width normal)

   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-command-key "SPC"
   dotspacemacs-ex-command-key ":"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   dotspacemacs-distinguish-gui-tab nil
   dotspacemacs-default-layout-name "Default"
   dotspacemacs-display-default-layout nil
   dotspacemacs-auto-resume-layouts nil
   dotspacemacs-auto-generate-layout-names nil
   dotspacemacs-large-file-size 1
   dotspacemacs-auto-save-file-location 'cache
   dotspacemacs-max-rollback-slots 5
   dotspacemacs-enable-paste-transient-state nil
   dotspacemacs-which-key-delay 0.1
   dotspacemacs-which-key-position 'bottom
   dotspacemacs-switch-to-buffer-prefers-purpose nil
   dotspacemacs-loading-progress-bar t
   dotspacemacs-fullscreen-at-startup nil
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup nil
   dotspacemacs-undecorated-at-startup nil
   dotspacemacs-active-transparency 90
   dotspacemacs-inactive-transparency 90
   dotspacemacs-show-transient-state-title t
   dotspacemacs-show-transient-state-color-guide t
   dotspacemacs-mode-line-unicode-symbols t
   dotspacemacs-smooth-scrolling t
   dotspacemacs-line-numbers 'relative
   dotspacemacs-folding-method 'evil
   dotspacemacs-smartparens-strict-mode nil
   dotspacemacs-smart-closing-parenthesis t
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-enable-server nil
   dotspacemacs-server-socket-dir nil
   dotspacemacs-persistent-server nil
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")
   dotspacemacs-frame-title-format "%I@%S"
   dotspacemacs-icon-title-format nil
   dotspacemacs-whitespace-cleanup 'changed
   dotspacemacs-use-clean-aindent-mode t
   dotspacemacs-zone-out-when-idle nil
   dotspacemacs-pretty-docs nil))

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (spacemacs/load-spacemacs-env))

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."
  (setq-default git-magit-status-fullscreen t)
  (setq show-paren-style 'expression)
  (setq doom-modeline-height 15)
  (setq inhibit-compacting-font-caches t)
  (setq doom-modeline-project-detection 'project)
  (setq doom-modeline-buffer-file-name-style 'auto)
  (setq doom-modeline-project-detection 'project)
  (setq doom-modeline-project-detection 'ffip))
(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump."
  (require 'magit-todos)
  (require 'solaire-mode)
  (require 'semantic/db-file))

(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."
  (defun custom/ex-kill-all-buffers-and-close ()
    (interactive)
    (kill-matching-buffers ".*")
    (spacemacs/frame-killer))

  (defun custom/ex-force-kill-all-buffers-and-close ()
    (interactive)
    (spacemacs/kill-matching-buffers-rudely ".*")
    (spacemacs/frame-killer))

  (defun custom/ex-kill-buffer-and-close ()
    (interactive)
    (kill-this-buffer))

  (defun custom/ex-save-kill-buffer-and-close ()
    (interactive)
    (save-buffer)
    (kill-this-buffer))

  (evil-leader/set-key "qq" 'spacemacs/frame-killer)
  (evil-ex-define-cmd "q[uit!]" 'custom/ex-kill-buffer-and-close)
  (evil-ex-define-cmd "wq" 'custom/ex-save-kill-buffer-and-close)
  (evil-ex-define-cmd "qal[l]" 'custom/ex-force-kill-all-buffers-and-close)
  (evil-ex-define-cmd "qa" 'spacemacs/frame-killer)

  (global-company-mode t)
  (global-flycheck-mode t)
  (global-git-commit-mode t)
  (solaire-global-mode +1)
  (solaire-mode-swap-bg)
  ;(spacemacs/toggle-indent-guide-globally-on)
  (custom-set-faces
   '(company-tooltip-common
     ((t (:inherit company-tooltip :weight bold :underline nil))))
   '(company-tooltip-common-selection
     ((t (:inherit company-tooltip-selection :weight bold :underline nil))))))

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-want-Y-yank-to-eol nil)
 '(package-selected-packages
   '(hl-block-mode highlight-indent-guides package-lint ein polymode anaphora websocket mu4e-maildirs-extension mu4e-alert helm-mu shrink-path magit projectile org-plus-contrib flycheck lsp-mode anzu yaml-mode zenburn-theme zen-and-art-theme yasnippet-snippets yapfify xterm-color ws-butler writeroom-mode winum white-sand-theme which-key vterm volatile-highlights vi-tilde-fringe uuidgen use-package unfill underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme treemacs-projectile treemacs-persp treemacs-magit treemacs-evil toxi-theme toc-org terminal-here tao-theme tangotango-theme tango-plus-theme tango-2-theme symon symbol-overlay sunny-day-theme sublime-themes subatomic256-theme subatomic-theme string-inflection spaceline-all-the-icons spacegray-theme soothe-theme solarized-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme smeargle shell-pop seti-theme reverse-theme restart-emacs rebecca-theme rainbow-delimiters railscasts-theme pytest pyenv-mode py-isort purple-haze-theme professional-theme powershell popwin planet-theme pippel pipenv pip-requirements phoenix-dark-pink-theme phoenix-dark-mono-theme pcre2el password-generator paradox overseer orgit organic-green-theme org-projectile org-present org-pomodoro org-mime org-download org-cliplink org-bullets org-brain open-junk-file omtose-phellack-theme omnisharp oldlace-theme occidental-theme obsidian-theme noctilux-theme naquadah-theme nameless mwim mustang-theme multi-term move-text monokai-theme monochrome-theme molokai-theme moe-theme modus-vivendi-theme modus-operandi-theme mmm-mode minimal-theme material-theme markdown-toc majapahit-theme magit-svn magit-section magit-gitflow madhat2r-theme macrostep lush-theme lsp-ui lsp-python-ms lsp-haskell lorem-ipsum live-py-mode link-hint light-soap-theme kaolin-themes jbeans-theme jazz-theme ir-black-theme intero insert-shebang inkpot-theme importmagic hybrid-mode hungry-delete htmlize hlint-refactor hl-todo hindent highlight-parentheses highlight-numbers highlight-indentation heroku-theme hemisu-theme helm-xref helm-themes helm-swoop helm-pydoc helm-purpose helm-projectile helm-org-rifle helm-org helm-mode-manager helm-make helm-lsp helm-ls-git helm-hoogle helm-gitignore helm-git-grep helm-flx helm-descbinds helm-company helm-c-yasnippet helm-ag hc-zenburn-theme haskell-snippets gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme google-translate golden-ratio gnuplot gitignore-templates gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe+ gh-md gandalf-theme fuzzy fsharp-mode font-lock+ flyspell-correct-helm flycheck-pos-tip flycheck-package flycheck-haskell flycheck-elsa flycheck-bashate flx-ido flatui-theme flatland-theme fish-mode farmhouse-theme fancy-battery eziam-theme eyebrowse expand-region exotica-theme evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-textobj-line evil-surround evil-org evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-lion evil-indent-plus evil-iedit-state evil-goggles evil-exchange evil-escape evil-ediff evil-cleverparens evil-args evil-anzu eval-sexp-fu espresso-theme eshell-z eshell-prompt-extras esh-help elisp-slime-nav editorconfig dumb-jump dracula-theme dotenv-mode doom-themes doom-modeline django-theme diminish devdocs define-word darktooth-theme darkokai-theme darkmine-theme darkburn-theme dap-mode dante dakrone-theme cython-mode cyberpunk-theme company-statistics company-shell company-quickhelp company-lsp company-ghci company-ghc company-cabal company-box company-anaconda column-enforce-mode color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized cmm-mode clues-theme clean-aindent-mode chocolate-theme cherry-blossom-theme centered-cursor-mode busybee-theme bubbleberry-theme browse-at-remote blacken birds-of-paradise-plus-theme badwolf-theme auto-yasnippet auto-highlight-symbol auto-dictionary auto-compile attrap apropospriate-theme anti-zenburn-theme ample-zen-theme ample-theme alect-themes aggressive-indent afternoon-theme ace-link ace-jump-helm-line ac-ispell)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil))))
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))
)
