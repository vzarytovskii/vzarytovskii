(setq user-full-name "Vlad Zarytovskii"
      user-mail-address "vzaritovsky@hotmail.com")

(setq doom-font (font-spec :family "monospace" :size 14))
(setq doom-theme 'doom-one)
(setq org-directory "~/org/")
(setq display-line-numbers-type t)


(remove-hook 'post-self-insert-hook
             #'blink-paren-post-self-insert-function)
;; this still needs to be set for `blink-matching-open` to work
(setq blink-matching-paren 'show)

(setq show-paren-style 'parenthesis
      show-paren-delay 0.0
      show-paren-highlight-openparen t
      show-paren-when-point-inside-paren t
      show-paren-when-point-in-periphery t)
(show-paren-mode 1)


(use-package! helm-tramp
  :config
  (setq tramp-default-method "ssh")
  (setq make-backup-files nil)
  (setq create-lockfiles nil)
  (add-hook 'helm-tramp-pre-command-hook '(lambda () ;;(global-aggressive-indent-mode 0)
                                            (projectile-mode 0)
                                            ;;(editorconfig-mode 0)
                                            ))
  (add-hook 'helm-tramp-quit-hook '(lambda () ;;(global-aggressive-indent-mode 1)
                                     (projectile-mode 1)
                                     ;;(editorconfig-mode 1)
                                     ))
  )

(use-package! counsel
  :hook
  (after-init . ivy-mode)
  (counsel-grep-post-action . better-jumper-set-jump)
  :diminish ivy-mode
  :custom
  (company-idle-delay 0)
  (company-echo-delay 0)
  (company-minimum-prefix-length 2)
  :config
  (setq counsel-find-file-ignore-regexp "\\(?:^[#.]\\)\\|\\(?:[#~]$\\)\\|\\(?:^Icon?\\)"
        counsel-describe-function-function #'helpful-callable
        ncounsel-describe-variable-function #'helpful-variable
        counsel-rg-base-command "rg -S --no-heading --line-number --color never %s ."
        counsel-ag-base-command "ag -S --nocolor --nogroup %s"
        counsel-pt-base-command "pt -S --nocolor --nogroup -e %s"
        counsel-find-file-at-point t))

(use-package! ivy-rich
  :config
  (ivy-rich-mode 1)
  (setq ivy-format-function #'ivy-format-function-line))

(use-package company
  :diminish company-mode
  :defines (company-dabbrev-ignore-case company-dabbrev-downcase)
  :hook (after-init . global-company-mode)
  :bind (:map company-active-map
          ("C-n" . company-select-next)
          ("C-p" . company-select-previous)
          ("<tab>" . company-complete-common-or-cycle)
          ("RET" . company-complete-selection)
          ("<return>" . company-complete-selection)
          ("SPC" . company-complete-selection)
          :map company-search-map
          ("C-p" . company-select-previous)
          ("C-n" . company-select-next))
  :custom
  (company-idle-delay 0)
  (company-echo-delay 0)
  (company-minimum-prefix-length 2)
  (company-dabbrev-code-everywhere t)
  (company-dabbrev-code-modes t)
  (company-dabbrev-code-ignore-case t)
  (company-tooltip-align-annotations t)
  (company-transformers '(company-sort-by-occurrence
                          company-sort-by-backend-importance))
  (company-frontends
   '(company-tng-frontend
     company-pseudo-tooltip-frontend
     company-echo-metadata-frontend))
  :config
  (set-company-backend!
    '(c-mode c++-mode ess-mode fsharp-mode csharp-mode haskell-mode
             emacs-lisp-mode lisp-mode sh-mode python-mode rust-mode js-mode)
    '(:separate
      company-files
      company-yasnippet
      company-lsp
      company-tabnine))

  (setq +lsp-company-backend '(company-lsp :with company-tabnine :separate)))

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
  (advice-add #'company-tabnine :around #'my-company-tabnine))

(use-package company-posframe
  :hook (company-mode . company-posframe-mode))

(use-package company-box
  :diminish
  :hook (company-mode . company-box-mode)
  :init (setq company-box-icons-alist 'company-box-icons-all-the-icons)
  :config
  (setq company-box-backends-colors nil)
  (setq company-box-show-single-candidate t)
  (setq company-box-max-candidates 50)

  (defun company-box-icons--elisp (candidate)
    (when (derived-mode-p 'emacs-lisp-mode)
      (let ((sym (intern candidate)))
        (cond ((fboundp sym) 'Function)
              ((featurep sym) 'Module)
              ((facep sym) 'Color)
              ((boundp sym) 'Variable)
              ((symbolp sym) 'Text)
              (t . nil)))))

  (with-eval-after-load 'all-the-icons
    (declare-function all-the-icons-faicon 'all-the-icons)
    (declare-function all-the-icons-fileicon 'all-the-icons)
    (declare-function all-the-icons-material 'all-the-icons)
    (declare-function all-the-icons-octicon 'all-the-icons)
    (setq company-box-icons-all-the-icons
          `((Unknown . ,(all-the-icons-material "find_in_page" :height 0.7 :v-adjust -0.15))
            (Text . ,(all-the-icons-faicon "book" :height 0.68 :v-adjust -0.15))
            (Method . ,(all-the-icons-faicon "cube" :height 0.7 :v-adjust -0.05 :face 'font-lock-constant-face))
            (Function . ,(all-the-icons-faicon "cube" :height 0.7 :v-adjust -0.05 :face 'font-lock-constant-face))
            (Constructor . ,(all-the-icons-faicon "cube" :height 0.7 :v-adjust -0.05 :face 'font-lock-constant-face))
            (Field . ,(all-the-icons-faicon "tags" :height 0.65 :v-adjust -0.15 :face 'font-lock-warning-face))
            (Variable . ,(all-the-icons-faicon "tag" :height 0.7 :v-adjust -0.05 :face 'font-lock-warning-face))
            (Class . ,(all-the-icons-faicon "clone" :height 0.65 :v-adjust 0.01 :face 'font-lock-constant-face))
            (Interface . ,(all-the-icons-faicon "clone" :height 0.65 :v-adjust 0.01))
            (Module . ,(all-the-icons-octicon "package" :height 0.7 :v-adjust -0.15))
            (Property . ,(all-the-icons-octicon "package" :height 0.7 :v-adjust -0.05 :face 'font-lock-warning-face)) ;; Golang module
            (Unit . ,(all-the-icons-material "settings_system_daydream" :height 0.7 :v-adjust -0.15))
            (Value . ,(all-the-icons-material "format_align_right" :height 0.7 :v-adjust -0.15 :face 'font-lock-constant-face))
            (Enum . ,(all-the-icons-material "storage" :height 0.7 :v-adjust -0.15 :face 'all-the-icons-orange))
            (Keyword . ,(all-the-icons-material "filter_center_focus" :height 0.7 :v-adjust -0.15))
            (Snippet . ,(all-the-icons-faicon "code" :height 0.7 :v-adjust 0.02 :face 'font-lock-variable-name-face))
            (Color . ,(all-the-icons-material "palette" :height 0.7 :v-adjust -0.15))
            (File . ,(all-the-icons-faicon "file-o" :height 0.7 :v-adjust -0.05))
            (Reference . ,(all-the-icons-material "collections_bookmark" :height 0.7 :v-adjust -0.15))
            (Folder . ,(all-the-icons-octicon "file-directory" :height 0.7 :v-adjust -0.05))
            (EnumMember . ,(all-the-icons-material "format_align_right" :height 0.7 :v-adjust -0.15 :face 'all-the-icons-blueb))
            (Constant . ,(all-the-icons-faicon "tag" :height 0.7 :v-adjust -0.05))
            (Struct . ,(all-the-icons-faicon "clone" :height 0.65 :v-adjust 0.01 :face 'font-lock-constant-face))
            (Event . ,(all-the-icons-faicon "bolt" :height 0.7 :v-adjust -0.05 :face 'all-the-icons-orange))
            (Operator . ,(all-the-icons-fileicon "typedoc" :height 0.65 :v-adjust 0.05))
            (TypeParameter . ,(all-the-icons-faicon "hashtag" :height 0.65 :v-adjust 0.07 :face 'font-lock-const-face))
            (Template . ,(all-the-icons-faicon "code" :height 0.7 :v-adjust 0.02 :face 'font-lock-variable-name-face))))))
;; Show quick tooltip

(use-package company-quickhelp
  :defines company-quickhelp-delay
  :bind (:map company-active-map
          ("M-h" . company-quickhelp-manual-begin))
  :hook (global-company-mode . company-quickhelp-mode)
  :custom (company-quickhelp-delay 0.1)
  :config
  (company-quickhelp-mode 1))

(use-package! maple-iedit
  :commands (maple-iedit-match-all maple-iedit-match-next maple-iedit-match-previous)
  :config
  (delete-selection-mode t)
  (setq maple-iedit-ignore-case t)
  (defhydra maple/iedit ()
    ("n" maple-iedit-match-next "next")
    ("t" maple-iedit-skip-and-match-next "skip and next")
    ("T" maple-iedit-skip-and-match-previous "skip and previous")
    ("p" maple-iedit-match-previous "prev"))
  :bind (:map maple-iedit-mode-keymap
          ("C-n" . maple-iedit-match-next)
          ("C-m" . maple-iedit-match-previous)
          ("C-," . maple-iedit-skip-and-match-next)
          ("C-." . maple-iedit-skip-and-match-previous)
          )
  )

(use-package! maple-explorer
  :commands (maple-explorer-file maple-explorer-buffer maple-explorer-imenu maple-explorer-recentf)
  :config
  (setq maple-explorer-file-display-alist '((side . left) (slot . -1)))
  )

(use-package! imenu-list
  :config
  (setq imenu-list-auto-resize t)
  (setq imenu-list-focus-after-activation t)
  (setq imenu-list-after-jump-hook nil)
  (add-hook 'imenu-list-after-jump-hook #'recenter-top-bottom)
  )

(use-package! rainbow-delimiters
  :config
  (custom-set-faces
   '(rainbow-delimiters-mismatched-face ((t (:foreground "white" :background "red" :weight bold))))
   '(rainbow-delimiters-unmatched-face ((t (:foreground "white" :background "red" :weight bold))))

   ;; show parents (in case of rainbow failing !)
   '(show-paren-match ((t (:foreground "white" :background "blue" :weight bold))))
   '(show-paren-mismatch ((t (:foreground "white" :background "red" :weight bold)))))
  ;  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  )

;; highlight brackets
(setq show-paren-style 'parenthesis)
(use-package! dimmer
  :config (dimmer-mode))

(use-package! beacon
  :diminish
  :config (setq beacon-color "#666600")
  :hook   ((org-mode text-mode) . beacon-mode))

(use-package! golden-ratio
  :disabled
  :diminish golden-ratio-mode
  :init (golden-ratio-mode 1))

(use-package! awesome-pair)
(use-package! lsp-haskell
  :config
  (setq lsp-haskell-process-path-hie "ghcide")
  (setq lsp-haskell-process-args-hie '())
  (add-hook 'haskell-mode-hook #'lsp))
