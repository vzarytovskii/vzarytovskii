;;; programming.el --- Programming-related packages configuration (such as editorconfig, code folding, lsp, etc.).  -*- lexical-binding: t; -*-
;;; Commentary:
;; Core packages config, for main config, see config.el

;; General config
(use-package eldoc
  :straight nil
  :delight)

;;; Code:

(use-package editorconfig
  :delight
  :config
  (editorconfig-mode 1))

(use-package xref
  :ensure nil
  :bind (;;("C-." . xref-find-definitions)
         ("C-;" . 'counsel-imenu)))

(use-package imenu-anywhere
  :bind ("C-." . imenu-anywhere))

(use-package dumb-jump
  :commands (dumb-jump-xref-activate)
  :preface
  (defun override-dumb-jump-prompt-user-for-choice (proj results)
    (let ((choices (--map (dumb-jump--format-result proj it) results)))
      (funcall dumb-jump-ivy-jump-to-selected-function results choices proj)))
  :hook (xref-backend-functions . dumb-jump-xref-activate)
  :config
  (advice-add 'dumb-jump-prompt-user-for-choice :override #'override-dumb-jump-prompt-user-for-choice))

(use-package smart-jump
  ;; TODO: Use quickpeek for smart-jump-keep.
  ;; Package wasn't updated for a long time, xref should be used instead.
  :disabled t
  :after dumb-jump
  :bind (("M-." . smart-jump-go)
         ("M-," . smart-jump-back)
         ("M-/" . smart-jump-references)
         ("M-?" . smart-jump-references))
  :custom (dumb-jump-selector 'ivy)
  :config
  (smart-jump-setup-default-registers)
  (smart-jump-register :modes 'lsp-ui-mode
                       :jump-fn 'lsp-ui-peek-find-definitions
                       :refs-fn 'lsp-ui-peek-find-references
                       :pop-fn 'pop-tag-mark
                       :should-jump t
                       :heuristic 'point
                       :async 500)
  (smart-jump-register :modes '(emacs-lisp-mode lisp-interaction-mode)
                       :jump-fn 'elisp-slime-nav-find-elisp-thing-at-point
                       :pop-fn 'pop-tag-mark
                       :should-jump t
                       :heuristic 'error
                       :async nil))

;; Flymake and flymake configs:

(use-package flycheck
  :delight
  :defer t
  :hook (after-init-hook . global-flycheck-mode)
  :config
  (setq flycheck-highlighting-mode 'sexps))

;; TODO: Temporary disable inline on-demand error display.
(use-package flycheck-inline
  :disabled t
  :after flycheck
  :preface

  (setq flycheck-ov (make-overlay 0 0))
  (overlay-put flycheck-ov 'invisible t)
  (overlay-put flycheck-ov 'face '(:extend t :foreground "gray50" :background "#ff0000"))

  (defun fc-inline-overlay (msg &optional pos err)
    ;; MSG - message from flycheck
    ;; POS - position marker
    ;; ERR - flycheck-error object, other (including level, id, message, filename) can be extracted.

    ;; TODO define face here dependung on level/severity.
    ;; TODO: Clear overlay when leaving the line.
    (let ((beg (point-at-eol))
          (end (+ 1 (point-at-eol))))
      ;; (end (min (point-max) (+ 1 (point-at-eol)))))
      (move-overlay flycheck-ov beg end)
      (overlay-put flycheck-ov 'after-string (format "\t//\s%s\n" msg))))
  ;;(ov-set (ov-line) 'after-string (propertize (format "%s" msg) 'face '(:extend t :foreground "gray50")) 'ovfc t)

  (defun fc-inline-overlay-clear ()
    (overlay-put flycheck-ov 'invisible t))

  :config
  (setq flycheck-inline-display-function 'fc-inline-overlay
        flycheck-inline-clear-function 'fc-inline-overlay-clear)
  :hook (flycheck-mode-hook . flycheck-inline-mode))

;; Company mode config:
(use-package company
  :delight
  :straight (:host github :repo "company-mode/company-mode" :branch "master")
  :hook (after-init-hook . global-company-mode)
  :bind (:map company-active-map
              ("C-w" . 'backward-kill-word))
  :config
  (setq company-minimum-prefix-length 2
        company-idle-delay (lambda ()
                             (if (company-in-string-or-comment) nil 0.1))
        company-require-match nil
        company-frontends
        '(company-pseudo-tooltip-unless-just-one-frontend-with-delay
          company-preview-frontend
          company-echo-metadata-frontend)
        company-tooltip-align-annotations t
        company-tooltip-limit 20
        company-tooltip-offset-display 'lines
        company-tooltip-flip-when-above t
        company-text-icons-add-background t
        company-show-quick-access 'right)

  (push 'company-capf company-backends))

(use-package company-quickhelp
  :after company
  :hook (global-company-mode-hook . company-quickhelp-mode)
  :config
  (setq pos-tip-use-relative-coordinates t
        company-quickhelp-delay 0.0))

(use-package company-box
  :disabled t
  :delight
  :after (:all all-the-icons company)
  :functions (my-company-box--make-line
              my-company-box-icons--elisp)
  :commands (company-box--get-color
             company-box--resolve-colors
             company-box--add-icon
             company-box--apply-color
             company-box--make-line
             company-box-icons--elisp)
  :hook (company-mode-hook . company-box-mode)
  :custom
  (company-box-backends-colors nil)
  (company-box-show-single-candidate t)
  (company-box-max-candidates 50)
  (company-box-doc-delay 0.3))

(use-package company-prescient
  :after (:all company prescient))

(use-package company-posframe
  :disabled t
  :delight
  :if (and (>= emacs-major-version 26)
           (display-graphic-p))
  :after (:all all-the-icons company)
  :config
  (company-posframe-mode 1))

(use-package copilot
  :after company
  :disabled t
  :straight (:host github :repo "zerolfx/copilot.el"
                   :files ("dist" "copilot.el"))
  :hook (prog-mode-hook . copilot-mode)
  :config
  (defun my-tab ()
    (interactive)
    (or (copilot-accept-completion)
        (company-indent-or-complete-common nil)))

  (delq 'company-preview-if-just-one-frontend company-frontends)
  (define-key company-mode-map (kbd "<tab>") 'my-tab)
  (define-key company-mode-map (kbd "TAB") 'my-tab)
  (define-key company-active-map (kbd "<tab>") 'my-tab)
  (define-key company-active-map (kbd "TAB") 'my-tab))

;; Snippets config
(use-package yasnippet
  :defer t
  :after company
  :delight yas-minor-mode
  :hook (prog-mode-hook . yas-global-mode)
  :config
  (defvar company-mode/enable-yas t
    "Enable yasnippet for all backends.")

  (defun company-mode/backend-with-yas (backend)
    (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))

  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

  (setq yas-snippet-dirs '("~/.emacs.d/snippets")))

(use-package yasnippet-snippets
  :defer t
  :after yasnippet)

;; Tree-sitter config
(use-package tsc
  :defer t
  :straight (tsc :host github :repo "ubolonton/emacs-tree-sitter" :files ("core/*.el")))

(use-package tree-sitter
  :defer t
  :if (executable-find "tree-sitter")
  :straight (tree-sitter :type git :host github :repo "ubolonton/emacs-tree-sitter" :files ("lisp/*.el"))
  :custom-face
  (tree-sitter-hl-face:method.call   ((t (:inherit font-lock-function-name-face))))
  (tree-sitter-hl-face:function.call ((t (:inherit font-lock-function-name-face))))
  (tree-sitter-hl-face:operator      ((t (:inherit default))))
  (tree-sitter-hl-face:type.builtin  ((t (:inherit font-lock-type-face))))
  (tree-sitter-hl-face:number        ((t (:inherit highlight-numbers-number))))
  :hook (((python-mode-hook
           typescript-mode-hook) . tree-sitter-mode)
         ((tree-sitter-after-on-hook
           python-mode-hook
           typescript-mode-hook) . tree-sitter-hl-mode))
  :config
  (add-to-list 'tree-sitter-major-mode-language-alist
               '(rustic-mode . rust)))

(use-package tree-sitter-langs
  :defer t
  :if (executable-find "tree-sitter")
  :straight (tree-sitter-langs :type git :host github :repo "ubolonton/emacs-tree-sitter" :files ("langs/*.el" "langs/queries"))
  :after tree-sitter)

;; LSP Configuration:

(use-package lsp-mode
  :delight
  :commands (lsp lsp-deferred)
  :config
  (setq max-specpdl-size 32000) ;; A workaround when running gccemacs in WSL
  (setq lsp-auto-guess-root nil
        lsp-debounce-full-sync-notifications-interval 1.0
        lsp-diagnostic-package :flycheck
        lsp-diagnostics-attributes '((deprecated :strike-through t))
        lsp-document-sync-method 'incremental
        lsp-eldoc-enable-hover nil
        lsp-eldoc-enable-signature-help nil
        lsp-enable-file-watchers t
        lsp-enable-folding t
        lsp-enable-indentation t
        lsp-enable-on-type-formatting t
        lsp-enable-snippet nil
        lsp-enable-symbol-highlighting t
        lsp-enable-xref t
        lsp-flycheck-live-reporting t
        lsp-headerline-breadcrumbs-mode nil
        lsp-idle-delay 1
        lsp-keep-workspace-alive nil
        lsp-lens-debounce-interval 1.5
        lsp-navigation 'both
        lsp-prefer-capf t
        lsp-prefer-flymake nil
        lsp-response-timeout 10
        lsp-semantic-highlighting t
        lsp-session-file "~/.lsp-sessions"
        lsp-signature-auto-activate nil
        lsp-signature-render-all nil
        lsp-headerline-breadcrumb-segments '(file symbols)))

(use-package lsp-treemacs :after lsp)

(use-package lsp-ui
  :delight
  :after lsp-mode
  :hook (lsp-after-open-hook . lsp-ui-mode)
  :hook (lsp-after-open-hook . lsp-lens-mode)
  :hook (lsp-after-open-hook . lsp-signature-mode)
  :hook (lsp-after-open-hook . lsp-ui-sideline-mode)
  :hook (lsp-after-open-hook . lsp-headerline-breadcrumb-mode)
  :bind (:map lsp-ui-mode-map
              ;; TODO: move to remap instead of specifying keys,
              ("C-;" . lsp-ui-imenu)
              ("C-." . lsp-ui-imenu)
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references))
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-header nil
        lsp-ui-doc-border "green"
        lsp-ui-doc-max-height 50
        lsp-ui-doc-max-width 150
        lsp-ui-doc-include-signature t
        lsp-ui-doc-delay 0.0
        lsp-ui-doc-position 'at-point
        lsp-ui-doc-use-childframe t
        lsp-ui-flycheck-enable t
        lsp-ui-flycheck-list-position 'right
        lsp-ui-peek-enable t
        lsp-ui-peek-fontify 'always
        lsp-ui-peek-peek-height 30
        lsp-ui-peek-list-width 60
        lsp-ui-sideline-enable t
        lsp-ui-sideline-update-mode 'line
        lsp-ui-sideline-show-symbol nil
        lsp-ui-sideline-show-diagnostics nil
        lsp-ui-sideline-show-hover t
        lsp-ui-sideline-show-code-actions nil
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-sideline-delay 0.0
        lsp-ui-imenu-enable t
        lsp-ui-imenu-kind-position 'top))

(use-package ruled-switch-buffer
  :straight (:host github :repo "kzkn/ruled-switch-buffer" :branch "main")
  :config
  (ruled-switch-buffer-define fs-to-fsi
    :matcher (lambda (fn) (string-match ".fs$" fn))
    :mappers (lambda (fn) (replace-regexp-in-string "\\.fs$" ".fsi" fn)))
  (ruled-switch-buffer-define fsi-to-fs
    :matcher (lambda (fn) (string-match ".fsi$" fn))
    :mappers (lambda (fn) (replace-regexp-in-string "\\.fsi$" ".fs" fn))));; Language-specific configs:
;; .NET
(use-package dotnet
  :config
  (setenv "DOTNET_USE_POLLING_FILE_WATCHER" "true"))

(use-package sharper
  :straight (:host github :repo "sebasmonia/sharper" :branch "master")
  :bind ("C-x n" . sharper-main-transient))

(use-package fsharp-mode
  ;; :straight (:host github :repo "vzarytovskii/emacs-fsharp-mode" :branch "master")
  :after (:all sharper dotnet lsp-mode)
  :commands fsharp-mode
  :hook (fsharp-mode-hook . lsp-deferred)
  :hook (fsharp-mode-hook . dotnet-mode)
  :config
  (setq indent-tabs-mode nil
        truncate-lines t
        tab-width 4)
  (setq fsharp-doc-idle-delay 0.0
        fsharp-ac-use-popup t
        fsharp-ac-intellisense-enabled t
        fsharp-smart-indentation t
        fsharp-indent-offset 4
        inferior-fsharp-program "dotnet fsi"
        lsp-fsharp-server-runtime 'net-core
        ;;lsp-fsharp-server-install-dir "~/code/fsautocomplete/bin/release_netcore"
        lsp-fsharp-server-args '("--adaptive-lsp-server-enabled")
        lsp-fsharp-keywords-autocomplete t
        lsp-fsharp-external-autocomplete t
        lsp-fsharp-linter t
        lrsp-fsharp-union-case-stub-generation t
        lsp-fsharp-union-case-stub-generation-body "failwith \"TODO\""
        lsp-fsharp-record-stub-generation t
        lsp-fsharp-record-stub-generation-body "failwith \"TODO\""
        lsp-fsharp-interface-stub-generation t
        lsp-fsharp-interface-stub-generation-object-identifier "_"
        lsp-fsharp-interface-stub-generation-method-body "failwith \"TODO\""
        lsp-fsharp-unused-opens-analyzer t
        lsp-fsharp-unused-declarations-analyzer t
        lsp-fsharp-simplify-name-analyzer t
        lsp-fsharp-resolve-namespaces t
        lsp-fsharp-enable-reference-code-lens t
        lsp-fsharp-auto-workspace-init t
        lsp-fsharp-exclude-directories ["paket-files" ".git" "packages" "node_modules"]
        lsp-log-io t)
  (add-to-list 'company-transformers 'company-sort-prefer-same-case-prefix)
  (setq indent-region-function '(lambda (start end &optional indent-offset))))

;; Rust
(use-package rustic
  :config
  (setq rustic-lsp-server 'rust-analyzer
        rustic-lsp-client 'lsp-mode))

;; YAML
(use-package yaml-mode)

(use-package flymake-yamllint
  :straight (:host github :repo "shaohme/flymake-yamllint" :branch "main")
  :after (:all yaml-mode flymake)
  :hook (yaml-mode-hook . flymake-yamllint-setup))

;; JSON
(use-package json-mode
  :mode "\\.json\\'")

(use-package json-reformat
  :after json-mode)

(use-package json-snatcher
  :after json-mode)

;; Docker
(use-package docker
  :if (executable-find "docker"))

(use-package dockerfile-mode
  :config
  (setq dockerfile-mode-command "docker"))

(use-package docker-compose-mode
  :if (executable-find "docker-compose"))

;; Projectile
(use-package projectile
  :delight
  :hook (after-init-hook . projectile-mode)
  :bind ("C-c C-p" . 'projectile-command-map)
  :bind (:map projectile-command-map
              ("C-x C-f" . 'projectile-find-file)
              ("C-x f" . 'find-file))
  :preface
  (defvar +project/lsp-project-root-cache (make-hash-table :test 'equal)
    "Cached value of function `+project/lsp-project-root`.")

  (defun +project/lsp-project-root (&optional dir)
    (let* ((dir (or dir default-directory))
           (cache-key dir)
           (cache-value (gethash cache-key +project/lsp-project-root-cache)))
      (if (and cache-value (file-exists-p cache-value))
          cache-value
        (let* ((lsp-folders (lsp-session-folders (lsp-session)))
               (value (cl-find-if
                       (lambda (path)
                         (and
                          ;; fast filter to improve `ivy-rich-switch-buffer-root' performance, but not accurate
                          (string-prefix-p path (expand-file-name dir))
                          ;; double check if current dir in the lsp-project roots
                          (file-in-directory-p dir path)))
                       lsp-folders)))
          (puthash cache-key value +project/lsp-project-root-cache)
          value))))

  (defalias '+project/root 'projectile-project-root)

  (defun +project/projectile-buffer-filter (buffer)
    (let ((name (buffer-name buffer)))
      (or (and (string-prefix-p "*" name)
               (not (string-prefix-p "*eww*" name))
               (not (string-prefix-p "*ein: http" name))
               (not (string-prefix-p "*ein:notebooklist" name))
               (not (string-prefix-p "*vterm:" name))
               (not (string-prefix-p "*cider" name)))
          (string-match-p "magit.*:" name)
          (equal (buffer-name (current-buffer)) name))))

  (defun +project/projectile-buffer-filter-function (buffers)
    (cl-remove-if
     (lambda (buffer) (+project/projectile-buffer-filter buffer))
     buffers))

  :config
  (setq projectile-project-search-path '("~/code/")
        projectile-auto-discover t
        projectile-enable-caching t
        projectile-indexing-method 'alien
        projectile-globally-ignored-file-suffixes '("#" "~" ".swp" ".o" ".so" ".exe" ".dll" ".elc" ".pyc" ".jar")
        projectile-globally-ignored-directories '(".git" "node_modules" "__pycache__" ".vs")
        projectile-globally-ignored-files '("TAGS" "tags" ".DS_Store")
        projectile-completion-system 'ivy)

  (add-to-list 'projectile-project-root-files-functions #'+project/lsp-project-root))

(use-package counsel-projectile
  :after (:all counsel projectile)
  :bind ("C-x C-p" . counsel-projectile)
  ;; :bind ("C-x f" . counsel-projectile-find-file)
  :bind ("C-x s". counsel-projectile-rg))

;; Misc programming-related (i.e. tramp, devcontainers, etc)
(use-package codespaces
  :ensure-system-package gh
  :config (codespaces-setup))

(provide 'programming)
;;; programming.el ends here
