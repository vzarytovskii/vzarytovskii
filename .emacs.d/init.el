;;; -*- lexical-binding: t; -*- no-byte-compile: t; -*-

;;; Commentary:

;;; Code:

;; Setup package system

(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
      (build (expand-file-name "elpaca/" elpaca-builds-directory))
      (order (cdr elpaca-order))
      (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                ,@(when-let* ((depth (plist-get order :depth)))
                                                    (list (format "--depth=%d" depth) "--no-single-branch"))
                                                ,(plist-get order :repo) ,repo))))
                ((zerop (call-process "git" nil buffer t "checkout"
                                      (or (plist-get order :ref) "--"))))
                (emacs (concat invocation-directory invocation-name))
                ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                      "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                ((require 'elpaca))
                ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("gnu-devel" . "https://elpa.gnu.org/devel/") t)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)

(setq package-quickstart t
      gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"
      package-archive-priorities
      '(("melpa" .  500)
        ("melpa-stable" . 400)
        ("elpa" . 300)
        ("org" . 200)
        ("gnu-devel" . 100)
        ("gnu" . 50)))

;; Initialise the packages, avoiding a re-initialisation.
(unless (bound-and-true-p package--initialized)
  (package-initialize))

(setq use-package-vc-prefer-newest t
      package-install-upgrade-built-in t  ; Always upgrade built-in packages
      load-prefer-newer t                 ; Always load newer compiled files
      ad-redefinition-action 'accept)     ; Silence advice redefinition warnings

(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))

(setq elpaca-ignored-dependencies
      (delq 'transient elpaca-ignored-dependencies))

;; Setup use-package
(setq-default use-package-always-demand t
              use-package-always-defer t
              use-package-always-ensure t
              use-package-expand-minimally nil
              use-package-enable-imenu-support t
              use-package-compute-statistics t
              use-package-hook-name-suffix nil)

(setq gnutls-verify-error (not (getenv "INSECURE"))
      tls-checktrust gnutls-verify-error
      tls-program (list "gnutls-cli --x509cafile %t -p %p %h"
                        ;; compatibility fallbacks
                        "gnutls-cli -p %p %h"
                        "openssl s_client -connect %h:%p -no_ssl2 -no_ssl3 -ign_eof"))

(defun reload-init-file ()
    (interactive)
    (load-file user-init-file))

(defun emacs-recompile ()
  "Recompile all .elc files."
  (interactive)
  (message "Recompiling ...")
  (if (functionp 'async-byte-recompile-directory)
      (async-byte-recompile-directory package-user-dir)
    (byte-recompile-directory package-user-dir 0 'force)))

(defconst *sys/gui* (display-graphic-p))
(defconst *sys/is-mac* (eq system-type 'darwin))
(defconst *sys/is-linux* (or (eq system-type 'gnu/linux) (eq system-type 'linux)))
(defconst *sys/is-unix* (or *sys/is-linux* (eq system-type 'usg-unix-v) (eq system-type 'berkeley-unix)))
(defconst *sys/is-windows* (or (eq system-type 'ms-dos) (eq system-type 'windows-nt)))
(defconst *sys/is-cygwin* (eq system-type 'cygwin))
(defconst *sys/is-wsl* (and *sys/is-linux* (getenv "WSLENV")))

(when *sys/is-wsl*
  (setq browse-url-generic-program  "/mnt/c/Windows/System32/cmd.exe"
        browse-url-generic-args     '("/c" "start")
        browse-url-browser-function #'browse-url-generic))

(use-package auto-compile
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

(use-package esup)

(use-package llama)
(use-package closql)
(use-package emacsql)

(use-package hydra)

(use-package exec-path-from-shell
  :defer nil
  :when (eq system-type 'darwin)
  :hook (after-init-hook . exec-path-from-shell-initialize))

(use-package delight :defer t)

(use-package transient :defer t)

;; Configure Emacs' defaults and keybinds;
(use-package emacs
  :ensure nil
  :delight auto-revert-mode
  :bind (("C-z"             . nil)
         ("C-x C-z"         . nil)
         ("C-h h"           . nil)
         ("<C-backspace>"   . nil)
         ([delete]          . 'delete-forward-char)
         ("C-x C-2"         . 'vsplit-last-buffer)
         ("C-x C-2"         . 'vsplit-current-buffer)
         ("C-x 3"           . 'hsplit-last-buffer)
         ("C-x C-3"         . 'hsplit-current-buffer)
         ("C-x |"           . 'toggle-window-split)
         ("C-w"             . 'backward-kill-word)
         ("M-w"             . 'copy-region-or-line)
         ("C-g"             . 'keyboard-quit)
         ;; ("C-k"          . 'kill-buffer)
         ;; ("C-K"          . 'kill-this-buffer)
         ("C-c o"           . 'switch-to-minibuffer)
         ([remap keyboard-quit] . 'keyboard-quit-ex))
  :hook (after-init-hook . window-divider-mode)
  :delight lisp-interaction-mode
  :preface
  (defun keyboard-quit-ex ()
    (interactive)
    (cond
    ((region-active-p)
      (keyboard-quit))
    ((derived-mode-p 'completion-list-mode)
      (delete-completion-window))
    ((> (minibuffer-depth) 0)
      (abort-recursive-edit))
    (t
      (keyboard-quit))))
  (defun switch-to-minibuffer ()
    "Switch to minibuffer window."
    (interactive)
    (if (active-minibuffer-window)
        (select-window (active-minibuffer-window))
      (error "Minibuffer is not active")))

  (defun kill-this-buffer ()
    "Kill the current buffer."
    (interactive)
    (kill-buffer nil))

  (defun flash-mode-line ()
    (invert-face 'mode-line)
    (run-with-timer 0.1 nil #'invert-face 'mode-line))

  (defun vsplit-last-buffer ()
    "Split the window vertically and display the previous buffer."
    (interactive)
    (split-window-vertically)
    (other-window 1 nil)
    (switch-to-next-buffer))

  (defun vsplit-current-buffer ()
    "Split the window vertically and display the current buffer."
    (interactive)
    (split-window-vertically)
    (other-window 1 nil))

  (defun hsplit-last-buffer ()
    "Split the window horizontally and display the previous buffer."
    (interactive)
    (split-window-horizontally)
    (other-window 1 nil)
    (switch-to-next-buffer))

  (defun hsplit-current-buffer ()
    "Split the window horizontally and display the current buffer."
    (interactive)
    (split-window-horizontally)
    (other-window 1 nil))

  (defun toggle-window-split ()
    (interactive)
    (if (= (count-windows) 2)
        (let* ((this-win-buffer (window-buffer))
               (next-win-buffer (window-buffer (next-window)))
               (this-win-edges (window-edges (selected-window)))
               (next-win-edges (window-edges (next-window)))
               (this-win-2nd (not (and (<= (car this-win-edges)
                                           (car next-win-edges))
                                       (<= (cadr this-win-edges)
                                           (cadr next-win-edges)))))
               (splitter
                (if (= (car this-win-edges)
                       (car (window-edges (next-window))))
                    'split-window-horizontally
                  'split-window-vertically)))
          (delete-other-windows)
          (let ((first-win (selected-window)))
            (funcall splitter)
            (if this-win-2nd (other-window 1))
            (set-window-buffer (selected-window) this-win-buffer)
            (set-window-buffer (next-window) next-win-buffer)
            (select-window first-win)
            (if this-win-2nd (other-window 1))))))

  (defun copy-region-or-line (beg end)
    (interactive "r")
    (if mark-active
        (kill-ring-save beg end)
      (kill-ring-save (line-beginning-position) (line-end-position))))
  :custom
  (enable-recursive-minibuffers t)
  (read-extended-command-predicate #'command-completion-default-include-p)
  :config
  ;; Defaults

  (setq-default major-mode 'text-mode
                use-file-dialog nil
                use-dialog-box nil
                cursor-type 'box
                x-stretch-cursor t
                cursor-in-non-selected-window nil
                indent-tabs-mode nil
                tab-width 4
                select-enable-primary t
                select-enable-clipboard t)

  (fset 'yes-or-no-p 'y-or-n-p)

  (setq scroll-margin 0
        scroll-step 1
        scroll-conservatively 100000
        scroll-preserve-screen-position 1
        scroll-preserve-screen-position 'always
        hscroll-step 2
        hscroll-margin 2

        next-line-add-newlines nil

        byte-compile-warnings '(cl-functions)
        visible-bell nil
        ring-bell-function 'flash-mode-line

        tab-width 4

        display-raw-bytes-as-hex t
        redisplay-skip-fontification-on-input t

        window-divider-default-right-width 1
        window-divider-default-bottom-width 1
        window-divider-default-places 'right-only

        blink-cursor-mode nil

        show-trailing-whitespace t
        whitespace-style '(face trailing)
        make-backup-files t
        backup-directory-alist '(("." . "~/.saves"))
        auto-save-default nil
        track-eol nil
        line-move-visual nil
        kill-whole-line t
        truncate-lines t
        show-paren-style 'parenthesis
        frame-resize-pixelwise t
        use-short-answers t
        initial-buffer-choice (expand-file-name "~")))

(use-package dired
  :ensure nil
  :config
  (setq dired-dwim-target t
        ;; dired-listing-switches "-l --almost-all --human-readable --group-directories-first --no-group"
        dired-recursive-copies 'always
        dired-recursive-deletes 'top
        dired-create-destination-dirs 'ask)
  (when (eq system-type 'darwin)
    (let ((gls (executable-find "gls")))
      (when gls
        (setq dired-use-ls-dired t
              insert-directory-program gls
              dired-listing-switches "-aBhl  --group-directories-first"))))
  (put 'dired-find-alternate-file 'disabled nil)
  :hook ((dired-after-readin-hook . hl-line-mode)))

(use-package dired-x :ensure nil :after dired)

(use-package dirvish
  :init
  (dirvish-override-dired-mode)
  :config
  (setq dirvish-mode-line-format '(:left (sort symlink) :right (omit yank index))
        dirvish-attributes '(vc-state subtree-state collapse file-time file-size)
        dirvish-side-attributes '(vc-state collapse file-size)
        dirvish-large-directory-threshold 20000)
   :bind ; Bind `dirvish-fd|dirvish-side|dirvish-dwim' as you see fit
   (("C-x d" . dirvish)
    ("C-c f" . dirvish)
    :map dirvish-mode-map               ; Dirvish inherits `dired-mode-map'
    ("/"   . dirvish-fd)
    (";"   . dired-up-directory)        ; So you can adjust `dired' bindings here
    ("?"   . dirvish-dispatch)          ; [?] a helpful cheatsheet
    ("a"   . dirvish-setup-menu)        ; [a]ttributes settings:`t' toggles mtime, `f' toggles fullframe, etc.
    ("f"   . dirvish-file-info-menu)    ; [f]ile info
    ("o"   . dirvish-quick-access)      ; [o]pen `dirvish-quick-access-entries'
    ("s"   . dirvish-quicksort)         ; [s]ort flie list
    ("r"   . dirvish-history-jump)      ; [r]ecent visited
    ("l"   . dirvish-ls-switches-menu)  ; [l]s command flags
    ("v"   . dirvish-vc-menu)           ; [v]ersion control commands
    ("*"   . dirvish-mark-menu)
    ("y"   . dirvish-yank-menu)
    ("N"   . dirvish-narrow)
    ("^"   . dirvish-history-last)
    ("TAB" . dirvish-subtree-toggle)
    ("M-f" . dirvish-history-go-forward)
    ("M-b" . dirvish-history-go-backward)
    ("M-e" . dirvish-emerge-menu))
  :after dired)

(use-package ibuffer
  :ensure nil
  :custom
  (ibuffer-saved-filter-groups
    (quote (("default"
             ("Org" (or
                     (mode . org-mode)
                     (name . "^\\*Org Src")
                     (name . "^\\*Org Agenda\\*$")))
             ("C" (or
                   (mode . c-mode)))
             ("LISP" (or
                      (mode . emacs-lisp-mode)
                      (mode . ielm-mode)
                      (name . "^\\*scratch\\*$")
                      (mode . lisp-mode)))
             ("Dired" (or
                        (mode . dired-mode)
                        (mod . dirvish-mode)))
             ("Term" (or
                      (mode . term-mode)
                      (mode . eshell-mode)))))))
  :bind
  (("C-x C-b" . ibuffer))
  :hook ((ibuffer-mode-hook . (lambda () (ibuffer-switch-to-saved-filter-groups "default")))))

(use-package doom-two-tone-themes
  :ensure '(doom-two-tone-themes
    :host github
    :repo "eliraz-refael/doom-two-tone-themes"
    :branch "master"
    :files ("doom-two-tone-themes.el" "themes/*.el")
    :main "doom-two-tone-themes.el"))

(use-package auto-dark
  :ensure '(auto-dark :type git :host github :repo "LionyxML/auto-dark-emacs" :ref "478d10238a85cdda72ffbb529fc78d8a5a4322ff")
  :delight auto-dark-mode
  :hook (after-init-hook . auto-dark-mode)
  :init (auto-dark-mode)
  :config
  (setq auto-dark-allow-osascript t)
  (setq auto-dark-themes '((doom-silver-slate) (doom-slate-mushroom))))

(use-package hl-line
  :ensure nil
  :when (display-graphic-p)
  :hook (after-init-hook . global-hl-line-mode))

(use-package vertico
  :init
  (vertico-mode)
  (vertico-multiform-mode)
  (setq vertico-multiform-commands
        '(
          (consult-imenu buffer indexed)
          (consult-grep buffer indexed)
          (execute-extended-command flat)))
  (setq vertico-multiform-categories
      '((file grid)
        (consult-grep buffer))))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil)
        completion-category-overrides '((file (styles partial-completion))))

(use-package consult
  :bind (("M-s"   . consult-ripgrep))
  :hook (completion-list-mode-hook . consult-preview-at-point-mode)
  :init
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

(use-package consult-gh
  :ensure `(consult-gh :type git :host github :repo "armindarvish/consult-gh")
  :after consult)

(use-package consult-gh-forge
  :after consult-gh
  :config
  (require 'consult-gh-transient)
  (consult-gh-forge-mode +1))

;; ---

(use-package treesit
  :ensure nil
  :preface
  (defun ts-install-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    (dolist (grammar
             '(
                (bash "https://github.com/tree-sitter/tree-sitter-bash")
                (cmake "https://github.com/uyha/tree-sitter-cmake")
                (css "https://github.com/tree-sitter/tree-sitter-css")
                (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
                (elisp "https://github.com/Wilfred/tree-sitter-elisp")
                (html "https://github.com/tree-sitter/tree-sitter-html")
                (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
                (json "https://github.com/tree-sitter/tree-sitter-json")
                (make "https://github.com/alemuller/tree-sitter-make")
                (python "https://github.com/tree-sitter/tree-sitter-python")
                (rust "https://github.com/tree-sitter/tree-sitter-rust")
                (toml "https://github.com/tree-sitter/tree-sitter-toml")
                (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
                (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
                (yaml "https://github.com/ikatyang/tree-sitter-yaml")))
            (add-to-list 'treesit-language-source-alist grammar)
      (unless (treesit-language-available-p (car grammar))
        (treesit-install-language-grammar (car grammar))
        ; (message "`%s' parser was installed." lang)
        (sit-for 0.75))))

  (dolist (mapping
           '(
              (bash-mode . bash-ts-mode)
              (css-mode . css-ts-mode)
              (elisp-mode . elisp-ts-mode)
              (html-mode . html-ts-mode)
              (js2-mode . js-ts-mode)
              (json-mode . json-ts-mode)
              (makefile-mode . make-ts-mode)
              (python-mode . python-ts-mode)
              (toml-mode . toml-ts-mode)
              (typescript-mode . typescript-ts-mode)
              (rust-mode . rust-ts-mode)
              (yaml-mode . yaml-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))

  :config
  (ts-install-grammars))

(use-package markdown-mode)

(use-package flycheck
  :hook (after-init-hook . global-flycheck-mode))

(use-package lsp-mode
  :hook (prog-mode-hook . (lambda ()
                            (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode 'makefile-mode 'snippet-mode)
                              (lsp-deferred))))
  :preface
  (setq read-process-output-max (* 64 1024))
  :init
  (setenv "LSP_USE_PLISTS" "false") ;; enable if using booster
  (setq lsp-use-plists nil) ;; enable if using booster
  :config
  (setq lsp-log-io t)
  ;; Emacs LSP booster
  ;; @seee https://github.com/blahgeek/emacs-lsp-booster
  (when (executable-find "emacs-lsp-booster-disable")
    (defun lsp-booster--advice-json-parse (old-fn &rest args)
      "Try to parse bytecode instead of json."
      (or
      (when (equal (following-char) ?#)
        (let ((bytecode (read (current-buffer))))
          (when (byte-code-function-p bytecode)
            (funcall bytecode))))
      (apply old-fn args)))
    (advice-add (if (progn (require 'json)
                          (fboundp 'json-parse-buffer))
                    'json-parse-buffer
                  'json-read)
                :around
                #'lsp-booster--advice-json-parse)

    (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
      "Prepend emacs-lsp-booster command to lsp CMD."
      (let ((orig-result (funcall old-fn cmd test?)))
        (if (and (not test?)                            ;; for check lsp-server-present?
                (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
                lsp-use-plists
                (not (functionp 'json-rpc-connection))  ;; native json-rpc
                (executable-find "emacs-lsp-booster"))
            (progn
              (message "Using emacs-lsp-booster for %s!" orig-result)
              (cons "emacs-lsp-booster" orig-result))
          orig-result)))
    (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)))

  (use-package lsp-ui
    :after lsp-mode)

  (use-package lsp-bridge
    :disabled t ;; Until I sort out global python packages installation on macOS 14+
    :ensure '(lsp-bridge :type git :host github :repo "manateelazycat/lsp-bridge"
              :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
              :build (:not compile))
    :init
    (global-lsp-bridge-mode))

  (use-package whitespace-mode :ensure nil)

  ;; VC
  (use-package vc-mode
    :ensure nil)

  (use-package diff-mode
    :ensure nil
    :hook (diff-mode-hook .(lambda ()
                            (setq-local whitespace-style
                                        '( face tabs tab-mark spaces space-mark trailing
                                          indentation::space indentation::tab
                                          newline newline-mark))
                            (whitespace-mode)))
    :after whitespace-mode)

  (use-package diff-hl
    :config
    (setq diff-hl-draw-borders nil
      diff-hl-disable-on-remote t)
    :custom
    (defun enable-diff-hl-dired-locally ()
    (if (file-remote-p default-directory)
        (diff-hl-dired-mode -1)
      (diff-hl-dired-mode 1)))

    (add-hook 'prog-mode-hook #'diff-hl-mode)
    (add-hook 'conf-mode-hook #'diff-hl-mode)
    (add-hook 'dired-mode-hook #'enable-diff-hl-dired-locally)

    (with-eval-after-load 'diff-hl
    (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)
    ;; Highlight on-the-fly
    (diff-hl-flydiff-mode 1)
    (unless (display-graphic-p)
      ;; Fall back to the display margin since the fringe is unavailable in tty
      (diff-hl-margin-mode 1)
      ;; Avoid restoring `diff-hl-margin-mode'
      (with-eval-after-load 'desktop
        (add-to-list 'desktop-minor-mode-table
                    '(diff-hl-margin-mode nil))))))

  (use-package smerge-mode
    :ensure nil
    :config
    (defhydra smerge-hydra
      (:color pink :hint nil :post (smerge-auto-leave))
      "
  ^Move^       ^Keep^               ^Diff^                 ^Other^
  ^^-----------^^-------------------^^---------------------^^-------
  _n_ext       _b_ase               _<_: upper/base        _C_ombine
  _p_rev       _u_pper              _=_: upper/lower       _r_esolve
  ^^           _l_ower              _>_: base/lower        _k_ill current
  ^^           _a_ll                _R_efine
  ^^           _RET_: current       _E_diff
  "
      ("n" smerge-next)
      ("p" smerge-prev)
      ("b" smerge-keep-base)
      ("u" smerge-keep-upper)
      ("l" smerge-keep-lower)
      ("a" smerge-keep-all)
      ("RET" smerge-keep-current)
      ("\C-m" smerge-keep-current)
      ("<" smerge-diff-base-upper)
      ("=" smerge-diff-upper-lower)
      (">" smerge-diff-base-lower)
      ("R" smerge-refine)
      ("E" smerge-ediff)
      ("C" smerge-combine-with-next)
      ("r" smerge-resolve)
      ("k" smerge-kill-current)
      ("ZZ" (lambda ()
              (interactive)
              (save-buffer)
              (bury-buffer))
      "Save and bury buffer" :color blue)
      ("q" nil "cancel" :color blue))
    :hook (magit-diff-visit-file . (lambda ()
                                    (when smerge-mode
                                      (smerge-hydra/body)))))

  (use-package magit
    :commands magit-status
    :ensure '(magit :type git :host github :repo "magit/magit" :branch "main")
    :preface
    (defun magit-disable-whitespace-mode ()
      (setq-local whitespace-trailing nil))
    :custom
    (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
    :config

    (setq magit-ellipsis (get-byte 0 ".")
          magit-revision-insert-related-refs nil
          magit-diff-refine-hunk t
          magit-diff-paint-whitespace nil
          magit-commit-show-diff nil
          magit-branch-direct-configure nil
          magit-refresh-status-buffer t
          magit-tramp-pipe-stty-settings 'pty)

    :custom
    (defvar magit-toplevel-cache nil)
    (defun memoize-magit-toplevel (orig &optional directory)
      (memoize-remote (or directory default-directory)
                      'magit-toplevel-cache orig directory))
    (with-eval-after-load 'magit
      (advice-add 'magit-toplevel :around #'memoize-magit-toplevel))
    (add-hook 'magit-mode-hook 'magit-disable-whitespace-mode)
    (remove-hook 'magit-status-headers-hook 'magit-insert-tags-header))

  (use-package forge
    :after (:all magit llama))

  (use-package ghub
    :config
    (setq ghub-default-host "github.com")
    :after (:all magit))

(use-package rust-mode
  :hook (rust-mode-hook . lsp)
  :custom
  (rust-format-on-save t)
  (rust-mode-treesitter-derive t))

(use-package cargo-mode
  :hook
  (rust-mode-hook . cargo-minor-mode)
  :config
  (setq compilation-scroll-output t))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("01c64d818433031bcdaef3b0ce836980f640a13673c0ca5df888760f240d5f4d"
     "77f281064ea1c8b14938866e21c4e51e4168e05db98863bd7430f1352cab294a"
     "2e7dc2838b7941ab9cabaa3b6793286e5134f583c04bde2fba2f4e20f2617cf7"
     default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
