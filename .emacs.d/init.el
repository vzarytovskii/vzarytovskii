;;; init.el --- Emacs configuration bootstrap.
;; -*- lexical-binding: t; -*-
;; -*-no-byte-compile: t; -*-

;;; Commentary:

;;; Code:

;; Setup package system

(defvar elpaca-installer-version 0.7)
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
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                ,@(when-let ((depth (plist-get order :depth)))
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
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)

(setq package-quickstart t
      gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"
      package-archive-priorities
      '(("melpa" .  500)
        ("melpa-stable" . 400)
        ("elpa" . 300)
        ("org" . 200)
        ("gnu" . 100)))

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
              use-package-always-defer nil
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

(setenv "PATH" (concat (getenv "PATH") ":~/.dotnet:~/.dotnet/tools:~/.cabal/bin:~/.ghcup/bin:~/.local/bin"))
(setq exec-path (append exec-path '("~/.dotnet" "~/.dotnet/tools" "~/.cabal/bin" "~/.ghcup/bin" "~/.local/bin")))

(defun reload-init-file ()
    (interactive)
    (load-file user-init-file))

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

(use-package gcmh
  :hook (after-init-hook . gcmh-mode)
  :custom
  (gcmh-idle-delay 10)
  (gcmh-high-cons-threshold #x6400000))

(use-package auto-package-update
  :if (not (daemonp))
  :custom
  (auto-package-update-interval 7) ;; in days
  (auto-package-update-prompt-before-update t)
  (auto-package-update-delete-old-versions t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe))

(use-package exec-path-from-shell
  :when (eq system-type 'darwin)
  :hook (after-init-hook . exec-path-from-shell-initialize))

(use-package delight)

(use-package transient)

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
         ;; ("C-k"             . 'kill-buffer)
         ;; ("C-K"             . 'kill-this-buffer)
         ("C-c o"           . 'switch-to-minibuffer))
  :hook (after-init-hook . window-divider-mode)
  :delight lisp-interaction-mode
  :preface

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

  (setq inhibit-default-init t
        inhibit-startup-screen t
        inhibit-startup-message t
        inhibit-startup-echo-area-message t
        initial-scratch-message nil

	      idle-update-delay 1.1

        scroll-margin 0
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
        use-short-answers t))

(use-package modus-themes
  :config
  (setq modus-themes-italic-constructs nil
        modus-themes-bold-constructs t)

  (setq modus-themes-common-palette-overrides
        modus-themes-preset-overrides-intense))

(use-package auto-dark
  :config
  (auto-dark-mode t)
  (setq auto-dark-allow-osascript t)
  (setq auto-dark-dark-theme 'modus-vivendi-tritanopia)
  (setq auto-dark-light-theme 'modus-operandi-tritanopia))

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
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package consult
  :bind (
         ("M-s"   . consult-ripgrep)
         ("C-x b" . consult-buffer))
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

;; ---
(use-package yasnippet
  :defer t)

(use-package markdown-mode
  :defer t)

(use-package lsp-mode
  :defer t
  :hook (prog-mode-hook . (lambda ()
                            (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode 'makefile-mode 'snippet-mode)
                              (lsp-deferred))))
  :preface
  (setq read-process-output-max (* 1024 1024)) ; 1MB
  (setenv "LSP_USE_PLISTS" "true")
  :init
  (setq lsp-use-plists t)
  :config
  ;; Emacs LSP booster
  ;; @seee https://github.com/blahgeek/emacs-lsp-booster
  (when (executable-find "emacs-lsp-booster")
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
        (if (and (not test?)                             ;; for check lsp-server-present?
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
    :after lsp-mode
    :defer t)

  (use-package lsp-bridge
    :disabled t ;; Until I sort out global python packages installation on macOS 14+
    :defer t
    :ensure '(lsp-bridge :type git :host github :repo "manateelazycat/lsp-bridge"
              :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
              :build (:not compile))
    :init
    (global-lsp-bridge-mode))

  (use-package fsharp-mode
    :defer t
    :after (:all lsp-mode)
    :mode "\\.fs[iylx]?$"
    :config
    (setq indent-tabs-mode nil
          truncate-lines t
          tab-width 4))

  (use-package magit
    :defer t
    :commands magit-status
    :ensure '(magit :type git :host github :repo "magit/magit" :branch "main")
    :preface
    (defun magit-disable-whitespace-mode ()
      (setq-local whitespace-trailing nil))
    :custom
    (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
    :config

    (setq magit-ellipsis (get-byte 0 ".")
          magit-revision-insert-related-refs nil)

    (add-hook 'magit-mode-hook 'magit-disable-whitespace-mode)
    (remove-hook 'magit-refs-sections-hook 'magit-insert-tags)
    (remove-hook 'magit-status-sections-hook 'magit-insert-status-headers)
    (remove-hook 'magit-status-sections-hook 'magit-insert-tags-headers))

  (use-package forge
    :defer t
    :after magit)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("9a977ddae55e0e91c09952e96d614ae0be69727ea78ca145beea1aae01ac78d2"
     default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
