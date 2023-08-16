;;; init.el --- Emacs configuration bootstrap. -*- lexical-binding: t; -*-
;;; Commentary:
;; Emacs bootstrap, for main configuration, see config.el

;;; Code:

(let ((minver 26))
  (unless (>= emacs-major-version minver)
    (error "Your Emacs is too old -- this configuration requires v%s or higher" minver)))

(unless (>= emacs-major-version 27)
  (message "Early init: Emacs Version < 27.0")
  (load (expand-file-name "early-init.el" user-emacs-directory)))

;; Setup package system

(eval-when-compile
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
  (add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/") t)
  (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)

  (setq package-quickstart t
        gnutls-algorithm-priority  "NORMAL:-VERS-TLS1.3"
        package-archive-priorities
        '(("melpa" .  500)
          ("melpa-stable" . 400)
          ("elpa" . 300)
          ("org" . 200)
          ("gnu" . 100)))

  ;; Initialise the packages, avoiding a re-initialisation.
  (unless (bound-and-true-p package--initialized)
    (setq package-enable-at-startup nil)
    (package-initialize))


  (setq load-prefer-newer t)              ; Always load newer compiled files
  (setq ad-redefinition-action 'accept)   ; Silence advice redefinition warnings

  ;; Setup use-package and straight.el
  (setq-default straight-repository-branch "develop"
                straight-use-package-by-default t
                straight-check-for-modifications nil
                straight-disable-autoloads nil
                straight-disable-byte-compilation nil
                straight-disable-native-compilation nil
                straight-use-symlinks t
                straight-cache-autoloads t
                straight-vc-git-default-branch "master"
                straight-vc-git-default-remote-name "origin"
                straight-vc-git-default-clone-depth 1
                straight-fix-flycheck t)

  ;; Setup use-package
  (setq-default use-package-always-demand t
                use-package-always-defer nil
                use-package-always-ensure nil
                use-package-expand-minimally nil
                use-package-enable-imenu-support t
                use-package-compute-statistics t
                use-package-hook-name-suffix nil)

  ;;; package manager bootstrap
  (defvar bootstrap-version)
  (let ((bootstrap-file
         (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
        (bootstrap-version 6))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    ;; catch emacs updates that have native compiled leftovers
    ;; Credits: https://github.com/radian-software/straight.el/issues/643
    (unless (catch 'emacs-version-changed
              (load bootstrap-file nil 'nomessage))
      (when (boundp 'native-comp-eln-load-path)
        ;; remove leftovers, with confirmation just to be safe
        (when (yes-or-no-p (format "Delete '%s'?" (car native-comp-eln-load-path)))
          (delete-directory (expand-file-name (car native-comp-eln-load-path)) t))
        ;; and try again
        (load bootstrap-file nil 'nomessage))))
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
  (straight-use-package 'use-package))

(require 'tls)

(setq gnutls-verify-error (not (getenv "INSECURE"))
      tls-checktrust gnutls-verify-error
      tls-program (list "gnutls-cli --x509cafile %t -p %p %h"
                        ;; compatibility fallbacks
                        "gnutls-cli -p %p %h"
                        "openssl s_client -connect %h:%p -no_ssl2 -no_ssl3 -ign_eof"))

;; (use-package gcmh
;;   :delight
;;   :init
;;   (setq gcmh-idle-delay 5
;;         gcmh-high-cons-threshold (* 512 1024 1024))
;;   :config
;;   (gcmh-mode))

(setenv "PATH" (concat (getenv "PATH") ":~/.dotnet:~/.dotnet/tools:~/.cabal/bin:~/.ghcup/bin"))
(setq exec-path (append exec-path '("~/.dotnet" "~/.dotnet/tools" "~/.cabal/bin" "~/.ghcup/bin")))

;; Load actual config
;; Common functions and libs
(use-package ov) ;; For manipulating overlays

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

;; Core packages
(use-package esup :ensure)

(use-package auto-package-update
  :if (not (daemonp))
  :custom
  (auto-package-update-interval 7) ;; in days
  (auto-package-update-prompt-before-update t)
  (auto-package-update-delete-old-versions t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe))

(use-package no-littering
  :config
  (require 'recentf)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (setq custom-file (no-littering-expand-etc-file-name "custom.el")))

(use-package saveplace
  :config
  (setq save-place-file (no-littering-expand-etc-file-name "saveplace"))
  (setq save-place-forget-unreadable-files t)
  (save-place-mode 1))

(use-package savehist
  :config
  (setq savehist-file (no-littering-expand-etc-file-name "savehist"))
  (setq history-length 1000)
  (setq history-delete-duplicates t)
  (setq savehist-save-minibuffer-history t)
  :hook (after-init-hook . savehist-mode))

(use-package super-save
  :delight
  :defer 0.5
  :config
  (add-to-list 'super-save-triggers 'switch-window)
  (setq super-save-idle-duration 1)
  (setq super-save-auto-save-when-idle t)
  (setq save-silently t)
  (super-save-mode 1))

(use-package ansi-color
  :ensure nil
  :preface
  (defun my-colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-after-start (point-max))))
  :hook (compilation-filter-hook . my-colorize-compilation-buffer))

(use-package delight
  :after use-package)

(use-package async
  :init
  (setq async-bytecomp-allowed-packages '(all))
  :config
  (async-bytecomp-package-mode t)
  (dired-async-mode 1)
  ;; limit number of async processes
  (eval-when-compile
    (require 'cl-lib))
  (defvar async-maximum-parallel-procs 20)
  (defvar async--parallel-procs 0)
  (defvar async--queue nil)
  (defvar-local async--cb nil)
  (advice-add #'async-start :around
              (lambda (orig-func func &optional callback)
                (if (>= async--parallel-procs async-maximum-parallel-procs)
                    (push `(,func ,callback) async--queue)
                  (cl-incf async--parallel-procs)
                  (let ((future (funcall orig-func func
                                         (lambda (re)
                                           (cl-decf async--parallel-procs)
                                           (when async--cb (funcall async--cb re))
                                           (when-let (args (pop async--queue))
                                             (apply #'async-start args))))))
                    (with-current-buffer (process-buffer future)
                      (setq async--cb callback)))))
              '((name . --queue-dispatch))))

;; Configure Emacs' defaults and keybinds;
(use-package emacs
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
         ("C-k"             . 'kill-buffer)
         ("C-K"             . 'kill-this-buffer)
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

  :config
  ;; Defaults

  (setq-default major-mode 'text-mode
                use-file-dialog nil
                use-dialog-box t
                cursor-type 'box
                x-stretch-cursor t
                cursor-in-non-selected-window nil
                indent-tabs-mode nil)

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
        next-line-add-newlines nil

        byte-compile-warnings '(cl-functions)
        visible-bell nil
        ring-bell-function 'flash-mode-line

        tab-width 4
        frame-resize-pixelwise t

	redisplay-skip-fontification-on-input t

        window-divider-default-right-width 1
        window-divider-default-bottom-width 1
        window-divider-default-places 'right-only

	show-trailing-whitespace t
        whitespace-style '(face trailing)
        make-backup-files t
        backup-directory-alist '(("." . "~/.saves"))
        auto-save-default nil
        track-eol nil
        line-move-visual nil
        kill-whole-line t
        indent-tabs-mode nil
        truncate-lines t
        show-paren-style 'parenthesis
        frame-resize-pixelwise t))

(use-package frame
  :straight nil
  :config
  (setq mode-line-compact t
        max-mini-window-height 1.0
        input-method-use-echo-area nil
        echo-keystrokes 0
        resize-mini-windows nil)
  (blink-cursor-mode -1)
  (column-number-mode t)
  (global-subword-mode t)
  (horizontal-scroll-bar-mode -1)
  (line-number-mode +1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (size-indication-mode t)
  (toggle-scroll-bar -1)
  (tool-bar-mode -1)

  (when (featurep 'ns)
    (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t)))
  (add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))
  (add-to-list 'default-frame-alist '(menu-bar-lines . 0))
  (add-to-list 'default-frame-alist '(tool-bar-lines . 0))
  (add-to-list 'default-frame-alist '(left-fringe . 1))
  (add-to-list 'default-frame-alist '(right-fringe . 1))
  (add-to-list 'default-frame-alist '(internal-border-width . 0)))

(use-package fullframe
  :after magit
  :config
  (fullframe magit-status magit-mode-quit-window))

(use-package faces
  :straight nil
  :preface

  ;; (defun my-dpi ()
  ;;   (let* ((attrs (car (display-monitor-attributes-list)))
  ;;          (size (assoc 'mm-size attrs))
  ;;          (sizex (cadr size))
  ;;          (res (cdr (assoc 'geometry attrs)))
  ;;          (resx (- (caddr res) (car res)))
  ;;          dpi)
  ;;     (catch 'exit
  ;;       ;; in terminal
  ;;       (unless sizex
  ;;         (throw 'exit 10))
  ;;       ;; on big screen
  ;;       (when (> sizex 1000)
  ;;         (throw 'exit 10))
  ;;       ;; DPI
  ;;       (* (/ (float resx) sizex) 25.4))))

  (defun my-dpi (&optional frame)
    "Get the DPI of FRAME (or current if nil)."
    (cl-flet ((pyth (lambda (w h)
                      (sqrt (+ (* w w)
                               (* h h)))))
              (mm2in (lambda (mm)
                       (/ mm 25.4))))
      (let* ((atts (frame-monitor-attributes frame))
             (pix-w (cl-fourth (assoc 'geometry atts)))
             (pix-h (cl-fifth (assoc 'geometry atts)))
             (pix-d (pyth pix-w pix-h))
             (mm-w (cl-second (assoc 'mm-size atts)))
             (mm-h (cl-third (assoc 'mm-size atts)))
             (mm-d (pyth mm-w mm-h)))
        (/ pix-d (mm2in mm-d)))))

  (defun my-preferred-font-size ()
    (let ((dpi (my-dpi)))
      (message "DPI: %d" dpi)
      (cond
       ((< dpi 110) 17)
       ((> dpi 130) 18)
       (t 14))))

  (message "Initial preferred font size: %d" (my-preferred-font-size))

  (defvar --font-name
    "Fira Code")
  (defvar --default-font
    (font-spec :family --font-name :size (my-preferred-font-size) :dpi (my-dpi) :weight 'normal))

  (defun adapt-font-size (&optional frame)
    (message "Adapted preferred font size: %d" (my-preferred-font-size))
    (set-frame-font (font-spec :family --font-name :size (my-preferred-font-size) :dpi (my-dpi) :weight 'normal)))

  :config

  (setf (alist-get 'font default-frame-alist)
        (font-xlfd-name --default-font))
  (set-frame-font --default-font t t)
  (when (display-graphic-p)
    (setq font-use-system-font t))

  (add-function :after after-focus-change-function #'adapt-font-size)
  (add-hook 'window-size-change-functions #'adapt-font-size)
  (add-hook 'after-make-frame-functions #'adapt-font-size))

(use-package visual-fill-column)

(use-package display-line-numbers
  :ensure nil
  :hook (prog-mode-hook . display-line-numbers-mode)
  :config
  (setq-default display-line-numbers-width 5))

(use-package mixed-pitch
  :diminish)

(use-package dired
  :straight nil
  :config
  (setq dired-kill-when-opening-new-dired-buffer t))

(use-package fast-scroll
  :delight
  :disabled t
  ;; TODO: Turn off all highlighter modes, restore them afterwards (turn off font-lock-mode?).
  :hook (fast-scroll-start-hook . (lambda ()
                                    (flycheck-mode -1)
                                    (highlight-indent-guides-mode -1)
                                    (highlight-symbol-mode -1)))
  :hook (fast-scroll-end-hook . (lambda ()
                                  (flycheck-mode 1)
                                  (highlight-indent-guides-mode 1)
                                  (highlight-symbol-mode 1)))
  :config
  (fast-scroll-config)
  (fast-scroll-mode 1))

(use-package fringe
  :straight nil
  :config
  (fringe-mode nil)
  (setq-default fringes-outside-margins nil
                indicate-buffer-boundaries nil
                indicate-empty-lines nil
                overflow-newline-into-fringe t))

(use-package exec-path-from-shell
  :custom
  (exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-variables '("PATH" "DOTNET_HOME" "DOTNET_ROOT" "SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO"))
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package hydra)

(use-package popwin)

;; Editing and navigation (including windows navigation, dwim/mwin, mc, etc):

(use-package so-long
  :ensure nil
  :hook (after-init-hook . global-so-long-mode))

(use-package vlf
  :defer t
  :preface
  (defun ffap-vlf ()
    "Find file at point with VLF."
    (interactive)
    (require 'ffap)
    (let ((file (ffap-file-at-point)))
      (unless (file-exists-p file)
        (error "File does not exist: %s" file))
      (vlf file))))

(use-package recentf
  :ensure nil
  :hook (after-init-hook . recentf-mode)
  :custom
  (recentf-auto-cleanup "05:00am")
  (recentf-max-saved-items 200)
  (recentf-exclude '((expand-file-name package-user-dir)
                     ".cache"
                     ".cask"
                     ".elfeed"
                     "bookmarks"
                     "cache"
                     "ido.*"
                     "persp-confs"
                     "recentf"
                     "undo-tree-hist"
                     "url"
                     "COMMIT_EDITMSG\\'"))
  :config
  (save-place-mode 1)
  (setq-default history-length 500))

(use-package subword
  :straight nil
  :delight)

(use-package hungry-delete
  :delight
  :straight (:host github :repo "nflath/hungry-delete" :branch "master")
  ;; :straight (:host github :repo "nflath/hungry-delete" :branch "master" :build (:not compile))
  :config
  (setq hungry-delete-join-reluctantly t)
  (global-hungry-delete-mode))

(use-package expand-region
  :bind (("C-=" . 'er/expand-region)
         ("C-+" . 'er/contract-region)))

(use-package multiple-cursors
  :config
  (add-to-list 'mc/unsupported-minor-modes 'lispy-mode)
  (setq mc/always-run-for-all t)
  :bind (("C-S-c C-S-c" . 'mc/edit-lines)
         ("C-d" . 'mc/mark-next-like-this)
         ("C-S-d" . 'mc/mark-previous-like-this)
         ("C->" . 'mc/mark-next-like-this)
         ("C-<" . 'mc/mark-previous-like-this)
         ("C-c C-<" . 'mc/mark-all-like-this)))

(use-package drag-stuff
  :delight
  :config
  (drag-stuff-define-keys)
  (drag-stuff-global-mode 1))

(use-package delsel
  :hook (after-init-hook . delete-selection-mode))

(use-package comment-dwim-2
  :bind (("M-;" . 'comment-dwim-2)))

(use-package mwim
  :bind (("C-a" . 'mwim-beginning-of-code-or-line)
         ("C-e" . 'mwim-end-of-code-or-line)
         ("<home>" . 'mwim-beginning-of-code-or-line)
         ("<end>" . 'mwim-end-of-code-or-line)))

(use-package crux
  :bind (("C-x K"        . crux-kill-other-buffers)
         ("C-k"          . crux-smart-kill-line)
         ("C-c 2"        . crux-duplicate-current-line-or-region)
         ("<S-return>"   . crux-smart-open-line)
         ("<C-S-return>" . crux-smart-open-line-above))
  :config
  (crux-with-region-or-buffer indent-region)
  (crux-with-region-or-buffer untabify)
  (crux-with-region-or-point-to-eol kill-ring-save)
  (defalias 'rename-file-and-buffer #'crux-rename-file-and-buffer))

(use-package window
  :straight nil
  :init
  (setq window-combination-resize t
        even-window-sizes 'height-only
        window-sides-slots '(0 1 1 1)
        window-sides-vertical nil
        switch-to-buffer-in-dedicated-window 'pop
        display-buffer-alist
        '(;; top side window
          ;;
          ;; bottom side window
          ("\\*\\(Flymake\\|Package-Lint\\|vc-git :\\).*"
           (display-buffer-reuse-window display-buffer-in-previous-window display-buffer-in-side-window)
           (window-height . 0.16)
           (side . bottom)
           (slot . 0)
           (window-parameters . ((no-other-window . t))))
          ("\\*Messages.*"
           (display-buffer-reuse-window display-buffer-in-previous-window display-buffer-in-side-window)
           (window-height . 0.16)
           (side . bottom)
           (slot . 1)
           (window-parameters . ((no-other-window . t))))
          ("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\)\\*"
           (display-buffer-reuse-window display-buffer-in-previous-window display-buffer-in-side-window)
           (window-height . 0.16)
           (side . bottom)
           (slot . 2)
           (window-parameters . ((no-other-window . t))))
          ("\\*\\(Output\\|Register Preview\\).*"
           (display-buffer-reuse-window display-buffer-in-previous-window display-buffer-in-side-window)
           (window-width . 0.16)       ; See the :hook
           (side . bottom)
           (slot . -1)
           (window-parameters . ((no-other-window . t))))
          ;; left side window
          ("\\*Help.*"
           (display-buffer-reuse-window display-buffer-in-previous-window display-buffer-in-side-window)
           (window-width . 0.20)       ; See the :hook
           (side . left)
           (slot . 0)
           (window-parameters . ((no-other-window . t))))
          ;; right side window
          ("\\*Faces\\*"
           (display-buffer-reuse-window display-buffer-in-previous-window display-buffer-in-side-window)
           (window-width . 0.25)
           (side . right)
           (slot . 0)
           (window-parameters
            . ((no-other-window . t)
               (mode-line-format
                . (" "
                   mode-line-buffer-identification)))))
          ("\\*Custom.*"
           (display-buffer-reuse-window display-buffer-in-previous-window display-buffer-in-side-window)
           (window-width . 0.25)
           (side . right)
           (slot . 1))
          ;; bottom buffer (NOT side window)
          ("\\*\\vc-\\(incoming\\|outgoing\\).*"
           (display-buffer-at-bottom)))))

(use-package ace-window
  :bind (("C-x o" . 'ace-window))
  :config
  (set-face-attribute
   'aw-leading-char-face nil
   :foreground "deep sky blue"
   :weight 'bold
   :height 2.0)
  (set-face-attribute
   'aw-mode-line-face nil
   :inherit 'mode-line-buffer-id
   :foreground "lawn green")
  (setq aw-keys '(?a ?s ?d ?f ?j ?k ?l)
        aw-dispatch-always nil
        aw-dispatch-alist
        '((?x aw-delete-window "Ace - Delete Window")
          (?c aw-swap-window "Ace - Swap Window")
          (?n aw-flip-window)
          (?v aw-split-window-vert "Ace - Split Vert Window")
          (?h aw-split-window-horz "Ace - Split Horz Window")
          (?m delete-other-windows "Ace - Maximize Window")
          (?g delete-other-windows)
          (?b balance-windows)
          (?u (lambda ()
                (progn
                  (winner-undo)
                  (setq this-command 'winner-undo))))
          (?r winner-redo)))
  (ace-window-display-mode t))

(use-package zoom
  :init
  (defvar default-zoom-size '(0.5 . 0.5))
  (defun zoom-to-default ()
    (interactive)
    (custom-set-variables '(zoom-size default-zoom-size))
    (zoom))
  (defvar maximum-zoom-size '(0.99 . 0.99))
  (defun zoom-to-maximum ()
    (interactive)
    (custom-set-variables '(zoom-size maximum-zoom-size))
    (zoom))
  :bind (("C-x z" . zoom-to-maximum)
         ("C-x C-z" . zoom-to-default))
  :config
  (custom-set-variables
   '(zoom-ignored-major-modes '(dired-mode markdown-mode))
   '(zoom-ignored-buffer-names '("zoom.el" "init.el"))
   '(zoom-ignored-buffer-name-regexps '("^*calc"))
   '(zoom-size '(0.99 . 0.99))))

(use-package undo-tree
  :delight
  :straight (:host gitlab :repo "tsc25/undo-tree" :branch "master")
  :init
  (setq undo-tree-auto-save-history t
        undo-tree-visualizer-diff t
        undo-tree-visualizer-timestamps t)
  :bind (("C-x u" . undo-tree-visualize)
         ("C-z"   . 'undo)
         ("C-S-z" .'undo-tree-redo))
  :config
  (progn
    (defun my/undo-tree-restore-default ()
      (setq undo-tree-visualizer-diff t))
    (advice-add 'undo-tree-visualizer-quit :after #'my/undo-tree-restore-default))
  (global-undo-tree-mode t))

;; (use-package point-history
;;   :straight nil
;;   :load-path "~/code/elisp/point-history"
;;   :bind (("M-g s" . 'point-history-show))
;;   :config
;;   (setq point-history-ignore-buffer "^ \\*Minibuf\\|^ \\*point-history-show*\\|\\*scratch\\|\\*"
;;         point-history-ignore-major-mode '(emacs-lisp-mode ruby-mode))
;;   (point-history-mode t))


(use-package point-stack ;; Alternative: jumplist (ganmacs/jumplist)
  ;; TODO: Add fuinction which will show list of points via ivy.
  :straight (:host github :repo "dgutov/point-stack" :branch "master")
  :bind (("M-[" . 'point-stack-pop)
         ("M-]" . 'point-stack-forward-stack-pop))
  :config
  (setq jump-advised-functions
        '(isearch-mode
          find-function-do-it
          find-library
          imenu
	  isearch-forward
          my/swiper
          counsel-switch-buffer
          counsel-ibuffer
          counsel-imenu
          counsel-recentf
          counsel-find-file
          dumb-jump-go
          dumb-jump-back
          smart-jump-go
          smart-jump-back
          ;; switch-to-buffer
          ;; change-buffer
          ;; previous-buffer
          ;; next-buffer
          beginning-of-buffer
          end-of-buffer
          backward-up-list
          beginning-of-defun
          end-of-defun
          find-function
          find-variable
          mark-defun
          mark-whole-buffer
          xref-find-definitions
          xref-find-references
          xref-pop-marker-stack
          xref-push-marker-stack
          magit-diff-visit-file))

  (setq point-stack-advised-functions jump-advised-functions)
  (point-stack-setup-advices))

(use-package smartparens
  :delight
  :config
  (smartparens-global-mode 1))

(use-package text-mode
  :straight nil
  :custom
  (adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*")
  (adaptive-fill-first-line-regexp "^* *$")
  (sentence-end "\\([。、！？]\\|……\\|[,.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*"))

;; Spelling
(use-package flyspell
  :defer t
  :delight
  :ensure nil
  :if (executable-find "aspell")
  ;; Add spell-checking in comments for all programming language modes
  :hook ((prog-mode-hook . flyspell-prog-mode)
         (flyspell-mode-hook . (lambda ()
                                 (dolist (key '("C-;" "C-."))
                                   (unbind-key key flyspell-mode-map)))))
  :init
  (setq flyspell-issue-message-flag nil
        ispell-program-name "aspell"
        ispell-extra-args '("--sug-mode=fast" "--lang=en_US" "--camel-case")
        ispell-personal-dictionary
        (expand-file-name "en_US.personal" "~/.config/aspell/")))

;; Correcting words with flyspell via completing-read
(use-package flyspell-correct
  :after flyspell
  :bind (:map flyspell-mode-map ("C-," . flyspell-correct-wrapper)))

;; Terminal
(use-package vterm
  :if (not *sys/is-wsl*))
(use-package vterm-toggle
  :after vterm
  :bind (("C-`" . vterm-toggle)
         ("C-M-`" . vterm-toggle-cd))
  :config
  (setq vterm-toggle-cd-auto-create-buffer nil
        vterm-toggle-fullscreen-p t)
  (add-to-list 'display-buffer-alist
               '((lambda(bufname _) (with-current-buffer bufname (equal major-mode 'vterm-mode)))
                 (display-buffer-reuse-window display-buffer-same-window))))

;; Tooling packages
(use-package deadgrep
  :preface
  (defun deadgrep--include-args (rg-args)
    (push "--color=auto" rg-args)
    (push "--hidden" rg-args)
    (push "--follow" rg-args))
  :if (executable-find "rg")
  :bind ("M-s" . 'deadgrep)
  :bind (:map deadgrep-mode-map
              ("M-e" . deadgrep-edit-mode)
              ("RET" . deadgrep-visit-result-other-window)
              ("o" . deadgrep-visit-result))
  :config
  (advice-add 'deadgrep--arguments :filter-return #'deadgrep--include-args))

(use-package flycheck
  :defer t
  :hook (after-init-hook . global-flycheck-mode)
  :config
  (setq flycheck-highlighting-mode 'sexps))

(use-package fsharp-mode
  :defer t
  :after (:all lsp-mode)
  :commands fsharp-mode
  :config
  (setq indent-tabs-mode nil
        truncate-lines t
        tab-width 4))

(use-package lsp-mode
  :defer t
  :commands (lsp lsp-deferred lsp-mode)
  :hook (fsharp-mode-hook . lsp-deferred)
  :after flycheck)

(use-package lsp-ui
  :defer t
  :after lsp-mode)

;; Git stuff

(use-package git-modes
  :defer t)

(use-package magit
  :defer t
  :commands (magit magit-status magit-blame magit-mode magit-file-popup)
  :bind (("C-x g" . magit-status)
         ("C-c g" . magit-dispatch-popup))
  :config
  (setq magit-status-margin '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 18)
        magit-list-refs-sortby "-creatordate"
        magit-refs-show-commit-count 'branch
        magit-status-sections-hook (-concat magit-status-sections-hook '(magit-insert-local-branches))
        magit-diff-refine-hunk t
  	magit-commit-arguments '("--verbose")
  	magit-section-initial-visibility-alist
        '((unpulled . show)
          (unpushed . show)
          (untracked . show)
          (unstaged . show)
          (pullreqs . show)
          (issues . show)
          (stashes . show)
          (todos . show)
          (recent . show)))

  (magit-add-section-hook 'magit-status-sections-hook 'forge-insert-assigned-pullreqs nil t)
  (magit-add-section-hook 'magit-status-sections-hook 'forge-insert-assigned-issues nil t)

  (progn
    (setq magit-post-display-buffer-hook
          #'(lambda ()
              (when (derived-mode-p 'magit-status-mode)
                (delete-other-windows))))

    (setenv "GIT_PAGER" "")
    (add-hook 'magit-log-edit-mode-hook
              '(lambda ()
                 (auto-fill-mode)
                 (flyspell-mode)
                 (set-fill-column 120))))

  (defvar magit--modified-files nil)

  (defun magit-maybe-cache-modified-files ()
    "Maybe save a list of modified files.
 That list is later used by `magit-update-uncommitted-buffers',
 provided it is a member of `magit-post-refresh-hook'.  If it is
 not, then don't save anything here."
    (when (memq 'magit-update-uncommitted-buffers magit-post-refresh-hook)
      (setq magit--modified-files (magit-unstaged-files t))))

  (add-hook 'magit-pre-refresh-hook #'magit-maybe-cache-modified-files)
  (add-hook 'magit-pre-call-git-hook #'magit-maybe-cache-modified-files)
  (add-hook 'magit-pre-start-git-hook #'magit-maybe-cache-modified-files)

  (defun magit-update-uncommitted-buffers ()
    "Update some file-visiting buffers belonging to the current repository.
 Run `magit-update-uncommitted-buffer-hook' for each buffer
 which visits a file inside the current repository that had
 uncommitted changes before running the current Magit command
 and/or that does so now."
    (let ((topdir (magit-toplevel)))
      (dolist (file (delete-consecutive-dups
		     (sort (nconc (magit-unstaged-files t)
				  magit--modified-files)
			   #'string<)))
	(--when-let (find-buffer-visiting (expand-file-name file topdir))
	  (with-current-buffer it
	    (run-hooks 'magit-update-uncommitted-buffer-hook))))))

  (add-hook 'magit-post-refresh-hook #'magit-update-uncommitted-buffers)

  (define-advice magit-push-current-to-upstream (:before (args) query-yes-or-no)
    "Prompt for confirmation before permitting a push to upstream."
    (when-let ((branch (magit-get-current-branch)))
      (unless (yes-or-no-p (format "Push %s branch upstream to %s? "
				   branch
				   (or (magit-get-upstream-branch branch)
				       (magit-get "branch" branch "remote"))))
	(user-error "Push to upstream aborted by user")))))

(use-package magit-lfs
  :after magit)

(use-package magit-todos
  :after magit
  :hook (magit-mode-hook . magit-todos-mode)
  :custom
  (magit-todos-exclude-globs '("node_modules" "*.json"))
  :config
  (setq magit-todos-auto-group-items 'always))

(use-package magit-delta
  :delight
  :if (executable-find "delta")
  :after magit
  :hook (magit-mode-hook . magit-delta-mode)
  :config
  (setq magit-delta-hide-plus-minus-markers nil))

(use-package magit-filenotify
  :after magit
  :hook (after-save-hook . magit-filenotify-mode))

(use-package forge
  :after magit
  :config
  (setq forge-topic-list-limit '(100 . -10)))

(use-package git-commit-insert-issue
  :config
  (add-hook 'git-commit-mode-hook 'git-commit-insert-issue-mode))

(use-package graphql)

(use-package gh-notify
  :after (:all magit forge transient)
  :config
  (transient-insert-suffix 'forge-dispatch '(1)
    ["GitHub"
     ("g n" "notifications" gh-notify)]))

(use-package ghub)

(use-package github-explorer
  :after graphql)

;; Have multiple packages for PR handling, just to test them out.
(use-package github-review
  ;; :disabled t
  :after (:all magit forge transient)
  :straight (:host github :repo "charignon/github-review" :files ("github-review.el"))
  :config
  (transient-insert-suffix 'forge-dispatch '(1)
    ["GitHub Review"
     ("p g" "github-review at point" github-review-forge-pr-at-point)]))

(use-package code-review
  :disabled t ;; breaking with updated CloSQL
  ;;:straight (:host github :repo "vzarytovskii/code-review")
  :after (:all magit forge transient)
  :config
  (setq code-review-new-buffer-window-strategy #'switch-to-buffer)
  (transient-insert-suffix 'forge-dispatch '(1)
    ["Code Review"
     ("p c" "code-review at point" code-review-forge-pr-at-point)]))

(use-package pr-review
  ;; :disabled t
  :straight (:host github :repo "blahgeek/emacs-pr-review" :files (:defaults "graphql"))
  :after (:all magit forge transient)
  :config
  (transient-insert-suffix 'forge-dispatch '(1)
    ["PR Review"
     ("p p" "pr-review at point" pr-review-notification-open)]))

(use-package git-link
  :bind (("C-x C-g i")))

(use-package git-blamed)

(use-package git-timemachine
  :after magit
  :commands my/git-timemachine-on
  :bind ("C-x v t" . git-timemachine-toggle)
  :config
  (defhydra my/git-timemachine
    (:color pink :hint nil)
    ("n" git-timemachine-show-next-revision "Next Revision" :column "Go to")
    ("p" git-timemachine-show-previous-revision "Next Revision")
    ("c" git-timemachine-show-current-revision "Current Revision")
    ("g" git-timemachine-show-nth-revision "Nth Revision")
    ("t" git-timemachine-show-revision-fuzzy "Search")
    ("W" git-timemachine-kill-revision "Copy full revision" :column "Actions")
    ("w" git-timemachine-kill-abbreviated-revision "Copy abbreviated revision" :column "Actions")
    ("C" git-timemachine-show-commit "Show commit")
    ("b" git-timemachine-blame "Blame")
    ("q" git-timemachine-quit "cancel" :color blue :column nil))
  (defun my/git-timemachine-on ()
    (interactive)
    (git-timemachine)
    (my/git-timemachine/body))

  ;; (define-advice git-timemachine-mode (:after (_args) open-timemachine-hydra)
  ;; (my/git-timemachine-on))

  (transient-insert-suffix 'magit-dispatch '(1)
    ["Git Time Machine"
     ("T" "Toggle time machine" my/git-timemachine-on)]))

(use-package git-messenger
  :init (setq git-messenger:show-detail t)
  :bind (:map vc-prefix-map
              ("p" . git-messenger:popup-message)))

(use-package git-gutter-fringe
  :delight git-gutter-mode
  :hook (after-init-hook . global-git-gutter-mode)
  :init (setq git-gutter:visual-line t
              git-gutter:disabled-modes '(asm-mode image-mode)
              git-gutter:modified-sign "~" ;;"❚"
              git-gutter:added-sign "+" ;;"✚"
              git-gutter:deleted-sign "-" ;;"✖"
              blamer-max-commit-message-length 100)

  :bind (("C-c v =" . git-gutter:popup-hunk)
         ("C-c p" . git-gutter:previous-hunk)
         ("C-c n" . git-gutter:next-hunk)))

(use-package diff-hl
  :disabled t
  :config
  (global-diff-hl-mode))

(use-package smerge-mode
  :after hydra
  :preface
  (with-eval-after-load 'hydra
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
      ("q" nil "cancel" :color blue)))
  :hook ((find-file-hook . (lambda ()
                             (save-excursion
                               (goto-char (point-min))
                               (when (re-search-forward "^<<<<<<< " nil t)
                                 (smerge-mode 1)))))
         (magit-diff-visit-file-hook . (lambda ()
                                         (when smerge-mode
                                           (smerge-hydra/body))))))

;; TODO:Move to common packages
(use-package transient
  :config
  (setq transient-default-level 5))

(provide 'init)
;;; init.el ends here
