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

(use-package auto-package-update
  :if (not (daemonp))
  :custom
  (auto-package-update-interval 7) ;; in days
  (auto-package-update-prompt-before-update t)
  (auto-package-update-delete-old-versions t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe))

(require 'tls)

(setq gnutls-verify-error (not (getenv "INSECURE"))
      tls-checktrust gnutls-verify-error
      tls-program (list "gnutls-cli --x509cafile %t -p %p %h"
                        ;; compatibility fallbacks
                        "gnutls-cli -p %p %h"
                        "openssl s_client -connect %h:%p -no_ssl2 -no_ssl3 -ign_eof"))

(setenv "PATH" (concat (getenv "PATH") ":~/.dotnet:~/.dotnet/tools:~/.cabal/bin:~/.ghcup/bin"))
(setq exec-path (append exec-path '("~/.dotnet" "~/.dotnet/tools" "~/.cabal/bin" "~/.ghcup/bin")))

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
    "Consolas")
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