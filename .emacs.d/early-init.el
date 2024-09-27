;;; early-init.el --- Emacs 27+ early init.
;; -*- lexical-binding: t; -*-
;; -*-no-byte-compile: t; -*-
;;; Commentary:
;; Emacs 27+ early init, for main configuration, see init.el.

;;; Code:

(let ((minver 26))
  (unless (>= emacs-major-version minver)
    (error "Your Emacs is too old -- this configuration requires v%s or higher" minver)))

(unless (>= emacs-major-version 27)
  (message "Early init: Emacs Version < 27.0")
  (load (expand-file-name "early-init.el" user-emacs-directory)))

(with-eval-after-load 'package
  (setopt package-enable-at-startup nil))

(defun display-startup-echo-area-message ()
  (message ""))

(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(fullscreen . fullheight))
(add-to-list 'default-frame-alist '(undecorated . t))
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'set-scroll-bar-mode) (set-scroll-bar-mode nil))

;; Some basic checks:

(if (and (fboundp 'native-comp-available-p)
         (native-comp-available-p))
    (message "Native compilation is available")
  (message "Native compilation is *not* available"))

(if (functionp 'json-serialize)
    (message "Native JSON is available")
  (message "Native JSON is *not* available"))

(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

(setq locale-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

(set-face-attribute 'default nil :family "Consolas" :height 140)
(set-face-attribute 'fixed-pitch nil :family "Consolas" :height 130 :weight 'semi-light :width 'expanded)
(set-face-attribute 'variable-pitch nil :family "Consolas" :height 130 :weight 'regular)

(defvar default-file-name-handler-alist file-name-handler-alist)

(setq gc-cons-percentage 0.6
      gc-cons-threshold most-positive-fixnum
      file-name-handler-alist nil
      inhibit-default-init t
      inhibit-message t
      inhibit-redisplay t
      inhibit-startup-buffer-menu t
      inhibit-startup-echo-area-message ""
      inhibit-startup-screen t
      initial-scratch-message nil
      load-prefer-newer noninteractive
      package-native-compile t
      idle-update-delay 1.1
      site-run-file nil
      package-install-upgrade-built-in t
      native-comp-speed 3
      native-comp-debug 0
      native-comp-verbose 0
      native-comp-deferred-compilation nil
      native-comp-async-report-warnings-errors nil
      native-comp-async-jobs-number 20
      native-comp-jit-compilation t
      native-comp-enable-subr-trampolines t
      native-comp-driver-options '("-Ofast" "-g0" "-fno-finite-math-only")
      native-comp-compiler-options '("-Ofast" "-g0" "-fno-finite-math-only")
      native-comp-always-compile t
      warning-minimum-level :warning
      byte-compile-warnings '(not obsolete)
      warning-suppress-log-types '((comp) (bytecomp))
      native-comp-async-report-warnings-errors 'silent)

(setenv "PATH" (concat (getenv "PATH") ":~/.dotnet:~/.dotnet/tools:~/.cabal/bin:~/.ghcup/bin:~/.local/bin:/opt/homebrew/bin"))
(setq exec-path (append exec-path '("~/.dotnet" "~/.dotnet/tools" "~/.cabal/bin" "~/.ghcup/bin" "~/.local/bin" "/opt/homebrew/bin")))

(defun emacs-startup ()
  (setq file-name-handler-alist default-file-name-handler-alist
        gc-cons-threshold (* 1024 1024 64)
        inhibit-redisplay nil
        inhibit-message nil)
  (makunbound 'default-file-name-handler-alist))

(add-hook 'emacs-startup-hook #'emacs-startup 100)

(provide 'early-init)
;;; early-init.el ends here
