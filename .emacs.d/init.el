;;; init.el --- Emacs configuration bootstrap
;;; Commentary:
;; Emacs bootstrap, for main configuration, see config.el

;;; -*- lexical-binding: t -*-

;;; Code:

;; GC and JIT

(setq inhibit-compacting-font-caches t)

(setq gc-cons-threshold most-positive-fixnum
      file-name-handler-alist nil
      gc-cons-percentage 0.6
      auto-window-vscroll nil
      read-process-output-max (* 1024 1024 3)
      message-log-max 16384
      idle-update-delay 2
      jit-lock-defer-time 0
      jit-lock-stealth-time 0.2
      jit-lock-stealth-verbose nil)

;; Native compilation support via libgccjit (only available in feature/nativecom branch).
(setq comp-deferred-compilation t)

(defvar file-name-handler-alist-old file-name-handler-alist)
(add-hook 'emacs-startup-hook
          `(lambda ()
             (setq file-name-handler-alist file-name-handler-alist-old
                   gc-cons-threshold 800000
                   gc-cons-percentage 0.1)
             (garbage-collect)) t)


;; Setup straight.el
(setq-default straight-repository-branch "develop"
              straight-use-package-by-default t
              straight-check-for-modifications '(watch-files find-when-checking)
              straight-disable-autoloads nil
              straight-disable-byte-compilation nil
              straight-disable-native-compilation nil
              straight-use-symlinks t
              straight-cache-autoloads t
              straight-vc-git-default-branch "master"
              straight-vc-git-default-remote-name "origin"
              straight-fix-flycheck t)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

;; Setup use-package
(setq-default use-package-always-demand t
              use-package-always-defer nil
              use-package-always-ensure nil)

(eval-when-compile
  (require 'use-package))

;; Encoding
(setq locale-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

;; PATH Setup
;; TODO: Properly get it either from list, or user's shell
(setenv "PATH" (concat (getenv "PATH") ":~/.dotnet/"))
(setq exec-path (append exec-path '("~/.dotnet/")))

;; Load actual config
(load "~/.emacs.d/config.el")

(provide 'init)
;;; init.el ends here
