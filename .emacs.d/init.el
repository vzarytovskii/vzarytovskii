;;; init.el --- Emacs configuration bootstrap
;;; Commentary:
;; Emacs bootstrap, for main configuration, see config.el

;;; -*- lexical-binding: t -*-

;;; Code:

;; Some basic checks:
(if (and (fboundp 'native-comp-available-p)
       (native-comp-available-p))
  (message "Native compilation is available")
  (message "Native complation is *not* available"))

(if (functionp 'json-serialize)
  (message "Native JSON is available")
  (message "Native JSON is *not* available"))

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
(setq comp-deferred-compilation t
      gc-cons-threshold-custom 16777216)

(defvar file-name-handler-alist-old file-name-handler-alist)
(add-hook 'emacs-startup-hook
          `(lambda ()
             (setq file-name-handler-alist file-name-handler-alist-old
                   gc-cons-threshold gc-cons-threshold-custom
                   gc-cons-percentage 0.1)
             (garbage-collect)) t)

(defun defer-garbage-collection-h ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun restore-garbage-collection-h ()
  (run-at-time 1 nil (lambda () (setq gc-cons-threshold gc-cons-threshold-custom))))

(add-hook 'minibuffer-setup-hook #'defer-garbage-collection-h)
(add-hook 'minibuffer-exit-hook #'restore-garbage-collection-h)

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


(setenv "PATH" (concat (getenv "PATH") ":~/.dotnet:~/.cabal/bin:~/.ghcup/bin"))
(setq exec-path (append exec-path '("~/.dotnet" "~/.cabal/bin" "~/.ghcup/bin")))

;; Load actual config
(load "~/.emacs.d/config.el")

(provide 'init)
;;; init.el ends here.
