;;; packaging.el --- Emacs packaging setup. -*- lexical-binding: t; -*-
;;; Commentary:
;; Emacs packaging setup, for main configuration, see config.el

;;; Code:

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
                use-package-compute-statistics nil
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

(use-package use-package-ensure-system-package :ensure t)

(require 'tls)

(setq gnutls-verify-error (not (getenv "INSECURE"))
      tls-checktrust gnutls-verify-error
      tls-program (list "gnutls-cli --x509cafile %t -p %p %h"
                        ;; compatibility fallbacks
                        "gnutls-cli -p %p %h"
                        "openssl s_client -connect %h:%p -no_ssl2 -no_ssl3 -ign_eof"))

(provide 'packaging)
;;; packaging.el ends here
