;;; packaging.el --- Emacs packaging setup.
;;; Commentary:
;; Emacs packaging setup, for main configuration, see config.el

;;; -*- lexical-binding: t -*-

;;; Code:


;; Setup package system
(require 'package)

(setq package-archives
      '(("melpa" . "http://melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")
        ("gnu" . "https://elpa.gnu.org/packages/")))

(setq package-archive-priorities
      '(("melpa" .  3)
        ("org" . 2)
        ("gnu" . 1)))

(require 'tls)

(setq gnutls-verify-error (not (getenv "INSECURE"))
      tls-checktrust gnutls-verify-error
      tls-program (list "gnutls-cli --x509cafile %t -p %p %h"
                        ;; compatibility fallbacks
                        "gnutls-cli -p %p %h"
                        "openssl s_client -connect %h:%p -no_ssl2 -no_ssl3 -ign_eof"))

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

;;; package manager bootstrap
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
  ;; catch emacs updates that have native compiled leftovers
  ;; Credits: https://github.com/raxod502/straight.el/643/issues
  (unless (catch 'emacs-version-changed
            (load bootstrap-file nil 'nomessage))
    (when (boundp 'comp-eln-load-path)
      ;; remove leftovers, with confirmation just to be safe
      (when (yes-or-no-p (format "Delete '%s'?" (car comp-eln-load-path)))
        (delete-directory (expand-file-name (car comp-eln-load-path)) t))
      ;; and try again
      (load bootstrap-file nil 'nomessage))))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
    (straight-use-package 'use-package))


;; Setup use-package
(setq-default use-package-always-demand t
              use-package-always-defer nil
              use-package-always-ensure nil
              use-package-expand-minimally nil
              use-package-enable-imenu-support t
              use-package-compute-statistics nil
	      use-package-hook-name-suffix nil)

(eval-when-compile
  (require 'use-package))

(provide 'packaging)
;;; packaging.el ends here
