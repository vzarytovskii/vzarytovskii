;;; -*- lexical-binding: t; -*- no-byte-compile: t; -*-

(require 'package)

(add-to-list-many 'package-archives
                  '(("gnu" . "https://elpa.gnu.org/packages/")
                    ("gnu-devel" . "https://elpa.gnu.org/devel/")
                    ("melpa" . "https://stable.melpa.org/packages/")
                    ("melpa-devel" . "https://melpa.org/packages/")
                    ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(setq-default
              ad-redefinition-action 'accept
              custom-file (expand-file-name "custom.el" user-emacs-directory)
              gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"
              gnutls-verify-error (not (getenv "INSECURE"))
              load-prefer-newer t
              package-install-upgrade-built-in t
              package-quickstart t
              package-archive-priorities
              '(("melpa-devel" . 500)
                ("melpa" . 400)
                ("elpa" . 300)
                ("nongnu" . 200)
                ("gnu-devel" . 100)
                ("gnu" . 50))
              tls-checktrust gnutls-verify-error
              tls-program (list "gnutls-cli --x509cafile %t -p %p %h"
                        "gnutls-cli -p %p %h"
                        "openssl s_client -connect %h:%p -no_ssl2 -no_ssl3 -ign_eof"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (unless package-archive-contents
    (package-refresh-contents))
  (package-install 'use-package))

(require 'use-package)

(use-package use-package
  :demand t
  :defer nil
  :custom
  (use-package-always-ensure nil)
  (use-package-always-demand nil)
  (use-package-always-defer t)
  (use-package-always-pin nil)
  (use-package-vc-prefer-newest t)
  (use-package-expand-minimally nil)
  (use-package-enable-imenu-support t)
  (use-package-compute-statistics t)
  (use-package-hook-name-suffix nil)
  (use-package-verbose t))

(use-package compile-angel
  :ensure t
  :demand t

  :custom
  (compile-angel-verbose t)
  (compile-angel-enable-byte-compile t)
  (compile-angel-enable-native-compile t)

  :hook
  (emacs-lisp-mode-hook . compile-angel-on-save-local-mode)

  :preface
  (defun u/compile-angel-setup-exclusions ()
    "Set up compile-angel exclusions for various config files."
    (with-eval-after-load "savehist"
      (push (concat "/" (file-name-nondirectory savehist-file))
            compile-angel-excluded-files))

    (with-eval-after-load "recentf"
      (push (concat "/" (file-name-nondirectory recentf-save-file))
            compile-angel-excluded-files))

    (with-eval-after-load "cus-edit"
      (when (stringp custom-file)
        (push (concat "/" (file-name-nondirectory custom-file))
              compile-angel-excluded-files)))

    (push ".emacs.d/init.el" compile-angel-excluded-files)
    (push ".emacs.d/early-init.el" compile-angel-excluded-files)
    (push "lisp/subdirs.el" compile-angel-excluded-files))

  :config
  (u/compile-angel-setup-exclusions)
  (compile-angel-on-load-mode 1))

(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)