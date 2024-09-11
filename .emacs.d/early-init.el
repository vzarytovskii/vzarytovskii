;;; early-init.el --- Emacs 27+ early init. -*- lexical-binding: t; -*-
;;; Commentary:
;; Emacs 27+ early init, for main configuration, see init.el.

;;; Code:

(let ((minver 26))
  (unless (>= emacs-major-version minver)
    (error "Your Emacs is too old -- this configuration requires v%s or higher" minver)))

(unless (>= emacs-major-version 27)
  (message "Early init: Emacs Version < 27.0")
  (load (expand-file-name "early-init.el" user-emacs-directory)))

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
(set-default-coding-systems 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
(set-selection-coding-system 'utf-8-unix)

(when (and (fboundp 'native-comp-available-p) (native-comp-available-p))
  (setq native-comp-speed 3
        native-comp-debug 0
        native-comp-verbose 0
        native-comp-deferred-compilation nil
        native-comp-async-report-warnings-errors nil
        native-comp-async-jobs-number 20
        native-comp-jit-compilation nil
        native-comp-enable-subr-trampolines t
        native-comp-driver-options '("-Ofast" "-g0" "-fno-finite-math-only")
        native-comp-compiler-options '("-Ofast" "-g0" "-fno-finite-math-only")
        native-comp-always-compile t
        warning-minimum-level :error))


(provide 'early-init)
;;; early-init.el ends here
