;;; early-init.el --- Emacs 27+ early init.
;;; Commentary:
;; Emacs 27+ early init, for main configuration, see config.el.

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

(setq package-enable-at-startup t
      package-quickstart t
      frame-inhibit-implied-resize t)

(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

(setq locale-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
(set-selection-coding-system 'utf-8-unix)

(blink-cursor-mode -1)
(column-number-mode t)
(global-display-line-numbers-mode 1)
(global-subword-mode t)
(horizontal-scroll-bar-mode -1)
(line-number-mode +1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(size-indication-mode t)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(add-to-list 'default-frame-alist
             '(vertical-scroll-bars . nil))

(fset 'yes-or-no-p 'y-or-n-p)

;; GC, JIT and native compilation setup.
(defvar file-name-handler-alist-old file-name-handler-alist)
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      file-name-handler-alist nil
      inhibit-compacting-font-caches t
      auto-window-vscroll nil
      read-process-output-max (* 1024 1024 3)
      message-log-max 16384
      idle-update-delay 2
      jit-lock-defer-time 0
      jit-lock-stealth-time 0.2
      jit-lock-stealth-verbose nil
      fast-but-imprecise-scrolling t
      comp-speed 3
      comp-deferred-compilation t
      comp-async-jobs-number 12
      comp-native-driver-options
      '("-march=native" "-Ofast" "-g0" "-fno-finite-math-only")
      comp-always-compile t)

(add-hook 'emacs-startup-hook
          `(lambda ()
             (setq file-name-handler-alist file-name-handler-alist-old
                   gc-cons-threshold 67108864 ; 64mb
                   gc-cons-percentage 0.1)
             (garbage-collect)) t)

(defun defer-garbage-collection-h ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun restore-garbage-collection-h ()
  (run-at-time 1 nil (lambda () (setq gc-cons-threshold 67108864))))

(add-hook 'minibuffer-setup-hook #'defer-garbage-collection-h)
(add-hook 'minibuffer-exit-hook #'restore-garbage-collection-h)

(provide 'early-init)
;;; early-init.el ends here
