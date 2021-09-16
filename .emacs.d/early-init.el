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

(setq package-enable-at-startup nil
      package-quickstart t)

(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

(setq locale-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
(set-selection-coding-system 'utf-8-unix)

;; GC, JIT and native compilation setup.
(defvar file-name-handler-alist-old file-name-handler-alist)

(defvar default-gc-cons-threshold)
(setq default-gc-cons-threshold 67108864) ; 64mb

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      frame-inhibit-implied-resize t
      file-name-handler-alist nil
      inhibit-compacting-font-caches t
      ffap-machine-p-known 'reject
      auto-window-vscroll nil
      read-process-output-max (* 1024 1024 3)
      message-log-max 16384
      idle-update-delay 2
      jit-lock-defer-time 0.05
      jit-lock-stealth-time 0.2
      jit-lock-stealth-verbose nil
      fast-but-imprecise-scrolling t)
;;      max-lisp-eval-depth most-positive-fixnum
;;      max-specpdl-size most-positive-fixnum)

(when (and (fboundp 'native-comp-available-p) (native-comp-available-p))
      (setq native-comp-speed 3
            native-comp-deferred-compilation t
            native-comp-async-jobs-number 20
            native-comp-driver-options '("-march=native" "-mtune=native" "-Ofast" "-g0" "-fno-finite-math-only")
            native-comp-compiler-options '("-march=native" "-mtune=native" "-Ofast" "-g0" "-fno-finite-math-only")
            native-comp-always-compile t))


(add-hook 'emacs-startup-hook
          `(lambda ()
             (setq file-name-handler-alist file-name-handler-alist-old
                   gc-cons-threshold default-gc-cons-threshold ; 64mb
                   gc-cons-percentage 0.1)
             (garbage-collect)) t)

;; GC automatically while unfocusing the frame
;; `focus-out-hook' is obsolete since 27.1
(if (boundp 'after-focus-change-function)
    (add-function :after after-focus-change-function
                  (lambda ()
                    (unless (frame-focus-state)
                      (garbage-collect))))
  (add-hook 'focus-out-hook 'garbage-collect))

(defun defer-garbage-collection-h ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun restore-garbage-collection-h ()
  (run-at-time 1 nil (lambda () (setq gc-cons-threshold default-gc-cons-threshold))))

(add-hook 'minibuffer-setup-hook #'defer-garbage-collection-h)
(add-hook 'minibuffer-exit-hook #'restore-garbage-collection-h)

(provide 'early-init)
;;; early-init.el ends here
