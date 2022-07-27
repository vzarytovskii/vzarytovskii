;;; early-init.el --- Emacs 27+ early init. -*- lexical-binding: t; -*-
;;; Commentary:
;; Emacs 27+ early init, for main configuration, see config.el.

;;; Code:

;; Some basic checks:

(if (and (fboundp 'native-comp-available-p)
         (native-comp-available-p))
    (message "Native compilation is available")
  (message "Native compilation is *not* available"))

(if (functionp 'json-serialize)
    (message "Native JSON is available")
  (message "Native JSON is *not* available"))

(defadvice display-warning
    (around no-warn-.emacs.d-in-load-path (type message &rest unused) activate)
  "Ignore the warning about the `.emacs.d' directory being in `load-path'."
  (unless (and (eq type 'initialization)
               (string-prefix-p "Your `load-path' seems to contain\nyour `.emacs.d' directory"
                                message t))
    ad-do-it))

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s seconds with %d garbage collections."
                     (emacs-init-time "%.2f")
                     gcs-done)))

;; GC Setup (ported over from GCMH)
(defvar gc-low-cons-threshold (* 1024 1024 1024))
(defvar gc-high-cons-threshold most-positive-fixnum)
(defvar gc-auto-idle-delay-factor 1000)
(defvar gc-idle-delay 'auto)
(defvar gc-idle-timer nil)
(defvar gc-last-gc-time 0.1)
(defvar file-name-handler-alist-old file-name-handler-alist)

(defmacro gc-time (&rest body)
  "Measure and return the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (float-time (time-since time))))

(defun gc-set-highest-threshold ()
  "Set high gc threshold."
  (setq gc-cons-percentage 1.0)
  (setq gc-cons-threshold gc-high-cons-threshold))

(defun gc-set-high-threshold ()
  "Set high gc threshold."
  (setq gc-cons-percentage 0.7)
  (setq gc-cons-threshold gc-high-cons-threshold))

(defun gc-set-low-threshold ()
  "Set high gc threshold."
  (setq gc-cons-percentage 0.5)
  (setq gc-cons-threshold gc-low-cons-threshold))

(defun gc-register-idle-gc ()
  "Register a timer to run `gc-idle-garbage-collect'.  Cancel the previous one if present."
  (let ((idle-t (if (eq gc-idle-delay 'auto)
                    (* gc-auto-idle-delay-factor gc-last-gc-time)
                  gc-idle-delay)))
    (if (timerp gc-idle-timer)
      	(timer-set-time gc-idle-timer idle-t)
      (setq gc-idle-timer
            (run-with-timer idle-t nil #'gc-idle-garbage-collect)))))

(defun gc-idle-garbage-collect ()
  "Run garbage collection after `gc-idle-delay'."
  ;; (message "Running idle GC: %s" (current-time))
  (setf gc-last-gc-time (gc-time (garbage-collect)))
  (gc-set-low-threshold))

(gc-set-highest-threshold)

(setq file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
          `(lambda ()
             (setq file-name-handler-alist file-name-handler-alist-old)
             (add-hook 'minibuffer-setup-hook #'gc-set-high-threshold)
             (add-hook 'minibuffer-exit-hook #'gc-register-idle-gc)
             (add-hook 'pre-command-hook #'gc-set-high-threshold)
             (add-hook 'post-command-hook #'gc-register-idle-gc)))


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

(setq garbage-collection-messages t
      comp-deferred-compilation nil
      frame-inhibit-implied-resize t
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

(when (and (fboundp 'native-comp-available-p) (native-comp-available-p))
  (setq native-comp-speed 3
        native-comp-deferred-compilation nil
        native-comp-async-jobs-number 20
        native-comp-driver-options '("-march=skylake" "-mtune=native" "-Ofast" "-g0" "-fno-finite-math-only")
        native-comp-compiler-options '("-march=skylake" "-mtune=native" "-Ofast" "-g0" "-fno-finite-math-only")
        native-comp-always-compile t))

(provide 'early-init)
;;; early-init.el ends here
