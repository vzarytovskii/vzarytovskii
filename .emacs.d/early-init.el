;;; -*- lexical-binding: t; -*- no-byte-compile: t; -*-

(defun reload-init-file ()
  (interactive)
  (load-file user-init-file))

(defun emacs-recompile ()
  "Recompile all .elc files."
  (interactive)
  (message "Recompiling ...")
  (if (functionp 'async-byte-recompile-directory)
      (async-byte-recompile-directory package-user-dir)
    (byte-recompile-directory package-user-dir 0 'force)))

(defun add-to-list-many (list-var elements)
  (dolist (element elements)
    (add-to-list list-var element)))

(defmacro defconsts (&rest definitions)
  `(progn
     ,@(mapcar (lambda (def)
                 `(defconst ,(car def) ,(cadr def)))
               definitions)))

(defmacro set-face-attributes (&rest definitions)
  `(progn
     ,@(mapcar (lambda (def)
                 `(set-face-attribute ',(car def) nil ,@(cdr def)))
               definitions)))

(if (and (fboundp 'native-comp-available-p)
         (native-comp-available-p))
    (message "Native compilation is available")
  (message "Native compilation is *not* available"))

(if (functionp 'json-serialize)
    (message "Native JSON is available")
  (message "Native JSON is *not* available"))

(defconsts
  (default-font-name "Hack")
  (default-file-name-handler-alist file-name-handler-alist)
  (default-vc-handled-backends vc-handled-backends)
  (gc-cons-highest-threshold most-positive-fixnum)
  (gc-cons-default-threshold (* 1024 1024 100))
  (gc-cons-higher-percentage 1.0)
  (gc-cons-default-percentage 0.1)
  (gc-idle-timer 30)
  (gc-focus-blur-timer 3.0)
  (*sys/gui* (display-graphic-p))
  (*sys/is-mac* (eq system-type 'darwin))
  (*sys/is-linux* (or (eq system-type 'gnu/linux) (eq system-type 'linux)))
  (*sys/is-unix* (or *sys/is-linux* (eq system-type 'usg-unix-v) (eq system-type 'berkeley-unix)))
  (*sys/is-windows* (or (eq system-type 'ms-dos) (eq system-type 'windows-nt)))
  (*sys/is-cygwin* (eq system-type 'cygwin))
  (*sys/is-wsl* (and *sys/is-linux* (getenv "WSLENV")))
  (my-extra-paths
  '("~/.cargo/bin"
    "~/.dotnet"
    "~/.dotnet/tools"
    "~/.cabal/bin"
    "~/.ghcup/bin"
    "~/.local/bin"
    "/opt/homebrew/bin"
    "/usr/local/share/dotnet")
  "Additional paths to add to PATH and `exec-path'."))

(let ((expanded-paths (mapcar #'expand-file-name my-extra-paths)))
  (setenv "PATH" (concat (getenv "PATH") ":" (string-join expanded-paths ":")))
  (setq exec-path (append exec-path expanded-paths)))

(set-face-attributes
  (default :family default-font-name :height 140)
  (fixed-pitch :family default-font-name :height 130 :weight 'semi-light :width 'expanded)
  (variable-pitch :family default-font-name :height 130 :weight 'regular))

(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(add-to-list-many 'default-frame-alist
                  '((fullscreen . fullheight)
                    (undecorated . t)
                    (menu-bar-lines . 0)
                    (tool-bar-lines . 0)
                    (vertical-scroll-bars)))

(when (featurep 'ns)
  (push '(ns-transparent-titlebar . t) default-frame-alist))

(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'set-scroll-bar-mode) (set-scroll-bar-mode nil))

(when (display-graphic-p)
  (setq-default x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))
(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

(setopt bidi-inhibit-bpa t)
(setq-default package-enable-at-startup nil
              package-quickstart nil
              locale-coding-system 'utf-8

              bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right

              ;mode-line-format nil

              ns-use-native-fullscreen t
              ns-use-thin-smoothing t
              ns-pop-up-frames nil

              gc-cons-percentage gc-cons-higher-percentage
              gc-cons-threshold gc-cons-highest-threshold
              garbage-collection-messages t

              file-name-handler-alist nil

              vc-handled-backends nil

              read-process-output-max (* 1024 1024)

              inhibit-default-init t
              inhibit-message t
              inhibit-redisplay t
              inhibit-startup-buffer-menu t
              inhibit-startup-echo-area-message ""
              inhibit-startup-screen t
              inhibit-splash-screen t

              frame-inhibit-implied-resize t

              initial-scratch-message nil

              load-prefer-newer noninteractive
              idle-update-delay 1.1

              site-run-file nil

              compilation-safety 0

              native-comp-speed 3
              native-comp-debug 0
              native-comp-verbose 0
              native-comp-async-report-warnings-errors nil
              native-comp-async-jobs-number 24
              native-comp-async-on-battery-power nil
              native-comp-jit-compilation t
              native-comp-enable-subr-trampolines t
              native-comp-driver-options '("-Ofast" "-g0" "-fno-finite-math-only" "-fgraphite-identity" "-floop-nest-optimize" "-floop-parallelize-all" "-ftree-parallelize-loops=4" "-fdevirtualize-at-ltrans" "-fipa-pta" "-fno-semantic-interposition" "-flto=auto")
              native-comp-compiler-options '("-Ofast" "-g0" "-fno-finite-math-only" "-fgraphite-identity" "-floop-nest-optimize" "-floop-parallelize-all" "-ftree-parallelize-loops=4" "-fdevirtualize-at-ltrans" "-fipa-pta" "-fno-semantic-interposition" "-flto=auto")
              native-comp-deferred-compilation t
              native-comp-always-compile t
              native-comp-async-query-on-exit t
              native-comp-async-report-warnings-errors 'silent

              package-native-compile t

              confirm-kill-processes t

              warning-minimum-level :warning
              byte-compile-warnings '(not obsolete)
              warning-suppress-log-types '((comp) (bytecomp))

              window-min-height 1
              window-min-width 2)


;; We want to avoid GC for as long as we can, so when we are in minibuffer (consult, ivy, helm, etc), set percentage to highest (100%) and memory to the max int
(defun my-minibuffer-setup-hook ()
   (setq gc-cons-threshold gc-cons-highest-threshold
         gc-cons-percentage gc-cons-higher-percentage))
(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)

;; When we exit, we re-set those settings to defaults ones
(defun my-minibuffer-exit-hook ()
   (setq gc-cons-threshold gc-cons-default-threshold
         gc-cons-percentage gc-cons-default-percentage))
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

;; On startup, set the sane defaults
(defun emacs-startup ()
  (setq file-name-handler-alist default-file-name-handler-alist
        vc-handled-backends default-vc-handled-backends
        gc-cons-percentage gc-cons-default-percentage
        gc-cons-threshold gc-cons-default-threshold
        inhibit-redisplay nil
        inhibit-message nil)
  (makunbound 'default-file-name-handler-alist))

(add-hook 'emacs-startup-hook #'emacs-startup 100)

;; We GC in two cases:
;; 1. If emacs is idling for more than 30s
;; 2. If emacs' window lost focus and didn't regain it in 3s
(run-with-idle-timer gc-idle-timer t 'garbage-collect)
(add-function :after after-focus-change-function (lambda ()
  (unless (frame-focus-state)
    (run-with-timer 3.0 nil (lambda ()
      (unless (frame-focus-state)
        (let (garbage-collection-messages)
          (garbage-collect))))))))

;; Experimental opportunistic GC (credit goes to Stefan Monnier - https://lists.gnu.org/archive/html/emacs-devel/2021-03/msg00393.html):
(defvar gc--opportunistic-last-gcs gcs-done)
(defvar gc--opportunistic-state 'noncmd)
(defvar gc--opportunistic-counters nil)
(defvar gc--opportunistic-single-gc-cmds 0)

(defun gc--check ()
  (let ((gcs-counted
         (+ (alist-get 'multi-gcs gc--opportunistic-counters 0)
                   (alist-get 'earlier-gcs gc--opportunistic-counters 0)
                   (alist-get 'single-gc-cmds gc--opportunistic-counters 0)
                   (alist-get 'noncmds-gcs gc--opportunistic-counters 0)
                   (alist-get 'opportunistic-gcs gc--opportunistic-counters 0)
                   (or (car (alist-get 'error-gcs gc--opportunistic-counters))
0))))
    (unless (= gcs-done gcs-counted)
      (push (+ (- gcs-done gcs-counted)
               (or (car (alist-get 'error-gcs gc--opportunistic-counters)) 0))
            (alist-get 'error-gcs gc--opportunistic-counters)))))

(defun gc--opportunistic-record (nextstate)
  (let ((counts (alist-get gc--opportunistic-state gc--opportunistic-counters)))
    (unless counts
      (setf (alist-get gc--opportunistic-state gc--opportunistic-counters)
            (setq counts (list 0 0 0))))
    (pcase (prog1 (- gcs-done gc--opportunistic-last-gcs)
             (setq gc--opportunistic-last-gcs gcs-done))
      ((pred (>= 0)) nil)
      (1 (cl-incf (nth 0 counts)))
      (gcs (cl-incf (nth 1 counts))
           (cl-incf (nth 2 counts) gcs))))
  (setq gc--opportunistic-state nextstate))

(defun gc--opportunistic-postch ()
  (cl-incf (alist-get 'commands gc--opportunistic-counters 0))
  (gc--opportunistic-record 'noncmd))

(defun gc--opportunistic-prech ()
  (cl-callf identity
      (alist-get 'earlier-gcs gc--opportunistic-counters gcs-done))
  (gc--opportunistic-record 'cmd)
  ;; (gc--check)
  )

(defun gc--opportunistic-jitlock (orig-fun start)
  (if (eq gc--opportunistic-state 'cmd)
      ;; Count jit-lock execution which happens during a command as
      ;; being part of command execution rather than as part of jit-lock!
      (funcall orig-fun start)
    (let ((gc--opportunistic-state gc--opportunistic-state))
      (gc--opportunistic-record 'jit)
      (unwind-protect
          (funcall orig-fun start)
        (gc--opportunistic-record 'postjit)))))

(add-hook 'pre-command-hook #'gc--opportunistic-prech)
(add-hook 'post-command-hook #'gc--opportunistic-postch)
(advice-add 'jit-lock-function :around #'gc--opportunistic-jitlock)

(defun gc--opportunistic ()
  "Run the GC during idle time."
  ;; This is good for two reasons:
  ;; - It reduces the number of times we have to GC in the middle of
  ;;   an operation.
  ;; - It means we GC when the C stack is short, reducing the risk of false
  ;;   positives from the conservative stack scanning.
  (when (garbage-collect-maybe 3)
    (cl-incf (alist-get 'opportunistic-gcs gc--opportunistic-counters 0))
    ;; Don't double count this GC in other categories.
    (cl-incf gc--opportunistic-last-gcs)
    ;; Recalibrate the timer.
    (cancel-function-timers #'gc--opportunistic)
    (run-with-idle-timer
     ;; FIXME: Magic formula!
     (+ 1 (* 10 (/ gc-elapsed gcs-done))) t #'gc--opportunistic)))

(defun gc--opportunistic-score ()
  "Show the current counters's that keep track of GC behavior."
  (interactive)
  (message "%S" gc--opportunistic-counters))

(run-with-idle-timer 1 t #'gc--opportunistic)
(let ((last-gcs nil))
      (add-hook 'pre-command-hook (lambda () (setq last-gcs gcs-done)))
      (add-hook 'post-command-hook
                (lambda ()
                  (if (eq (1- gcs-done) last-gcs)
                      (setq gc--opportunistic-single-gc-cmds
                            (1+ gc--opportunistic-single-gc-cmds))))))
;;


(provide 'early-init)
