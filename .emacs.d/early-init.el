;;; -*- lexical-binding: t; -*- no-byte-compile: t; -*-

;;; Commentary:
;; Emacs 27+ early init, for main configuration, see init.el.

;;; Code:

(let ((minver 27))
  (unless (>= emacs-major-version minver)
    (error "Your Emacs is too old -- this configuration requires v%s or higher" minver)))

;; Some basic checks:

(if (and (fboundp 'native-comp-available-p)
         (native-comp-available-p))
    (message "Native compilation is available")
  (message "Native compilation is *not* available"))

(if (functionp 'json-serialize)
    (message "Native JSON is available")
  (message "Native JSON is *not* available"))

(with-eval-after-load 'package
  (setopt package-enable-at-startup nil))

(defun display-startup-echo-area-message ()
  (message ""))

(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(fullscreen . fullheight))
(add-to-list 'default-frame-alist '(undecorated . t))
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'set-scroll-bar-mode) (set-scroll-bar-mode nil))
(setq-default mode-line-format nil)

;; LSP Plists (does it have to be in the early-init?)
(setenv "LSP_USE_PLISTS" "true")

;; macOS related
(setq ns-use-native-fullscreen t
      ns-use-thin-smoothing t
      ns-pop-up-frames nil)
(defun homebrew-gcc-paths ()
  "Return GCC library paths from Homebrew installations.
Detects paths for gcc and libgccjit packages to be used in LIBRARY_PATH."
  (let* ((paths '())
         (brew-bin (or (executable-find "brew")
                       (let ((arm-path "/opt/homebrew/bin/brew")
                             (intel-path "/usr/local/bin/brew"))
                         (cond
                          ((file-exists-p arm-path) arm-path)
                          ((file-exists-p intel-path) intel-path))))))

    (when brew-bin
      ;; Get gcc paths.
      (let* ((gcc-prefix (string-trim
                          (shell-command-to-string
                           (concat brew-bin " --prefix gcc"))))
             (gcc-lib-current (expand-file-name "lib/gcc/current" gcc-prefix)))
        (push gcc-lib-current paths)

        ;; Find apple-darwin directory.
        (let* ((default-directory gcc-lib-current)
               (arch-dirs (file-expand-wildcards "gcc/*-apple-darwin*/*[0-9]")))
          (when arch-dirs
            (push (expand-file-name
                   (car (sort arch-dirs #'string>)))
                  paths))))

      ;; Get libgccjit paths
      (let* ((jit-prefix (string-trim
                          (shell-command-to-string
                           (concat brew-bin " --prefix libgccjit"))))
             (jit-lib-current (expand-file-name "lib/gcc/current" jit-prefix)))
        (push jit-lib-current paths)))

    (nreverse paths)))

(defun setup-macos-native-comp-library-paths ()
  "Set up LIBRARY_PATH for native compilation on macOS.
Includes Homebrew GCC paths and CommandLineTools SDK libraries."
  (let* ((existing-paths (split-string (or (getenv "LIBRARY_PATH") "") ":" t))
         (gcc-paths (homebrew-gcc-paths))
         (clt-paths '("/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/lib"))
         (unique-paths (delete-dups
                        (append existing-paths gcc-paths clt-paths))))

    (setenv "LIBRARY_PATH" (mapconcat #'identity unique-paths ":"))))

;; Set up library paths for native compilation on macOS.
(when (eq system-type 'darwin)
  (setup-macos-native-comp-library-paths))



(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

(setq locale-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-language-environment "UTF-8")

(setopt bidi-inhibit-bpa t)
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

(defvar default-font-name "Hack")
(set-face-attribute 'default nil :family default-font-name :height 140)
(set-face-attribute 'fixed-pitch nil :family default-font-name :height 130 :weight 'semi-light :width 'expanded)
(set-face-attribute 'variable-pitch nil :family default-font-name :height 130 :weight 'regular)

(defvar default-file-name-handler-alist file-name-handler-alist)
(defvar default-vc-handled-backends vc-handled-backends)

(defvar gc-cons-highest-threshold most-positive-fixnum)
(defvar gc-cons-default-threshold (* 1024 1024 100))

(defvar gc-cons-higher-percentage 1.0)
(defvar gc-cons-default-percentage 0.1)

(setq gc-cons-percentage gc-cons-higher-percentage
      gc-cons-threshold gc-cons-highest-threshold
      garbage-collection-messages t
      file-name-handler-alist nil
      vc-handled-backends nil
      inhibit-default-init t
      inhibit-message t
      inhibit-redisplay t
      inhibit-startup-buffer-menu t
      inhibit-startup-echo-area-message ""
      inhibit-startup-screen t
      initial-scratch-message nil
      load-prefer-newer noninteractive
      package-native-compile t
      idle-update-delay 1.1
      site-run-file nil
      package-install-upgrade-built-in t
      compilation-safety 0
      native-comp-speed 3
      native-comp-debug 0
      native-comp-verbose 0
      native-comp-deferred-compilation nil
      native-comp-async-report-warnings-errors nil
      native-comp-async-jobs-number 24
      native-comp-async-on-battery-power nil
      native-comp-jit-compilation t
      native-comp-enable-subr-trampolines t
      native-comp-driver-options '("-Ofast" "-g0" "-fno-finite-math-only")
      native-comp-compiler-options '("-Ofast" "-g0" "-fno-finite-math-only")
      native-comp-always-compile t
      warning-minimum-level :warning
      byte-compile-warnings '(not obsolete)
      warning-suppress-log-types '((comp) (bytecomp))
      native-comp-async-report-warnings-errors 'silent)

(setenv "PATH" (concat (getenv "PATH") ":~/.cargo/bin:~/.dotnet:~/.dotnet/tools:~/.cabal/bin:~/.ghcup/bin:~/.local/bin:/opt/homebrew/bin:/usr/local/share/dotnet:~/.dotnet/tools"))
(setq exec-path (append exec-path '("~/.cargo/bin" "~/.dotnet" "~/.dotnet/tools" "~/.cabal/bin" "~/.ghcup/bin" "~/.local/bin" "/opt/homebrew/bin" "/usr/local/share/dotnet" "~/.dotnet/tools")))

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
(run-with-idle-timer 30 t 'garbage-collect)
(add-function :after after-focus-change-function (lambda ()
  (unless (frame-focus-state)
    (run-with-timer 3.0 nil (lambda ()
      (unless (frame-focus-state)
        (let (garbage-collection-messages)
          (garbage-collect))))))))

;; Experimental opportunistic GC:
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
;;; early-init.el ends here
