;;; -*- lexical-binding: t; -*- no-byte-compile: t; -*-

;;; Commentary:

;;; Code:

;; Setup package system

(setq-default use-package-always-demand nil
              use-package-always-defer t
              use-package-always-ensure t
              use-package-always-pin nil
              use-package-vc-prefer-newest t
              package-install-upgrade-built-in t
              use-package-expand-minimally nil
              use-package-enable-imenu-support t
              use-package-compute-statistics t
              use-package-hook-name-suffix nil
              use-package-verbose t
              package-install-upgrade-built-in t
              load-prefer-newer t
              ad-redefinition-action 'accept)

(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
      (build (expand-file-name "elpaca/" elpaca-builds-directory))
      (order (cdr elpaca-order))
      (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                ,@(when-let* ((depth (plist-get order :depth)))
                                                    (list (format "--depth=%d" depth) "--no-single-branch"))
                                                ,(plist-get order :repo) ,repo))))
                ((zerop (call-process "git" nil buffer t "checkout"
                                      (or (plist-get order :ref) "--"))))
                (emacs (concat invocation-directory invocation-name))
                ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                      "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                ((require 'elpaca))
                ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("gnu-devel" . "https://elpa.gnu.org/devel/") t)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)

(setq package-quickstart t
      gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"
      package-archive-priorities
      '(("melpa" .  500)
        ("melpa-stable" . 400)
        ("elpa" . 300)
        ("org" . 200)
        ("gnu-devel" . 100)
        ("gnu" . 50)))

;; Initialise the packages, avoiding a re-initialisation.
(unless (bound-and-true-p package--initialized)
  (package-initialize))


(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode)
  (setq elpaca-use-package-by-default t)  )

(setq elpaca-ignored-dependencies
      (delq 'transient elpaca-ignored-dependencies))

;; Setup use-package


(setq gnutls-verify-error (not (getenv "INSECURE"))
      tls-checktrust gnutls-verify-error
      tls-program (list "gnutls-cli --x509cafile %t -p %p %h"
                        ;; compatibility fallbacks
                        "gnutls-cli -p %p %h"
                        "openssl s_client -connect %h:%p -no_ssl2 -no_ssl3 -ign_eof"))

(setq vc-handled-backends (eval (car (get 'vc-handled-backends 'standard-value))))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

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

(defconst *sys/gui* (display-graphic-p))
(defconst *sys/is-mac* (eq system-type 'darwin))
(defconst *sys/is-linux* (or (eq system-type 'gnu/linux) (eq system-type 'linux)))
(defconst *sys/is-unix* (or *sys/is-linux* (eq system-type 'usg-unix-v) (eq system-type 'berkeley-unix)))
(defconst *sys/is-windows* (or (eq system-type 'ms-dos) (eq system-type 'windows-nt)))
(defconst *sys/is-cygwin* (eq system-type 'cygwin))
(defconst *sys/is-wsl* (and *sys/is-linux* (getenv "WSLENV")))

(when *sys/is-wsl*
  (setq browse-url-generic-program  "/mnt/c/Windows/System32/cmd.exe"
        browse-url-generic-args     '("/c" "start")
        browse-url-browser-function #'browse-url-generic))

(use-package compile-angel
  :demand t
  :config
  (setq compile-angel-verbose t
        compile-angel-enable-byte-compile t
        compile-angel-enable-native-compile t)

  (add-hook 'emacs-lisp-mode-hook #'compile-angel-on-save-local-mode)

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

  (push "lisp/subdirs.el" compile-angel-excluded-files)

  (compile-angel-on-load-mode 1))

(use-package esup)

(use-package cond-let
  :ensure (:host github :repo "tarsius/cond-let" :branch "main"))

(use-package llama)
(use-package emacsql)
(use-package closql :after emacsql)

(use-package hydra :ensure (:wait t))
(use-package posframe)

(use-package exec-path-from-shell
  :demand nil
  :when (eq system-type 'darwin)
  :hook (after-init-hook . exec-path-from-shell-initialize))

(use-package delight :ensure (:wait t))

(use-package transient)

;; Configure Emacs' defaults and keybinds;
(use-package emacs
  :ensure nil
  :delight auto-revert-mode
  :bind (("C-z"             . nil)
         ("C-x C-z"         . nil)
         ("C-h h"           . nil)
         ("<C-backspace>"   . nil)
         ([delete]          . 'delete-forward-char)
         ("C-w"             . 'backward-kill-word)
         ("M-w"             . 'copy-region-or-line)
         ("C-g"             . 'keyboard-quit)
         ("C-k"             . 'kill-buffer)
         ("C-S-k"           . 'kill-buffer-and-window)
         ("C-c o"           . 'switch-to-minibuffer)
         ([remap keyboard-quit] . 'keyboard-quit-ex))
  :hook (after-init-hook . window-divider-mode)
  :config
  :delight lisp-interaction-mode
  :preface
  (defun kill-buffer-and-window (&optional arg)
    "Kill the current buffer. If there is more than one window on the current frame, also delete the selected window. Otherwise, just kill the buffer."
      (interactive)
      (let ((kill-window-p (> (count-windows) 1)))
        (kill-this-buffer)
        (when kill-window-p
          (delete-window))))
  (defun keyboard-quit-ex ()
    (interactive)
    (cond
    ((region-active-p)
      (keyboard-quit))
    ((derived-mode-p 'completion-list-mode)
      (delete-completion-window))
    ((> (minibuffer-depth) 0)
      (abort-recursive-edit))
    (t
      (keyboard-quit))))
  (defun switch-to-minibuffer ()
    "Switch to minibuffer window."
    (interactive)
    (if (active-minibuffer-window)
        (select-window (active-minibuffer-window))
      (error "Minibuffer is not active")))

  (defun kill-this-buffer ()
    "Kill the current buffer."
    (interactive)
    (kill-buffer nil))

  (defun flash-mode-line ()
    (invert-face 'mode-line)
    (run-with-timer 0.1 nil #'invert-face 'mode-line))

  (defun vsplit-last-buffer ()
    "Split the window vertically and display the previous buffer."
    (interactive)
    (split-window-vertically)
    (other-window 1 nil)
    (switch-to-next-buffer))

  (defun vsplit-current-buffer ()
    "Split the window vertically and display the current buffer."
    (interactive)
    (split-window-vertically)
    (other-window 1 nil))

  (defun hsplit-last-buffer ()
    "Split the window horizontally and display the previous buffer."
    (interactive)
    (split-window-horizontally)
    (other-window 1 nil)
    (switch-to-next-buffer))

  (defun hsplit-current-buffer ()
    "Split the window horizontally and display the current buffer."
    (interactive)
    (split-window-horizontally)
    (other-window 1 nil))

  (defun toggle-window-split ()
    (interactive)
    (if (= (count-windows) 2)
        (let* ((this-win-buffer (window-buffer))
               (next-win-buffer (window-buffer (next-window)))
               (this-win-edges (window-edges (selected-window)))
               (next-win-edges (window-edges (next-window)))
               (this-win-2nd (not (and (<= (car this-win-edges)
                                           (car next-win-edges))
                                       (<= (cadr this-win-edges)
                                           (cadr next-win-edges)))))
               (splitter
                (if (= (car this-win-edges)
                       (car (window-edges (next-window))))
                    'split-window-horizontally
                  'split-window-vertically)))
          (delete-other-windows)
          (let ((first-win (selected-window)))
            (funcall splitter)
            (if this-win-2nd (other-window 1))
            (set-window-buffer (selected-window) this-win-buffer)
            (set-window-buffer (next-window) next-win-buffer)
            (select-window first-win)
            (if this-win-2nd (other-window 1))))))

  (defun copy-region-or-line (beg end)
    (interactive "r")
    (if mark-active
        (kill-ring-save beg end)
      (kill-ring-save (line-beginning-position) (line-end-position))))
  :custom
  (enable-recursive-minibuffers t)
  (read-extended-command-predicate #'command-completion-default-include-p)
  :config
  ;; Defaults

  (setq-default major-mode 'text-mode
                use-file-dialog nil
                use-dialog-box nil
                cursor-type 'box
                x-stretch-cursor t
                cursor-in-non-selected-window nil
                indent-tabs-mode nil
                tab-width 4
                select-enable-primary t
                select-enable-clipboard t)

  (fset 'yes-or-no-p 'y-or-n-p)

  (setq scroll-margin 0
        scroll-step 1
        scroll-conservatively 100000
        scroll-preserve-screen-position 1
        scroll-preserve-screen-position 'always
        hscroll-step 2
        hscroll-margin 2

        next-line-add-newlines nil

        byte-compile-warnings '(cl-functions)
        visible-bell nil
        ring-bell-function 'flash-mode-line

        tab-width 4

        display-raw-bytes-as-hex t
        redisplay-skip-fontification-on-input t

        window-divider-default-right-width 1
        window-divider-default-bottom-width 1
        window-divider-default-places t  ; Show dividers on all sides

        blink-cursor-mode nil

        show-trailing-whitespace t
        whitespace-style '(face trailing)
        make-backup-files t
        backup-directory-alist '(("." . "~/.saves"))
        auto-save-default nil
        track-eol nil
        line-move-visual nil
        kill-whole-line t
        truncate-lines t
        show-paren-style 'parenthesis
        frame-resize-pixelwise t
        use-short-answers t
        ;;initial-buffer-choice (expand-file-name "~")
        ))

(use-package window
  :ensure nil
  :preface
    (defun select-or-create-window-by-number (n)
      "Select the Nth window in the current frame (1-indexed).
Windows are ordered from left to right, top to bottom.
If the window doesn't exist, create one additional window by splitting horizontally."
      (interactive "nWindow number: ")
      (let* ((windows (sort (window-list nil 'no-minibuffer)
                           (lambda (w1 w2)
                             (let ((edges1 (window-edges w1))
                                   (edges2 (window-edges w2)))
                               ;; Sort by top edge first (Y), then left edge (X)
                               (or (< (cadr edges1) (cadr edges2))
                                   (and (= (cadr edges1) (cadr edges2))
                                        (< (car edges1) (car edges2))))))))
             (current-count (length windows))
             (target-window (nth (1- n) windows)))
        (cond
         ;; Window exists, select it
         (target-window
          (select-window target-window))
         ;; Window doesn't exist, create one more window
         ((> n current-count)
          ;; Split the current window horizontally to create one more
          (split-window-horizontally)
          ;; Move to the new window
          (other-window 1)
          (message "Created window %d" (1+ current-count)))
         ;; Fallback
         (t
          (message "Cannot select window %d" n)))))

    (defun scroll-windows-to-left (&optional windows-list)
      "Scroll all windows (or specified windows) to show the beginning of lines."
      (let ((windows (or windows-list (window-list nil 'no-minibuffer))))
        (dolist (window windows)
          (with-selected-window window
            (when (> (window-hscroll) 0)
              (set-window-hscroll window 0))))))

    (defun toggle-focus-window-by-number (n)
      "Toggle focus mode for the Nth window with simplified states:
1. Window doesn't exist or unfocused - create/focus it
2. Window focused and not maximized - maximize it (minimize all others)
3. Window focused and maximized - restore balance"
      (interactive "nToggle focus window number: ")
      (let* ((windows (sort (window-list nil 'no-minibuffer)
                           (lambda (w1 w2)
                             (let ((edges1 (window-edges w1))
                                   (edges2 (window-edges w2)))
                               (or (< (cadr edges1) (cadr edges2))
                                   (and (= (cadr edges1) (cadr edges2))
                                        (< (car edges1) (car edges2))))))))
             (target-window (nth (1- n) windows))
             (current-window (selected-window))
             ;; Check if current window is maximized (largest among all windows)
             (current-width (window-width current-window))
             (is-maximized (and (eq target-window current-window)
                               (> (length windows) 1)
                               (cl-every (lambda (w)
                                          (or (eq w current-window)
                                              (<= (window-width w) current-width)))
                                        windows)
                               ;; Also check that at least one other window is significantly smaller
                               (cl-some (lambda (w)
                                         (and (not (eq w current-window))
                                              (< (window-width w) (/ current-width 2))))
                                       windows))))

        (cond
         ;; State 1: Window doesn't exist or unfocused - create/focus it
         ((or (not target-window) (not (eq target-window current-window)))
          (select-or-create-window-by-number n)
          (scroll-windows-to-left (list (selected-window)))
          (message "Focused window %d" n))

         ;; State 3: Window focused and maximized - restore balance
         (is-maximized
          (balance-windows)
          (scroll-windows-to-left windows)
          (message "Restored balance for window %d" n))

         ;; State 2: Window focused but not maximized - maximize it
         (t
          ;; Minimize all other windows and maximize current
          (let ((min-width 8)
                (resize-results '()))
            (dolist (window windows)
              (unless (eq window current-window)
                (let* ((initial-width (window-width window))
                       (final-width initial-width))
                  (condition-case nil
                      (progn
                        (window-resize window (- min-width initial-width) t t)
                        (setq final-width min-width))
                    (error nil))
                  (push (format "%d→%d" initial-width final-width) resize-results))))
            (scroll-windows-to-left (list current-window))
            (message "Maximized window %d (others: %s)" n
                     (if resize-results
                         (mapconcat 'identity (reverse resize-results) ", ")
                       "no changes")))))))
  :bind (
    ("s-1"             . (lambda () (interactive) (toggle-focus-window-by-number 1)))
    ("s-2"             . (lambda () (interactive) (toggle-focus-window-by-number 2)))
    ("s-3"             . (lambda () (interactive) (toggle-focus-window-by-number 3)))
    ("s-4"             . (lambda () (interactive) (toggle-focus-window-by-number 4)))
    ("C-x 1"           . delete-other-windows)
    ("C-x 2"           . vsplit-last-buffer)
    ("C-x C-2"         . vsplit-current-buffer)
    ("C-x 3"           . hsplit-last-buffer)
    ("C-x C-3"         . hsplit-current-buffer)
    ("C-x |"           . toggle-window-split))
  :init
  (setq window-combination-resize t
        even-window-sizes 'height-only
        window-sides-slots '(0 1 1 1)
        window-sides-vertical nil
        switch-to-buffer-in-dedicated-window 'pop
        display-buffer-alist
        '(("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
            nil
            (window-parameters (mode-line-format . none)))
          ("\\*\\(Flymake\\|Package-Lint\\|vc-git :\\).*"
           (display-buffer-reuse-window display-buffer-in-previous-window display-buffer-in-side-window)
           (window-height . 0.16)
           (side . bottom)
           (slot . 0)
           (window-parameters . ((no-other-window . t))))
          ("\\*Messages.*"
           (display-buffer-reuse-window display-buffer-in-previous-window display-buffer-in-side-window)
           (window-height . 0.16)
           (side . bottom)
           (slot . 1)
           (window-parameters . ((no-other-window . t))))
          ("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\)\\*"
           (display-buffer-reuse-window display-buffer-in-previous-window display-buffer-in-side-window)
           (window-height . 0.16)
           (side . bottom)
           (slot . 2)
           (window-parameters . ((no-other-window . t))))
          ("\\*\\(Output\\|Register Preview\\).*"
           (display-buffer-reuse-window display-buffer-in-previous-window display-buffer-in-side-window)
           (window-width . 0.16)       ; See the :hook
           (side . bottom)
           (slot . -1)
           (window-parameters . ((no-other-window . t))))
          ;; left side window
          ("\\*Help.*"
           (display-buffer-reuse-window display-buffer-in-previous-window display-buffer-in-side-window)
           (window-width . 0.20)       ; See the :hook
           (side . left)
           (slot . 0)
           (window-parameters . ((no-other-window . t))))
          ;; right side window
          ("\\*Faces\\*"
           (display-buffer-reuse-window display-buffer-in-previous-window display-buffer-in-side-window)
           (window-width . 0.25)
           (side . right)
           (slot . 0)
           (window-parameters
            . ((no-other-window . t)
               (mode-line-format
                . (" "
                   mode-line-buffer-identification)))))
          ("\\*Custom.*"
           (display-buffer-reuse-window display-buffer-in-previous-window display-buffer-in-side-window)
           (window-width . 0.25)
           (side . right)
           (slot . 1))
          ;; bottom buffer (NOT side window)
          ("\\*\\vc-\\(incoming\\|outgoing\\).*"
           (display-buffer-at-bottom)))))

(use-package ace-window
  :bind (("C-x o" . 'ace-window)
         ("C-<tab>" . 'ace-window))
  :config
  (set-face-attribute
   'aw-leading-char-face nil
   :foreground "deep sky blue"
   :weight 'bold
   :height 2.0)
  (set-face-attribute
   'aw-mode-line-face nil
   :inherit 'mode-line-buffer-id
   :foreground "lawn green")
  (setq aw-keys '(?a ?s ?d ?f ?j ?k ?l)
        aw-dispatch-always nil
        aw-dispatch-alist
        '((?x aw-delete-window "Ace - Delete Window")
          (?c aw-swap-window "Ace - Swap Window")
          (?n aw-flip-window)
          (?v aw-split-window-vert "Ace - Split Vert Window")
          (?h aw-split-window-horz "Ace - Split Horz Window")
          (?m delete-other-windows "Ace - Maximize Window")
          (?g delete-other-windows)
          (?b balance-windows)
          (?u (lambda ()
                (progn
                  (winner-undo)
                  (setq this-command 'winner-undo))))
          (?r winner-redo)))
  (ace-window-display-mode t))

(use-package dired
  :ensure nil
  :bind (:map dired-mode-map
         ("M-s f C-s" . nil)
         ("M-s f C-M-s" . nil)
         ("M-s a C-s" . nil)
         ("M-s a C-M-s" . nil)
         ("M-s" . nil))
  :config
  (setq dired-dwim-target t
        ;; dired-listing-switches "-l --almost-all --human-readable --group-directories-first --no-group"
        dired-recursive-copies 'always
        dired-recursive-deletes 'top
        dired-create-destination-dirs 'ask)
  (when (eq system-type 'darwin)
    (let ((gls (executable-find "gls")))
      (when gls
        (setq dired-use-ls-dired t
              insert-directory-program gls
              dired-listing-switches "-aBhl  --group-directories-first"))))
  (put 'dired-find-alternate-file 'disabled nil)
  :hook ((dired-after-readin-hook . hl-line-mode)))

(use-package dired-x :ensure nil :after dired)

(use-package dirvish
  :init
  (dirvish-override-dired-mode)
  :config
  (setq dirvish-mode-line-format '(:left (sort symlink) :right (omit yank index))
        dirvish-attributes '(vc-state subtree-state collapse file-time file-size)
        dirvish-side-attributes '(vc-state collapse file-size)
        dirvish-large-directory-threshold 20000)
   :bind ; Bind `dirvish-fd|dirvish-side|dirvish-dwim' as you see fit
   (("C-x d" . dirvish)
    ("C-c f" . dirvish)
    :map dirvish-mode-map               ; Dirvish inherits `dired-mode-map'
    ("/"   . dirvish-fd)
    (";"   . dired-up-directory)        ; So you can adjust `dired' bindings here
    ("?"   . dirvish-dispatch)          ; [?] a helpful cheatsheet
    ("a"   . dirvish-setup-menu)        ; [a]ttributes settings:`t' toggles mtime, `f' toggles fullframe, etc.
    ("f"   . dirvish-file-info-menu)    ; [f]ile info
    ("o"   . dirvish-quick-access)      ; [o]pen `dirvish-quick-access-entries'
    ("s"   . dirvish-quicksort)         ; [s]ort flie list
    ("r"   . dirvish-history-jump)      ; [r]ecent visited
    ("l"   . dirvish-ls-switches-menu)  ; [l]s command flags
    ("v"   . dirvish-vc-menu)           ; [v]ersion control commands
    ("*"   . dirvish-mark-menu)
    ("y"   . dirvish-yank-menu)
    ("N"   . dirvish-narrow)
    ("C-s" . dirvish-narrow)
    ("^"   . dirvish-history-last)
    ("TAB" . dirvish-subtree-toggle)
    ("M-f" . dirvish-history-go-forward)
    ("M-b" . dirvish-history-go-backward)
    ("M-e" . dirvish-emerge-menu))
  :after dired)

(use-package vterm
  :if (not *sys/is-wsl*))

(use-package vterm-toggle
  :after vterm
  :bind (("C-`" . vterm-toggle)
         ("C-M-`" . vterm-toggle-cd))
  :config
  (setq vterm-toggle-cd-auto-create-buffer nil
        vterm-toggle-fullscreen-p t)
  (add-to-list 'display-buffer-alist
               '((lambda(bufname _) (with-current-buffer bufname (equal major-mode 'vterm-mode)))
                 (display-buffer-reuse-window display-buffer-same-window))))

(use-package tramp
  :ensure nil
  :config
  (put 'temporary-file-directory 'standard-value `(,temporary-file-directory))
  :custom
  (tramp-backup-directory-alist backup-directory-alist)
  (tramp-default-method "ssh")
  (tramp-default-proxies-alist nil)
  (tramp-change-syntax 'simplified))

(use-package ibuffer
  :ensure nil
  :custom
  (ibuffer-saved-filter-groups
    (quote (("default"
             ("Org" (or
                     (mode . org-mode)
                     (name . "^\\*Org Src")
                     (name . "^\\*Org Agenda\\*$")))
             ("C" (or
                   (mode . c-mode)))
             ("LISP" (or
                      (mode . emacs-lisp-mode)
                      (mode . ielm-mode)
                      (name . "^\\*scratch\\*$")
                      (mode . lisp-mode)))
             ("Dired" (or
                        (mode . dired-mode)
                        (mod . dirvish-mode)))
             ("Term" (or
                      (mode . term-mode)
                      (mode . eshell-mode)))))))
  :bind
  (("C-x C-b" . ibuffer))
  :hook ((ibuffer-mode-hook . (lambda () (ibuffer-switch-to-saved-filter-groups "default")))))

(use-package doom-themes
  :demand t
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic nil))

(use-package auto-dark
  :demand t
  :ensure '(auto-dark :type git :host github :repo "LionyxML/auto-dark-emacs" :branch "master")
  :delight auto-dark-mode
  :after doom-themes
  :hook (after-init-hook . auto-dark-mode)
  :init (auto-dark-mode)
  :config
  (setq auto-dark-allow-osascript t)
  (setq auto-dark-themes '((doom-one) (doom-one-light))))

(use-package hl-line
  :ensure nil
  :when (display-graphic-p)
  :hook (after-init-hook . global-hl-line-mode))

(use-package vertico
  :init
  (vertico-mode)
  (vertico-multiform-mode)
  (setq vertico-multiform-commands
        '(
          (consult-imenu buffer indexed)
          (consult-grep buffer indexed)
          (consult-ripgrep buffer indexed)
          (execute-extended-command flat)))
  (setq vertico-multiform-categories
      '((file grid)
        (consult-grep buffer))))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil)
        completion-category-overrides '((file (styles partial-completion))))

(use-package marginalia
  :config
  (marginalia-mode))

(use-package embark
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings))

  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config
  (keymap-set embark-general-map "?" #'gptel-quick)
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package consult
  :bind (("M-s"   . consult-ripgrep)
         ("C-x b" . consult-buffer))
  :hook (completion-list-mode-hook . consult-preview-at-point-mode)
  :init
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

(use-package consult-gh
  :ensure `(consult-gh :type git :host github :repo "armindarvish/consult-gh")
  :after consult)

(use-package consult-gh-forge
  :after consult-gh
  :config
  (require 'consult-gh-transient)
  (consult-gh-forge-mode +1))

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; Mostly text manipulation

(use-package wgrep
  :config
  (setq wgrep-enable-key "e"))

(use-package undo-tree
  :delight
  :ensure (:host gitlab :repo "tsc25/undo-tree" :branch "master")  :init
  (setq undo-tree-auto-save-history t
        undo-tree-visualizer-diff t
        undo-tree-visualizer-timestamps t)
  :bind (("C-x u" . undo-tree-visualize)
         ("C-z"   . 'undo)
         ("C-S-z" .'undo-tree-redo))
  :config
  (progn
    (defun my/undo-tree-restore-default ()
      (setq undo-tree-visualizer-diff t))
    (advice-add 'undo-tree-visualizer-quit :after #'my/undo-tree-restore-default))
  (global-undo-tree-mode t))

(use-package move-text
  :demand t
  :config
  (move-text-default-bindings))

(use-package expand-region
  :bind (("C-=" . 'er/expand-region)
         ("C-+" . 'er/contract-region)))

(use-package multiple-cursors
  :config
  (add-to-list 'mc/unsupported-minor-modes 'lispy-mode)
  (setq mc/always-run-for-all t)
  :bind (("C-S-c C-S-c" . 'mc/edit-lines)
         ("C-d" . 'mc/mark-next-like-this)
         ("C-S-d" . 'mc/mark-previous-like-this)
         ("C->" . 'mc/mark-next-like-this)
         ("C-<" . 'mc/mark-previous-like-this)
         ("C-c C-<" . 'mc/mark-all-like-this)))

(use-package crux
  :bind (("C-x K"        . crux-kill-other-buffers)
         ("C-k"          . crux-smart-kill-line)
         ("C-c 2"        . crux-duplicate-current-line-or-region)
         ("<S-return>"   . crux-smart-open-line)
         ("<C-S-return>" . crux-smart-open-line-above))
  :config
  (crux-with-region-or-buffer indent-region)
  (crux-with-region-or-buffer untabify)
  (crux-with-region-or-point-to-eol kill-ring-save)
  (defalias 'rename-file-and-buffer #'crux-rename-file-and-buffer))

(use-package mwim
  :bind (("C-a" . 'mwim-beginning-of-code-or-line)
         ("C-e" . 'mwim-end-of-code-or-line)
         ("<home>" . 'mwim-beginning-of-code-or-line)
         ("<end>" . 'mwim-end-of-code-or-line)))

;; ---

(use-package treesit
  :ensure nil
  :preface
  (defun ts-install-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    (dolist (grammar
             '(
               (bash "https://github.com/tree-sitter/tree-sitter-bash")
               (c "https://github.com/tree-sitter/tree-sitter-c")
               (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
               (cmake "https://github.com/uyha/tree-sitter-cmake")
               (css "https://github.com/tree-sitter/tree-sitter-css")
               (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
               (elisp "https://github.com/Wilfred/tree-sitter-elisp")
               (html "https://github.com/tree-sitter/tree-sitter-html")
               (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
               (json "https://github.com/tree-sitter/tree-sitter-json")
               (make "https://github.com/alemuller/tree-sitter-make")
               (python "https://github.com/tree-sitter/tree-sitter-python")
               (rust "https://github.com/tree-sitter/tree-sitter-rust")
               (toml "https://github.com/tree-sitter/tree-sitter-toml")
               (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
               (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
               (yaml "https://github.com/ikatyang/tree-sitter-yaml")))
            (add-to-list 'treesit-language-source-alist grammar)
      (unless (treesit-language-available-p (car grammar))
        (treesit-install-language-grammar (car grammar))
        ; (message "`%s' parser was installed." lang)
        (sit-for 0.75))))

  (dolist (mapping
           '(
              (bash-mode . bash-ts-mode)
              (css-mode . css-ts-mode)
              (c-mode . c-ts-mode)
              (c++-mode . c++-ts-mode)
              (elisp-mode . elisp-ts-mode)
              (html-mode . html-ts-mode)
              (js2-mode . js-ts-mode)
              (json-mode . json-ts-mode)
              (makefile-mode . make-ts-mode)
              (python-mode . python-ts-mode)
              (toml-mode . toml-ts-mode)
              (typescript-mode . typescript-ts-mode)
              (rust-mode . rust-ts-mode)
              (yaml-mode . yaml-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))

  :config
  (ts-install-grammars))

(use-package markdown-mode)

(use-package prog-mode
  :ensure nil
  :hook (prog-mode-hook . display-line-numbers-mode))

(use-package c++-mode
  :ensure nil
  :mode ("\\.cpp\\'"
         "\\.hpp\\'"
         "\\.cxx\\'")
  :hook ((c++-mode-hook c++-ts-mode-hook) . lsp-deferred))

(use-package c-mode
  :ensure nil
  :mode ("\\.c\\'"
         "\\.h\\'"
         "\\.cc\\'"))

(use-package flycheck
  :hook (after-init-hook . global-flycheck-mode))

(use-package lsp-mode
  :hook (
    (lsp-mode-hook . lsp-diagnostics-mode)
    (lsp-mode-hook . lsp-enable-which-key-integration)
    (prog-mode-hook . (lambda () (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode 'makefile-mode 'snippet-mode) (lsp-deferred)))))
  :preface
  (setq lsp-use-plists t)
  (defun lsp-booster--advice-json-parse (old-fn &rest args)
    "Try to parse bytecode instead of json."
    (or
     (when (equal (following-char) ?#)

       (let ((bytecode (read (current-buffer))))
         (when (byte-code-function-p bytecode)
           (funcall bytecode))))
     (apply old-fn args)))
  (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
    "Prepend emacs-lsp-booster command to lsp CMD."
    (let ((orig-result (funcall old-fn cmd test?)))
      (if (and (not test?)                             ;; for check lsp-server-present?
               (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
               lsp-use-plists
               (not (functionp 'json-rpc-connection))  ;; native json-rpc
               (executable-find "emacs-lsp-booster"))
          (progn
            (message "Using emacs-lsp-booster for %s!" orig-result)
            (cons "emacs-lsp-booster" orig-result))
        orig-result)))
  :init
  (setq lsp-keymap-prefix "C-c l"
        lsp-use-plists t
        lsp-log-io nil ; use only for debugging as it drastically affects performance
        lsp-keep-workspace-alive nil ; close LSP server if all project buffers are closed
        lsp-idle-delay 0.5
        lsp-diagnostics-provider :flycheck
        lsp-enable-xref t
        lsp-auto-configure t
        lsp-eldoc-enable-hover t
        lsp-lens-enable nil
        lsp-enable-dap-auto-configure t
        lsp-enable-file-watchers t
        lsp-enable-folding t
        lsp-enable-imenu t
        lsp-enable-indentation t
        lsp-enable-links t
        lsp-enable-on-type-formatting nil
        lsp-enable-suggest-server-download t
        lsp-enable-symbol-highlighting t
        lsp-enable-text-document-color t)
  (add-to-list 'warning-suppress-log-types '(lsp-mode))
  (add-to-list 'warning-suppress-types '(lsp-mode))
  (advice-add (if (progn (require 'json)
                         (fboundp 'json-parse-buffer))
                  'json-parse-buffer
                'json-read)
              :around
              #'lsp-booster--advice-json-parse)
  (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command))

  (use-package lsp-completion
    :ensure nil
    :hook ((lsp-mode-hook . lsp-completion-mode)
           (lsp-completion-mode . my/lsp-mode-setup-completion))
    :after lsp-mode
    :init
    (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))
    (setq lsp-completion-provider :none ;; Corfu
          lsp-completion-enable t
          lsp-completion-enable-additional-text-edit t
          lsp-enable-snippet t
          lsp-completion-show-kind t))

  (use-package lsp-ui
    :hook ((lsp-mode-hook . lsp-ui-mode))
    :after lsp-mode)

  (use-package lsp-ui-doc
    :ensure nil
    :hook ((lsp-mode-hook . lsp-ui-doc-mode))
    :after lsp-mode
    :init
    (setq
      lsp-ui-doc-enable t
      lsp-ui-doc-show-with-cursor t
      lsp-ui-doc-include-signature t
      lsp-ui-doc-position 'top
      lsp-ui-doc-side 'right))

  (use-package lsp-ui-peek
    :ensure nil
    :init
    (setq lsp-ui-peek-enable t)
    :after lsp-ui
    :config
    (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
    (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))

  (use-package lsp-ui-sideline
    :ensure nil
    :init
    (setq lsp-ui-sideline-show-diagnostics t
          lsp-ui-sideline-show-hover t
          lsp-ui-sideline-show-code-actions nil
          lsp-ui-sideline-update-mode 'line))

  (use-package lsp-bridge
    :disabled t ;; Until I sort out global python packages installation on macOS 14+
    :ensure '(lsp-bridge :type git :host github :repo "manateelazycat/lsp-bridge"
              :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
              :build (:not compile))
    :init
    (global-lsp-bridge-mode))

  (use-package prescient)

  (use-package cape)
  (use-package corfu
    :custom
    (corfu-cycle t)
    (corfu-preselect 'prompt)
    :hook((prog-mode-hook . corfu-mode)
          (shell-mode-hook . corfu-mode)
          (eshell-mode-hook . corfu-mode)
          (corfu-mode-hook . (lambda ()
                              (setq-local completion-styles '(basic)
                                          completion-category-overrides nil
                                          completion-category-defaults nil))))
    :init
    (global-corfu-mode)
    (corfu-history-mode)
    (corfu-popupinfo-mode)
    :config
    (setq corfu-auto t
          corfu-auto-delay 0
          corfu-auto-prefix 0
          corfu-quit-no-match 'separator))

  (use-package corfu-prescient :after (:all prescient corfu))

  (use-package whitespace-mode :ensure nil)

  ;; VC
  (use-package vc-mode
    :ensure nil)

  (use-package diff-mode
    :ensure nil
    :hook (diff-mode-hook .(lambda ()
                            (setq-local whitespace-style
                                        '( face tabs tab-mark spaces space-mark trailing
                                          indentation::space indentation::tab
                                          newline newline-mark))
                            (whitespace-mode)))
    :after whitespace-mode)

  (use-package diff-hl
    :config
    (setq diff-hl-draw-borders nil
      diff-hl-disable-on-remote t)
    :custom
    (defun enable-diff-hl-dired-locally ()
    (if (file-remote-p default-directory)
        (diff-hl-dired-mode -1)
      (diff-hl-dired-mode 1)))

    (add-hook 'prog-mode-hook #'diff-hl-mode)
    (add-hook 'conf-mode-hook #'diff-hl-mode)
    (add-hook 'dired-mode-hook #'enable-diff-hl-dired-locally)

    (with-eval-after-load 'diff-hl
    (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)
    ;; Highlight on-the-fly
    (diff-hl-flydiff-mode 1)
    (unless (display-graphic-p)
      ;; Fall back to the display margin since the fringe is unavailable in tty
      (diff-hl-margin-mode 1)
      ;; Avoid restoring `diff-hl-margin-mode'
      (with-eval-after-load 'desktop
        (add-to-list 'desktop-minor-mode-table
                    '(diff-hl-margin-mode nil))))))

  (use-package smerge-mode
    :ensure nil
    :config
    (defhydra smerge-hydra
      (:color pink :hint nil :post (smerge-auto-leave))
      "
  ^Move^       ^Keep^               ^Diff^                 ^Other^
  ^^-----------^^-------------------^^---------------------^^-------
  _n_ext       _b_ase               _<_: upper/base        _C_ombine
  _p_rev       _u_pper              _=_: upper/lower       _r_esolve
  ^^           _l_ower              _>_: base/lower        _k_ill current
  ^^           _a_ll                _R_efine
  ^^           _RET_: current       _E_diff
  "
      ("n" smerge-next)
      ("p" smerge-prev)
      ("b" smerge-keep-base)
      ("u" smerge-keep-upper)
      ("l" smerge-keep-lower)
      ("a" smerge-keep-all)
      ("RET" smerge-keep-current)
      ("\C-m" smerge-keep-current)
      ("<" smerge-diff-base-upper)
      ("=" smerge-diff-upper-lower)
      (">" smerge-diff-base-lower)
      ("R" smerge-refine)
      ("E" smerge-ediff)
      ("C" smerge-combine-with-next)
      ("r" smerge-resolve)
      ("k" smerge-kill-current)
      ("ZZ" (lambda ()
              (interactive)
              (save-buffer)
              (bury-buffer))
      "Save and bury buffer" :color blue)
      ("q" nil "cancel" :color blue))
    :hook (magit-diff-visit-file . (lambda ()
                                    (when smerge-mode
                                      (smerge-hydra/body)))))

  (use-package magit
    :commands magit-status
    :after (:all cond-let)
    :ensure '(magit :type git :host github :repo "magit/magit" :branch "main")
    :preface
    (defun magit-disable-whitespace-mode ()
      (setq-local whitespace-trailing nil))
    :custom
    ;;(magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
    (magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
    :config

    (setq 
          magit-revision-insert-related-refs nil
          magit-diff-refine-hunk t
          magit-diff-paint-whitespace nil
          magit-commit-show-diff nil
          magit-branch-direct-configure nil
          magit-refresh-status-buffer t
          magit-tramp-pipe-stty-settings 'pty
          magit-status-buffer-switch-function 'switch-to-buffer)

    :custom
    (defvar magit-toplevel-cache nil)
    (defun memoize-magit-toplevel (orig &optional directory)
      (memoize-remote (or directory default-directory)
                      'magit-toplevel-cache orig directory))
    (with-eval-after-load 'magit
      (advice-add 'magit-toplevel :around #'memoize-magit-toplevel))
    (add-hook 'magit-mode-hook 'magit-disable-whitespace-mode)
    (remove-hook 'magit-status-headers-hook 'magit-insert-tags-header)

    (setq magit-post-display-buffer-hook
      #'(lambda ()
          (when (derived-mode-p 'magit-status-mode)
            (delete-other-windows)))))

(use-package forge
  :after (:all cond-let closql magit llama))

(use-package ghub
  :config
  (setq ghub-default-host "github.com")
  :after (:all magit))

(use-package rust-mode
  :hook (rust-mode-hook . lsp)
  :custom
  (rust-format-on-save t)
  (rust-mode-treesitter-derive t))

(use-package cargo-mode
  :hook
  (rust-mode-hook . cargo-minor-mode)
  :config
  (setq compilation-scroll-output t))

;; Gptel and stuff
(use-package gptel
  :hook ((gptel-post-stream-hook . gptel-auto-scroll)
         (gptel-post-response-functions . gptel-end-of-response)
         (gptel-mode-hook  . (lambda ()
                              (when (and (derived-mode-p 'org-mode)
                                        (bound-and-true-p org-indent-mode))
                                (org-indent-mode -1)))))
  :commands (gptel gptel-send gptel-menu)
  :bind (("C-c <enter>"    . gptel-send)
         ("C-c RET"        . gptel-send)
         ("C-c C-<return>" . gptel-menu))
  :init
  (setopt gptel-default-mode 'org-mode)
  (setopt gptel-include-reasoning nil)
  :config
  (setq gptel-model 'claude-sonnet-4
        gptel-backend (gptel-make-gh-copilot "Copilot")))

(use-package gptel-quick
  :ensure (:host github :repo "karthink/gptel-quick" :branch "master")
  :after gptel)

(use-package claude-code-ide
  :ensure (:type git :host github :repo "manzaltu/claude-code-ide.el")
  :bind ("C-c C-'" . claude-code-ide-menu)
  :config
  (setq claude-code-ide-use-side-window   nil
        claude-code-ide-focus-on-open	  t
        claude-code-ide-enable-mcp-server t)
  (claude-code-ide-emacs-tools-setup))

;; Denote + org-mode
(use-package denote)
