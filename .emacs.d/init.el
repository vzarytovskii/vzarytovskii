;;; init.el --- Emacs configuration bootstrap. -*- lexical-binding: t; -*-
;;; Commentary:
;; Emacs bootstrap, for main configuration, see config.el

;;; Code:

(let ((minver 26))
  (unless (>= emacs-major-version minver)
    (error "Your Emacs is too old -- this configuration requires v%s or higher" minver)))

(unless (>= emacs-major-version 27)
  (message "Early init: Emacs Version < 27.0")
  (load (expand-file-name "early-init.el" user-emacs-directory)))

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
                use-package-compute-statistics t
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

(use-package auto-package-update
  :if (not (daemonp))
  :custom
  (auto-package-update-interval 7) ;; in days
  (auto-package-update-prompt-before-update t)
  (auto-package-update-delete-old-versions t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe))

(require 'tls)

(setq gnutls-verify-error (not (getenv "INSECURE"))
      tls-checktrust gnutls-verify-error
      tls-program (list "gnutls-cli --x509cafile %t -p %p %h"
                        ;; compatibility fallbacks
                        "gnutls-cli -p %p %h"
                        "openssl s_client -connect %h:%p -no_ssl2 -no_ssl3 -ign_eof"))

(setenv "PATH" (concat (getenv "PATH") ":~/.dotnet:~/.dotnet/tools:~/.cabal/bin:~/.ghcup/bin"))
(setq exec-path (append exec-path '("~/.dotnet" "~/.dotnet/tools" "~/.cabal/bin" "~/.ghcup/bin")))

(defun reload-init-file ()
    (interactive)
    (load-file user-init-file))

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

(defvar big-fringe-mode nil)
(define-minor-mode big-fringe-mode
  "Minor mode to use big fringe in the current buffer."
  :init-value nil
  :global t
  :variable big-fringe-mode
  :group 'editing-basics
  (if (not big-fringe-mode)
      (set-fringe-style nil)
    (set-fringe-mode
     (/ (- (frame-pixel-width)
           (* 200 (frame-char-width)))
        2))))


(use-package delight
  :after use-package)

(use-package async
  :init
  (setq async-bytecomp-allowed-packages '(all))
  :config
  (async-bytecomp-package-mode t)
  (dired-async-mode 1)
  ;; limit number of async processes
  (eval-when-compile
    (require 'cl-lib))
  (defvar async-maximum-parallel-procs 20)
  (defvar async--parallel-procs 0)
  (defvar async--queue nil)
  (defvar-local async--cb nil)
  (advice-add #'async-start :around
              (lambda (orig-func func &optional callback)
                (if (>= async--parallel-procs async-maximum-parallel-procs)
                    (push `(,func ,callback) async--queue)
                  (cl-incf async--parallel-procs)
                  (let ((future (funcall orig-func func
                                         (lambda (re)
                                           (cl-decf async--parallel-procs)
                                           (when async--cb (funcall async--cb re))
                                           (when-let (args (pop async--queue))
                                             (apply #'async-start args))))))
                    (with-current-buffer (process-buffer future)
                      (setq async--cb callback)))))
              '((name . --queue-dispatch))))

;; Configure Emacs' defaults and keybinds;
(use-package emacs
  :delight auto-revert-mode
  :bind (("C-z"             . nil)
         ("C-x C-z"         . nil)
         ("C-h h"           . nil)
         ("<C-backspace>"   . nil)
         ([delete]          . 'delete-forward-char)
         ("C-x C-2"         . 'vsplit-last-buffer)
         ("C-x C-2"         . 'vsplit-current-buffer)
         ("C-x 3"           . 'hsplit-last-buffer)
         ("C-x C-3"         . 'hsplit-current-buffer)
         ("C-x |"           . 'toggle-window-split)
         ("C-w"             . 'backward-kill-word)
         ("M-w"             . 'copy-region-or-line)
	       ("C-g"             . 'keyboard-quit)
         ("C-k"             . 'kill-buffer)
         ("C-K"             . 'kill-this-buffer)
         ("C-c o"           . 'switch-to-minibuffer))
  :hook (after-init-hook . window-divider-mode)
  :delight lisp-interaction-mode
  :preface

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

  :config
  ;; Defaults

  (add-hook 'window-configuration-change-DELETEME-hook
    (lambda ()
      (if (delq nil
                (let ((fw (frame-width)))
                  (mapcar (lambda(w) (< (window-width w) (/ fw 2)))
                          (window-list))))
          (big-fringe-mode 0)
        (big-fringe-mode 1))))

  (big-fringe-mode 0)

  (setq-default major-mode 'text-mode
                use-file-dialog nil
                use-dialog-box t
                cursor-type 'box
                x-stretch-cursor t
                cursor-in-non-selected-window nil
                indent-tabs-mode nil)

  (fset 'yes-or-no-p 'y-or-n-p)
  (setq inhibit-default-init t
        inhibit-startup-screen t
        inhibit-startup-message t
        inhibit-startup-echo-area-message t
        initial-scratch-message nil

	idle-update-delay 1.1

        scroll-margin 0
        scroll-step 1
        scroll-conservatively 100000
        scroll-preserve-screen-position 1
        next-line-add-newlines nil

        byte-compile-warnings '(cl-functions)
        visible-bell nil
        ring-bell-function 'flash-mode-line

        tab-width 4
        frame-resize-pixelwise t

	redisplay-skip-fontification-on-input t

        window-divider-default-right-width 1
        window-divider-default-bottom-width 1
        window-divider-default-places 'right-only

	show-trailing-whitespace t
        whitespace-style '(face trailing)
        make-backup-files t
        backup-directory-alist '(("." . "~/.saves"))
        auto-save-default nil
        track-eol nil
        line-move-visual nil
        kill-whole-line t
        indent-tabs-mode nil
        truncate-lines t
        show-paren-style 'parenthesis
        frame-resize-pixelwise t))

(use-package frame
  :straight nil
  :config
  (setq mode-line-compact t
        max-mini-window-height 1.0
        input-method-use-echo-area nil
        echo-keystrokes 0
        resize-mini-windows nil)
  (blink-cursor-mode -1)
  (column-number-mode t)
  (global-subword-mode t)
  (horizontal-scroll-bar-mode -1)
  (line-number-mode +1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (size-indication-mode t)
  (toggle-scroll-bar -1)
  (tool-bar-mode -1)

  (when (featurep 'ns)
    (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t)))
  (add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))
  (add-to-list 'default-frame-alist '(menu-bar-lines . 0))
  (add-to-list 'default-frame-alist '(tool-bar-lines . 0))
  (add-to-list 'default-frame-alist '(left-fringe . 1))
  (add-to-list 'default-frame-alist '(right-fringe . 1))
  (add-to-list 'default-frame-alist '(internal-border-width . 0)
  (add-to-list 'default-frame-alist '(fullscreen . maximized))))

(use-package faces
  :straight nil
  :preface

  (defun my-dpi (&optional frame)
    "Get the DPI of FRAME (or current if nil)."
    (cl-flet ((pyth (lambda (w h)
                      (sqrt (+ (* w w)
                               (* h h)))))
              (mm2in (lambda (mm)
                       (/ mm 25.4))))
      (let* ((atts (frame-monitor-attributes frame))
             (pix-w (cl-fourth (assoc 'geometry atts)))
             (pix-h (cl-fifth (assoc 'geometry atts)))
             (pix-d (pyth pix-w pix-h))
             (mm-w (cl-second (assoc 'mm-size atts)))
             (mm-h (cl-third (assoc 'mm-size atts)))
             (mm-d (pyth mm-w mm-h)))
        (/ pix-d (mm2in mm-d)))))

  (defun my-preferred-font-size ()
    (let ((dpi (my-dpi)))
      (message "DPI: %d" dpi)
      (cond
       ((< dpi 110) 17)
       ((> dpi 130) 18)
       (t 14))))

  (message "Initial preferred font size: %d" (my-preferred-font-size))

  (defvar --font-name
    "Consolas")
  (defvar --default-font
    (font-spec :family --font-name :size (my-preferred-font-size) :dpi (my-dpi) :weight 'normal))

  (defun adapt-font-size (&optional frame)
    (message "Adapted preferred font size: %d" (my-preferred-font-size))
    (set-frame-font (font-spec :family --font-name :size (my-preferred-font-size) :dpi (my-dpi) :weight 'normal)))

  :config

  (setf (alist-get 'font default-frame-alist)
        (font-xlfd-name --default-font))
  (set-frame-font --default-font t t)
  (when (display-graphic-p)
    (setq font-use-system-font t))

  (add-function :after after-focus-change-function #'adapt-font-size)
  (add-hook 'window-size-change-functions #'adapt-font-size)
  (add-hook 'after-make-frame-functions #'adapt-font-size))

(use-package hydra)
(use-package transient
  :config
  (setq transient-default-level 5))
(use-package fullframe
  :after magit
  :config
  (fullframe magit-status magit-mode-quit-window))

;; Git

(use-package forge
  :after magit
  :preface
  (defun forge-bug-reference-setup ()
  (magit--with-safe-default-directory nil
    (when-let ((repo (forge-get-repository 'full)))
      (setq-local
       bug-reference-auto-setup-functions
       (let ((functions bug-reference-auto-setup-functions))
         (list (lambda ()
                 (catch 'setup
                   (dolist (f functions)
                     (when (funcall f)
                       (setq bug-reference-bug-regexp
                             (concat "[^\n]" bug-reference-bug-regexp))
                       (throw 'setup t))))))))
      (if (derived-mode-p 'prog-mode)
          (bug-reference-prog-mode 1)
        (bug-reference-mode 1))
      (add-hook 'completion-at-point-functions
                'forge-topic-completion-at-point nil t))))
  :hook (magit-mode . forge-bug-reference-setup)
  :config
  (setq forge-topic-list-limit '(100 . -10)))

(use-package ediff
  :ensure nil
  :custom
  (ediff-keep-variants nil)
  (ediff-split-window-function #'split-window-horizontally)
  (ediff-window-setup-function #'ediff-setup-windows-plain))

(use-package git-modes)

(use-package magit
  :defer t
  :commands (magit magit-status magit-blame magit-mode magit-file-popup)
  :bind (("C-x g" . magit-status)
         ("C-c g" . magit-dispatch-popup))
  :config
  (setq magit-status-margin '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 18)
        magit-list-refs-sortby "-creatordate"
        magit-refs-show-commit-count 'branch
        magit-status-sections-hook (-concat magit-status-sections-hook '(magit-insert-local-branches))
        magit-diff-refine-hunk t
  	    magit-commit-arguments '("--verbose")
        magit-display-buffer-function #'magit-display-buffer-traditional
        magit-diff-highlight-hunk-region-functions
          '(magit-diff-highlight-hunk-region-dim-outside
            magit-diff-highlight-hunk-region-using-face)
        magit-section-initial-visibility-alist
            '((unpulled . show)
              (unpushed . show)
              (untracked . show)
              (unstaged . show)
              (pullreqs . hide)
              (issues . hide)
              (stashes . hide)
              (branches . hide)
              (todos . show)
              (branches . hide)
              (recent . show))
        magit-section-visibility-indicator '("…" . nil))

  (defun magit-remove-from-hook (magit-hook funs)
    (dolist (f funs)
      (remove-hook magit-hook f)))

  (defun magit-add-section-hooks (magit-hook funs)
    (dolist (f funs)
      (magit-add-section-hook magit-hook f nil t)))

  ;; Cleanup magit status page by removing everything from it, and then add only wanted items
  ;; Magit status header
  ;;(magit-remove-from-hook
  ;;  'magit-status-headers-hook
  ;;  '(magit-insert-head-branch-header
  ;;    magit-insert-upstream-branch-header
  ;;    magit-insert-push-branch-header
  ;;    magit-insert-tags-header
  ;;    magit-insert-error-header
  ;;    magit-insert-diff-filter-header))

  ;;(magit-add-section-hooks
  ;;  'magit-status-headers-hook
  ;;  '(magit-insert-head-branch-header
  ;;    magit-insert-upstream-branch-header
  ;;    ;;magit-insert-push-branch-header
  ;;    magit-insert-error-header
  ;;    magit-insert-diff-filter-header))

  ;; Magit status sections
  (magit-remove-from-hook
    'magit-status-sections-hook
    '(;;magit-insert-unpushed-to-upstream-or-recent
      ;;magit-insert-merge-log
      ;;magit-insert-rebase-sequence
      ;;magit-insert-am-sequence
      ;;magit-insert-sequencer-sequence
      ;;magit-insert-bisect-output
      ;;magit-insert-bisect-rest
      ;;magit-insert-bisect-log
      ;;magit-insert-untracked-files
      ;;magit-insert-unstaged-changes
      ;;magit-insert-staged-changes
      ;;magit-insert-stashes
      ;;magit-insert-unpulled-from-upstream
      ;;magit-insert-unpulled-from-pushremote
      ;;magit-insert-unpushed-to-upstream
      ;;magit-insert-unpushed-to-pushremote
      ;;magit-insert-tracked-files
      ;;magit-insert-ignored-files
      ;;magit-insert-skip-worktree-files
      ;;magit-insert-assumed-unchanged-files
      ;;magit-insert-unpulled-or-recent-commits
      ;;magit-insert-recent-commits
      ;;magit-insert-unpulled-cherries
      ;;magit-insert-unpushed-cherries
      magit-insert-local-branches
      ;;forge-insert-pullreqs
      ;;forge-insert-issues
      ))
  ;;(magit-add-section-hooks
  ;;  'magit-status-sections-hook
  ;;  '(magit-insert-unstaged-changes
  ;;    magit-insert-staged-changes
  ;;    magit-insert-untracked-files
  ;;    magit-insert-unpulled-or-recent-commits
  ;;    forge-insert-pullreqs
  ;;    forge-insert-issues))


  (magit-add-section-hook 'magit-status-sections-hook 'forge-insert-pullreqs nil t)
  (magit-add-section-hook 'magit-status-sections-hook 'forge-insert-issues nil t)
  ;;(magit-add-section-hook 'magit-status-sections-hook 'magit-insert-merge-log nil t)
  ;;(magit-add-section-hook 'magit-status-sections-hook 'magit-insert-rebase-sequence nil t)
  ;;(magit-add-section-hook 'magit-status-sections-hook 'magit-insert-am-sequence nil t)
  ;;(magit-add-section-hook 'magit-status-sections-hook 'magit-insert-sequencer-sequence nil t)
  ;;(magit-add-section-hook 'magit-status-sections-hook 'magit-insert-bisect-output nil t)
  ;;(magit-add-section-hook 'magit-status-sections-hook 'magit-insert-bisect-rest nil t)
  ;;(magit-add-section-hook 'magit-status-sections-hook 'magit-insert-bisect-log nil t)
  ;;(magit-add-section-hook 'magit-status-sections-hook 'magit-insert-untracked-files nil t)
  ;;(magit-add-section-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-upstream-or-recent nil t)

  (progn
    (setq magit-post-display-buffer-hook
          #'(lambda ()
              (when (derived-mode-p 'magit-status-mode)
                (delete-other-windows))))

    (setenv "GIT_PAGER" "")
    (add-hook 'magit-log-edit-mode-hook
              '(lambda ()
                 (auto-fill-mode)
                 (flyspell-mode)
                 (set-fill-column 120))))

  (defvar magit--modified-files nil)

  (defun magit-maybe-cache-modified-files ()
    "Maybe save a list of modified files.
 That list is later used by `magit-update-uncommitted-buffers',
 provided it is a member of `magit-post-refresh-hook'.  If it is
 not, then don't save anything here."
    (when (memq 'magit-update-uncommitted-buffers magit-post-refresh-hook)
      (setq magit--modified-files (magit-unstaged-files t))))

  (add-hook 'magit-pre-refresh-hook #'magit-maybe-cache-modified-files)
  (add-hook 'magit-pre-call-git-hook #'magit-maybe-cache-modified-files)
  (add-hook 'magit-pre-start-git-hook #'magit-maybe-cache-modified-files)

  (defun magit-update-uncommitted-buffers ()
    "Update some file-visiting buffers belonging to the current repository.
 Run `magit-update-uncommitted-buffer-hook' for each buffer
 which visits a file inside the current repository that had
 uncommitted changes before running the current Magit command
 and/or that does so now."
    (let ((topdir (magit-toplevel)))
      (dolist (file (delete-consecutive-dups
		     (sort (nconc (magit-unstaged-files t)
				  magit--modified-files)
			   #'string<)))
	(--when-let (find-buffer-visiting (expand-file-name file topdir))
	  (with-current-buffer it
	    (run-hooks 'magit-update-uncommitted-buffer-hook))))))

  (add-hook 'magit-post-refresh-hook #'magit-update-uncommitted-buffers)

  (define-advice magit-push-current-to-upstream (:before (args) query-yes-or-no)
    "Prompt for confirmation before permitting a push to upstream."
    (when-let ((branch (magit-get-current-branch)))
      (unless (yes-or-no-p (format "Push %s branch upstream to %s? "
				   branch
				   (or (magit-get-upstream-branch branch)
				       (magit-get "branch" branch "remote"))))
	(user-error "Push to upstream aborted by user")))))

(use-package magit-lfs
  :after magit)

(use-package magit-todos
  :disabled t
  :after magit
  :hook (magit-mode-hook . magit-todos-mode)
  :custom
  (magit-todos-exclude-globs '("node_modules" "*.json"))
  :config
  (setq magit-todos-auto-group-items 'always))

(use-package magit-delta
  :delight
  :if (executable-find "delta")
  :after magit
  :hook (magit-mode-hook . magit-delta-mode)
  :config
  (setq magit-delta-hide-plus-minus-markers nil))

(use-package magit-filenotify
  :after magit
  :hook (after-save-hook . magit-filenotify-mode))


(use-package git-commit-insert-issue
  :config
  (add-hook 'git-commit-mode-hook 'git-commit-insert-issue-mode))

(use-package graphql)

(use-package ghub)

(use-package github-explorer
  :after graphql)

(use-package pr-review
  :straight (:host github :repo "blahgeek/emacs-pr-review" :branch "master" :files (:defaults "graphql"))
  :after (:all magit forge transient)
  :config
  (transient-insert-suffix 'magit-dispatch '(1)
    ["Code Review"
     ("= n" "PR review notifications" pr-review-notification)]))

(use-package code-review
  :straight (:host github :repo "phelrine/code-review" :branch "fix/closql-update" :files (:defaults "graphql"))
  :after (:all magit forge transient)
  :config
  (transient-insert-suffix 'magit-dispatch '(1)
    ["Code Review"
     ("= c" "Review PR at cursor" code-review-forge-pr-at-point)]))

(use-package git-link
  :bind (("C-x C-g i")))

(use-package git-blamed)

(use-package git-timemachine
  :after magit
  :commands my/git-timemachine-on
  :bind ("C-x v t" . git-timemachine-toggle)
  :config
  (defhydra my/git-timemachine
    (:color pink :hint nil)
    ("n" git-timemachine-show-next-revision "Next Revision" :column "Go to")
    ("p" git-timemachine-show-previous-revision "Next Revision")
    ("c" git-timemachine-show-current-revision "Current Revision")
    ("g" git-timemachine-show-nth-revision "Nth Revision")
    ("t" git-timemachine-show-revision-fuzzy "Search")
    ("W" git-timemachine-kill-revision "Copy full revision" :column "Actions")
    ("w" git-timemachine-kill-abbreviated-revision "Copy abbreviated revision" :column "Actions")
    ("C" git-timemachine-show-commit "Show commit")
    ("b" git-timemachine-blame "Blame")
    ("q" git-timemachine-quit "cancel" :color blue :column nil))
  (defun my/git-timemachine-on ()
    (interactive)
    (git-timemachine)
    (my/git-timemachine/body))

  ;; (define-advice git-timemachine-mode (:after (_args) open-timemachine-hydra)
  ;; (my/git-timemachine-on))

  (transient-insert-suffix 'magit-dispatch '(1)
    ["Git Time Machine"
     ("T" "Toggle time machine" my/git-timemachine-on)]))

(use-package git-messenger
  :init
  (setq git-messenger:show-detail t
        git-messenger:use-magit-popup t)
  :bind (:map vc-prefix-map
              ("p" . git-messenger:popup-message)))

(use-package git-gutter-fringe
  :delight git-gutter-mode
  :hook (after-init-hook . global-git-gutter-mode)
  :init (setq git-gutter:visual-line t
              git-gutter:disabled-modes '(asm-mode image-mode)
              git-gutter:modified-sign "~" ;;"❚"
              git-gutter:added-sign "+" ;;"✚"
              git-gutter:deleted-sign "-" ;;"✖"
              blamer-max-commit-message-length 100)

  :bind (("C-c v =" . git-gutter:popup-hunk)
         ("C-c p" . git-gutter:previous-hunk)
         ("C-c n" . git-gutter:next-hunk)))

(use-package diff-hl
  :disabled t
  :config
  (global-diff-hl-mode))

(use-package smerge-mode
  :after hydra
  :preface
  (with-eval-after-load 'hydra
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
      ("q" nil "cancel" :color blue)))
  :hook ((find-file-hook . (lambda ()
                             (save-excursion
                               (goto-char (point-min))
                               (when (re-search-forward "^<<<<<<< " nil t)
                                 (smerge-mode 1)))))
         (magit-diff-visit-file-hook . (lambda ()
                                         (when smerge-mode
                                           (smerge-hydra/body))))))