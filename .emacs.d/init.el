;;; -*- lexical-binding: t; -*- no-byte-compile: t; -*-

;; Common tools to make sure installed via mason

(require 'package)

(setq-default
              ad-redefinition-action 'accept
              custom-file (expand-file-name "custom.el" user-emacs-directory)
              gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"
              gnutls-verify-error (not (getenv "INSECURE"))
              load-prefer-newer t
              package-install-upgrade-built-in t
              package-quickstart t
              package-archives
              '(("gnu" . "https://elpa.gnu.org/packages/")
                ("gnu-devel" . "https://elpa.gnu.org/devel/")
                ("melpa" . "https://stable.melpa.org/packages/")
                ("melpa-devel" . "https://melpa.org/packages/")
                ("nongnu" . "https://elpa.nongnu.org/nongnu/"))
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

(use-package cond-let
  :ensure t
  :vc (:url "https://github.com/tarsius/cond-let" :rev "main"))

(use-package llama)
(use-package emacsql)
(use-package closql :after emacsql)

(use-package hydra :ensure t)

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

(use-package timu-macos-theme
  :ensure t
  :defer nil
  :demand t
  :preface
  (defun u/macos-appearance ()
    "Return 'light or 'dark based on macOS system appearance."
    (let ((result (string-trim
                   (shell-command-to-string
                    "defaults read -g AppleInterfaceStyle 2>/dev/null || echo 'Light'"))))
      (if (string-equal result "Dark") 'dark 'light)))
  :config
  (customize-set-variable 'timu-macos-color-contrast 'contrasted)
  (customize-set-variable 'timu-macos-mode-line-border-type "none")

  (let ((appearance (u/macos-appearance)))
    (customize-set-variable 'timu-macos-flavour
                            (if (eq appearance 'dark) "dark" "light")))
  (load-theme 'timu-macos t))

(use-package auto-dark
  :ensure t
  :defer t
  :demand nil
  :disabled t
  :hook
  (auto-dark-dark-mode-hook
   . (lambda ()
       (customize-set-variable 'timu-macos-flavour "dark")
       (load-theme 'timu-macos t)))
  (auto-dark-light-mode-hook
   . (lambda ()
       (customize-set-variable 'timu-macos-flavour "light")
       (load-theme 'timu-macos t)))
  :init (auto-dark-mode)
  :custom
  (auto-dark-polling-interval-seconds 5)
  (auto-dark-allow-osascript t))

(use-package emacs
  :ensure nil
  :demand t
  :defer nil
  :hook
  ((prog-mode-hook . display-line-numbers-mode)
   (after-init-hook . hl-line-mode))
  :bind (("C-z"             . nil)
         ("C-x C-z"         . nil)
         ("C-h h"           . nil)
         ("<C-backspace>"   . nil)
         ([delete]          . 'delete-forward-char)
         ("C-w"             . 'backward-kill-word)
         ("M-w"             . 'copy-region-or-line)
         ("C-g"             . 'keyboard-quit)
         ("C-k"             . u/smart-kill-line)
         ("C-S-k"           . 'kill-buffer-and-window)
         ("C-c o"           . 'switch-to-minibuffer)
         ([remap keyboard-quit] . 'keyboard-quit-ex)
         ("C-M-<up>"          . previous-line)
         ("C-M-<down>"        . next-line)
         ("C-M-<left>"        . left-word)
         ("C-M-<right>"       . right-word)
         ("M-<up>"        . move-line-up)
         ("M-<down>"      . move-line-down)
         ("M-<left>"      . move-text-left)
         ("M-<right>"     . move-text-right))
  :preface
  (defun move-line-up ()
    "Move current line up."
    (interactive)
    (let ((col (current-column)))
      (transpose-lines 1)
      (forward-line -2)
      (move-to-column col)))

  ;; Move line down
  (defun move-line-down ()
    "Move current line down."
    (interactive)
    (let ((col (current-column)))
      (forward-line 1)
      (transpose-lines 1)
      (forward-line -1)
      (move-to-column col)))

  ;; Move text left
  (defun move-text-left ()
    "Move char or word(s) left based on selection.
No region: transpose char before point with previous char.
Whole word(s) selected: transpose words.
Partial word selected: transpose chars."
    (interactive)
    (if (region-active-p)
        (if (region-at-word-boundaries-p)
            (transpose-words -1)
          (transpose-chars -1))
      (transpose-chars -1)))

  ;; Move text right
  (defun move-text-right ()
    "Move char or word(s) right based on selection.
No region: transpose char at point with next char.
Whole word(s) selected: transpose words.
Partial word selected: transpose chars."
    (interactive)
    (if (region-active-p)
        (if (region-at-word-boundaries-p)
            (transpose-words 1)
          (progn
            (goto-char (region-end))
            (transpose-chars 1)))
      (progn
        (forward-char)
        (transpose-chars 1)
        (backward-char))))

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

  (defun u/smart-kill-line ()
    (interactive)
    (cond
     ((use-region-p)
      (kill-region (region-beginning) (region-end)))

     ((not (eolp))
      (kill-region (point) (line-end-position)))

     (t
      (let* ((bol (line-beginning-position))
             (eol (line-end-position))
             (line-text (buffer-substring-no-properties bol eol)))
        (cond
         ((string-match-p "\\`[[:space:]]*\\'" line-text)
          (delete-region bol eol)
          (when (and (= bol eol)
                     (looking-at "\r?\n"))
            (kill-region eol (match-end 0))))

         (t
          (let ((start (save-excursion
                         (goto-char bol)
                         (skip-chars-forward "[:space:]" eol)
                         (point))))
            (kill-region start eol))))))))
  :config
    (setq-default major-mode 'text-mode
                  use-file-dialog nil
                  use-dialog-box nil
                  cursor-type 'box
                  x-stretch-cursor t
                  cursor-in-non-selected-window nil

                  indent-tabs-mode nil
                  tab-width 4

                  select-enable-primary t
                  select-enable-clipboard t

                  scroll-margin 0
                  scroll-step 1
                  scroll-conservatively 100000
                  scroll-preserve-screen-position 'always

                  hscroll-step 2
                  hscroll-margin 2

                  next-line-add-newlines nil

                  byte-compile-warnings '(cl-functions)
                  visible-bell nil
                  ring-bell-function 'flash-mode-line

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
                  use-short-answers t)

  (fset 'yes-or-no-p 'y-or-n-p))

(use-package centered-window
  :ensure t
  :defer t
  :demand nil
  :commands centered-window-mode
  :hook (after-init-hook . centered-window-mode)
  :custom
  (cwm-centered-window-width 150))


(use-package mason
  :ensure t
  :defer t
  :demand nil
  :init
    (defvar mason-tools '("clangd"))
  :config
  (mason-setup
    (dolist (pkg mason-tools)
      (unless (mason-installed-p pkg)
        (ignore-errors (mason-install pkg))))))

(use-package treesit
  :ensure nil
  :defer nil
  :commands (treesit-install-language-grammar setup-install-grammars)
  :init
    (defvar treesitter-grammars
      '(
        (json . ("https://github.com/tree-sitter/tree-sitter-json" "master"))
        (elisp . ("https://github.com/Wilfred/tree-sitter-elisp" "main"))
        (cmake . ("https://github.com/uyha/tree-sitter-cmake" "master"))
        (c . ("https://github.com/tree-sitter/tree-sitter-c" "master"))
        (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp" "master"))
        (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "master"))
      ))
  :preface
    (defun setup-install-grammars ()
      "Install Tree-sitter grammars if they are absent."
      (interactive)
      (dolist (grammar treesitter-grammars)
        (add-to-list 'treesit-language-source-alist grammar)
        (unless (treesit-language-available-p (car grammar))
          (message "Installing Tree-sitter grammar for %s..." (car grammar))
          (treesit-install-language-grammar (car grammar)))))
    :config
      (setup-install-grammars))

(use-package treesit-auto
  :ensure t
  :defer nil
  :after treesit
  :hook (after-init-hook . global-treesit-auto-mode)
  :custom
    (treesit-auto-install 'prompt)
  :config
    (treesit-auto-add-to-auto-mode-alist 'all))

;; Git

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
  :ensure t
  :defer t
  :demand nil
  :commands (magit magit-status)
  :custom
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
      magit-status-buffer-switch-function 'switch-to-buffer
      magit-post-display-buffer-hook
        #'(lambda ()
            (when (derived-mode-p 'magit-status-mode)
              (delete-other-windows)))
      magit-display-buffer-function
        (lambda (buffer)
          (display-buffer
            buffer (if (and (derived-mode-p 'magit-mode)
                            (memq (with-current-buffer buffer major-mode)
                                  '(magit-process-mode
                                    magit-revision-mode
                                    magit-diff-mode
                                    magit-stash-mode
                                    magit-status-mode)))
                      nil
                    '(display-buffer-same-window))))))

(use-package forge :after magit)

(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)
