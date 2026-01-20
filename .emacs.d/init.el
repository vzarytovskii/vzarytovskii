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

(use-package emacs
  :ensure nil
  :demand t
  :defer nil
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

(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)