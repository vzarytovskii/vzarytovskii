;;; core.el --- Core packages configuration (such as Emacs, no-littering, async, etc).
;;; Commentary:
;; Core packages config, for main config, see config.el

;;; -*- lexical-binding: t -*-

;;; Code:

(use-package auto-package-update
  :if (not (daemonp))
  :custom
  (auto-package-update-interval 1) ;; in days
  (auto-package-update-prompt-before-update t)
  (auto-package-update-delete-old-versions t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe))

(use-package no-littering
  :config
  (require 'recentf)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (setq custom-file (no-littering-expand-etc-file-name "custom.el")))

(use-package delight
  :after use-package)

(use-package async
  :init
  (dired-async-mode 1)
  (async-bytecomp-package-mode 1)
  :custom
  (async-bytecomp-allowed-packages '(all)))

;; Configure Emacs' defaults and keybinds;
(use-package emacs
  :bind (("C-z"     . nil)
         ("C-x C-z" . nil)
         ("C-h h"   . nil)
         ([delete]  . 'delete-forward-char)
         ("C-x C-2" . 'vsplit-last-buffer)
         ("C-x 2"   . 'vsplit-current-buffer)
         ("C-x C-3" . 'hsplit-last-buffer)
         ("C-x 3"   . 'hsplit-current-buffer)
         ("C-x |"   . 'toggle-window-split)
         ("M-w"     . 'copy-region-or-line))
  :preface
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
  (setq-default major-mode 'text-mode
                use-file-dialog nil
                use-dialog-box t
                cursor-type 'box
                x-stretch-cursor t
                cursor-in-non-selected-window nil
                indent-tabs-mode nil)
  (setq inhibit-default-init t
        inhibit-startup-screen t
        inhibit-startup-message t
        inhibit-startup-echo-area-message t
        initial-scratch-message nil

        scroll-margin 10
        scroll-step 1
        next-line-add-newlines nil
        scroll-conservatively 10000
        scroll-preserve-screen-position 1
        
        byte-compile-warnings '(cl-functions)
        visible-bell nil
        ring-bell-function 'flash-mode-line

        tab-width 4
        frame-resize-pixelwise t

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

(use-package exec-path-from-shell
  :custom
  (exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-variables '("PATH" "DOTNET_HOME" "SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO"))
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package hydra)

;; Editing and navigation (including windows navigation, dwim/mwin, mc, etc):

(use-package subword
  :straight nil
  :delight)

(use-package hungry-delete
  :delight
  :config (global-hungry-delete-mode))

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

(use-package drag-stuff
  :delight
  :config
  (drag-stuff-define-keys)
  (drag-stuff-global-mode 1))

(use-package delsel
  :hook (after-init-hook . delete-selection-mode))

(use-package mwim
  :bind (("C-a" . 'mwim-beginning-of-code-or-line)
         ("C-e" . 'mwim-end-of-code-or-line)
         ("<home>" . 'mwim-beginning-of-code-or-line)
         ("<end>" . 'mwim-end-of-code-or-line)))

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

(use-package ace-window
  :bind (("C-x o" . 'ace-window))
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

(use-package undo-tree
  :delight
  :init
  (setq undo-tree-auto-save-history t
        undo-tree-visualizer-diff t
        undo-tree-visualizer-timestamps t)
  (global-undo-tree-mode t)
  :bind (("C-x u" . undo-tree-visualize)
         ("C-z"   . 'undo)
         ("C-S-z" .'undo-tree-redo))
  :config
  (progn
    (defun my/undo-tree-restore-default ()
      (setq undo-tree-visualizer-diff t))
    (advice-add 'undo-tree-visualizer-quit :after #'my/undo-tree-restore-default)))


(use-package text-mode
  :straight nil
  :custom
  (adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*")
  (adaptive-fill-first-line-regexp "^* *$")
  (sentence-end "\\([。、！？]\\|……\\|[,.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*"))

(provide 'core)
;;; config.el ends here
