;;; ui.el --- UI Configuration.
;;; Commentary:
;; User Interface config (theme, fonts, linenum, etc), for main config, see config.el

;;; -*- lexical-binding: t -*-

;;; Code:

(use-package all-the-icons)

(use-package kaolin-themes
 :straight (:host github :repo "ogdenwebb/emacs-kaolin-themes" :branch "master")
  :after all-the-icons
  :preface
  (defvar --default-font
    (font-spec :family "JetBrains Mono" :height 95 :weight 'normal))
  (defvar --fixed-pitch-font
    (font-spec :family "JetBrains Mono" :height 100 :weight 'semi-bold))
  (defvar --variable-pitch-font
    (font-spec :family "JetBrains Mono" :height 100 :weight 'normal))
  :config

  (setq-default display-line-numbers-width 5)

  (setq default-frame-alist
        `((left-fringe . 15)
          (right-fringe . 15)
          (internal-border-width . 1)
          (font . ,(font-xlfd-name --default-font))))

  (setq kaolin-themes-bold t
        kaolin-themes-italic t
        kaolin-themes-underline t
        kaolin-themes-modeline-border nil
        kaolin-themes-underline-wave nil
        kaolin-themes-italic-comments nil
        kaolin-themes-hl-line-colored t
        kaolin-themes-distinct-fringe t
        kaolin-themes-distinct-company-scrollbar t
        kaolin-themes-git-gutter-solid nil)

  (setq kaolin-ocean-alt-bg t)

  (setq pos-tip-background-color (face-background 'tooltip)
        pos-tip-foreground-color (face-foreground 'tooltip))

  (apply 'set-face-attribute 'default nil (font-face-attributes --default-font))
  (apply 'set-face-attribute 'fixed-pitch nil (font-face-attributes --fixed-pitch-font))
  (apply 'set-face-attribute 'variable-pitch nil (font-face-attributes --variable-pitch-font))

  (load-theme 'kaolin-ocean t))

(use-package smart-mode-line
  :config
  (setq sml/no-confirm-load-theme t
        sml/shorten-directory t
        sml/shorten-modes t)
  (sml/setup))

(use-package mini-modeline
  :delight
  :straight (:host github :repo "kiennq/emacs-mini-modeline" :branch "master")
  :after smart-mode-line
  :config
  (mini-modeline-mode t))

(use-package display-line-numbers
  :ensure nil
  :hook (prog-mode-hook . display-line-numbers-mode))

(use-package mixed-pitch
  :diminish)

(use-package beacon
  :config
  (beacon-mode 1)
  (setq beacon-lighter ""
        beacon-blink-delay 0.5
        bracon-blink-duration 1.00
        beacon-size 75
        beacon-blink-when-point-moves-vertically nil
        beacon-blink-when-point-moves-horizontally nil
        beacon-blink-when-window-scrolls nil
        beacon-blink-when-buffer-changes nil
        beacon-blink-when-focused nil
        beacon-blink-when-window-changes t))

(use-package goggles
  :delight
  :straight (:host github :repo "minad/goggles" :branch "master")
  :config
  (goggles-mode)
  (setq-default goggles-pulse t))

(use-package hl-line
  :hook (after-init-hook . global-hl-line-mode))

(use-package highlight-indent-guides
  :delight
  :hook (prog-mode-hook . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-character ?\┊
        highlight-indent-guides-responsive 'top))

(use-package highlight-symbol
  :delight highlight-symbol-mode
  :hook (highlight-symbol-mode-hook . highlight-symbol-nav-mode)
  :hook (prog-mode-hook . highlight-symbol-mode)
  :custom
  (highlight-symbol-highlight-single-occurrence nil)
  (highlight-symbol-idle-delay 0)
  (highlight-symbol-on-navigation-p t))

(use-package region-occurrences-highlighter
  :bind (:map region-occurrences-highlighter-nav-mode-map
              ("M-n" . 'region-occurrences-highlighter-next)
              ("M-p" . 'region-occurrences-highlighter-prev))
  :hook (prog-mode-hook . region-occurrences-highlighter-mode)
  :hook (org-mode-hook . region-occurrences-highlighter-mode)
  :hook (text-mode-hook . region-occurrences-highlighter-mode))

(use-package highlight-parentheses
  :delight
  :hook (prog-mode-hook . highlight-parentheses-mode))

(use-package hl-todo
  :ensure
  :hook (prog-mode-hook . hl-todo-mode))

(use-package digit-groups
  :config
  (digit-groups-global-mode t))

(use-package whitespace
  :delight
  :hook (prog-mode-hook . whitespace-mode)
  :config

  (set-face-background 'whitespace-space nil)
  (set-face-foreground 'whitespace-space "grey24")

  (set-face-background 'whitespace-newline nil)
  (set-face-foreground 'whitespace-newline "grey24")


  (setq-default whitespace-style
                '(face spaces space-mark tabs newline
                       trailing-space-before
                       tab space-after-tab
                       newline-mark))
  (setq whitespace-display-mappings
        '((space-mark 32 [183] [46])
          (tab-mark 9 [9655 9] [92 9])
          (newline-mark 10 [36 10]))))

(use-package whitespace-cleanup-mode
  :delight
  :hook (before-save-hook . delete-trailing-whitespace)
  :hook (prog-mode-hook . whitespace-cleanup-mode)
  :bind (("<remap> <just-one-space>" . cycle-spacing)))

(use-package unicode-troll-stopper
  :delight
  :hook (prog-mode-hook . unicode-troll-stopper-mode))

(use-package aggressive-indent
  :hook (emacs-lisp-mode-hook . aggressive-indent-mode)
  :hook (csharp-mode-hook . aggressive-indent-mode)
  :hook (fsharp-mode-hook . aggressive-indent-mode)
  :hook (css-mode-hook . aggressive-indent-mode))

(use-package paren
  :config
  (setq show-paren-style 'parenthesis
        show-paren-when-point-in-periphery t
        show-paren-when-point-inside-paren t)
  ;; Display matching line for off-screen paren.
  (defun display-line-overlay (pos str &optional face)
    "Display line at POS as STR with FACE.
FACE defaults to inheriting from default and highlight."
    (let ((ol (save-excursion
                (goto-char pos)
                (make-overlay (line-beginning-position)
                              (line-end-position)))))
      (overlay-put ol 'display str)
      (overlay-put ol 'face
                   (or face '(:inherit highlight))) ol))

  (defvar-local show-paren--off-screen-overlay nil)
  (defun show-paren-off-screen (&rest _args)
    "Display matching line for off-screen paren."
    (when (overlayp show-paren--off-screen-overlay)
      (delete-overlay show-paren--off-screen-overlay))
    ;; Check if it's appropriate to show match info,
    (when (and (overlay-buffer show-paren--overlay)
               (not (or cursor-in-echo-area
                        executing-kbd-macro
                        noninteractive
                        (minibufferp)
                        this-command))
               (and (not (bobp))
                    (memq (char-syntax (char-before)) '(?\) ?\$)))
               (= 1 (logand 1 (- (point)
                                 (save-excursion
                                   (forward-char -1)
                                   (skip-syntax-backward "/\\")
                                   (point))))))
      ;; Rebind `minibuffer-message' called by `blink-matching-open'
      ;; to handle the overlay display.
      (cl-letf (((symbol-function #'minibuffer-message)
                 (lambda (msg &rest args)
                   (let ((msg (apply #'format-message msg args)))
                     (setq show-paren--off-screen-overlay
                           (display-line-overlay
                            (window-start) msg ))))))
        (blink-matching-open))))

  ;; TODO: Use ob to show off-screen parens
  (advice-add #'show-paren-function :after #'show-paren-off-screen)
  :hook (after-init-hook . show-paren-mode))

(use-package smartparens
  :delight
  :config
  (progn
    (require 'smartparens-config)
    (smartparens-global-mode 1)
    (show-paren-mode t)))

(use-package parinfer
  :delight
  :init
  (progn
    (setq parinfer-extensions
          '(defaults       ; should be included.
             pretty-parens  ; different paren styles for different modes.
             lispy          ; If you use Lispy. With this extension, you should install Lispy and do not enable lispy-mode directly.
             paredit        ; Introduce some paredit commands.
             smart-tab      ; C-b & C-f jump positions and smart shift with tab & S-tab.
             smart-yank))   ; Yank behavior depend on mode.
    (add-hook 'clojure-mode-hook #'parinfer-mode)
    (add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
    (add-hook 'common-lisp-mode-hook #'parinfer-mode)
    (add-hook 'lisp-mode-hook #'parinfer-mode)))

(use-package rainbow-delimiters
  :hook (prog-mode-hook . rainbow-delimiters-mode))

(use-package awesome-pair
  :straight (:host github :repo "manateelazycat/awesome-pair" :branch "master")
  :bind ((:map awesome-pair-mode-map
               ("(" . 'awesome-pair-open-round)
               ("[" . 'awesome-pair-open-bracket)
               ("{" . 'awesome-pair-open-curly)
               (")" . 'awesome-pair-close-round)
               ("]" . 'awesome-pair-close-bracket)
               ("}" . 'awesome-pair-close-curly)
               ("%" . 'awesome-pair-match-paren)
               ("\"" . 'awesome-pair-double-quote)
               ("DEL" . 'awesome-pair-backward-delete)
               ("C-k" . 'awesome-pair-kill)
               ("M-\"" . 'awesome-pair-wrap-double-quote)
               ("M-[" . 'awesome-pair-wrap-bracket)
               ("M-{" . 'awesome-pair-wrap-curly)
               ("M-(" . 'awesome-pair-wrap-round)
               ("M-]" . 'awesome-pair-unwrap)
               ("M-n" . 'awesome-pair-jump-right)
               ("M-p" . 'awesome-pair-jump-left)
               ("M-RET" . 'awesome-pair-jump-out-pair-and-newline)))
  :hook (((prog-mode web-mode conf-mode yaml-mode editorconfig-mode vue-mode) . awesome-pair-mode)
         ((c++-mode java-mode rust-mode) . (lambda () (local-set-key (kbd "<") '+prog/insert-angle)))
         (rust-mode . (lambda () (local-set-key (kbd "|") '+prog/insert-rust-closure))))
  :config
  (defun awesome-pair-in-string-p-advice (&optional state)
    (unless (or (bobp) (eobp))
      (save-excursion
        (or
         (and
          (nth 3 (or state (awesome-pair-current-parse-state)))
          (not (equal (point) (line-end-position))))
         (and
          (eq (get-text-property (point) 'face) 'font-lock-string-face)
          (eq (get-text-property (- (point) 1) 'face) 'font-lock-string-face))
         (and
          (eq (get-text-property (point) 'face) 'font-lock-doc-face)
          (eq (get-text-property (- (point) 1) 'face) 'font-lock-doc-face))
         ;; fix single quote pair delete for c/c++/java-mode
         (and
          (eq ?\" (char-syntax (char-before)))
          (eq ?\" (char-syntax (char-after (point)))))))))

  (advice-add 'awesome-pair-in-string-p :override 'awesome-pair-in-string-p-advice)

  (defun +prog/insert-angle ()
    "Insert angle brackets like intellij idea."
    (interactive)
    (save-excursion
      (let ((pos (point))
            (bounds (bounds-of-thing-at-point 'symbol)))
        (if bounds
            (let ((letter (char-after (car bounds))))
              (if (and (eq (upcase letter) letter)
                       (not (eq (downcase letter) letter)))
                  (insert "<>")
                (insert "<")))
          (insert "<"))))
    (forward-char))

  (defun +prog/insert-rust-closure ()
    (interactive)
    (save-excursion
      (if (and (equal major-mode 'rust-mode)
               (eq ?\( (char-before)))
          (insert "||")
        (insert "|")))
    (forward-char))

  (defun +prog/in-empty-pair-p (awesome-in-empty-pair-fn &rest args)
    (or (funcall awesome-in-empty-pair-fn)
        (and (eq ?> (char-after))
             (eq ?< (char-before)))
        (and (equal major-mode 'rust-mode)
             (eq ?| (char-after))
             (eq ?| (char-before)))))

  (advice-add 'awesome-pair-in-empty-pair-p :around '+prog/in-empty-pair-p)

  (defun +prog/fix-unbalanced-parentheses-or-forward-char ()
    "Fix missing close pair or just move forward one character."
    (interactive)
    (let ((close (awesome-pair-missing-close)))
      (if close
          (cond ((eq ?\) (matching-paren close))
                 (insert ")"))
                ((eq ?\} (matching-paren close))
                 (insert "}"))
                ((eq ?\] (matching-paren close))
                 (insert "]")))
        (forward-char))))

  (advice-add 'awesome-pair-fix-unbalanced-parentheses :override '+prog/fix-unbalanced-parentheses-or-forward-char))

(provide 'ui)
;;; ui.el ends here
