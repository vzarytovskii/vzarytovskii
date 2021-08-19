;;; ui.el --- UI Configuration.
;;; Commentary:
;; User Interface config (theme, fonts, linenum, etc), for main config, see config.el

;;; -*- lexical-binding: t -*-

;;; Code:

(use-package frame
  :straight nil
  :config
  (blink-cursor-mode -1)
  (column-number-mode t)
  (global-display-line-numbers-mode 1)
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
  (add-to-list 'default-frame-alist '(internal-border-width . 0)))

(use-package all-the-icons)

;; TODO: Look at modus themes, with some customizations, they can be better readable than doom ones.
(use-package doom-themes
  :straight (:host github :repo "hlissner/emacs-doom-themes" :branch "master")
  :after all-the-icons
  :config

  ;; (fringe-mode 0)
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic nil)

  (setq pos-tip-background-color (face-background 'tooltip)
        pos-tip-foreground-color (face-foreground 'tooltip))


  (doom-themes-visual-bell-config)

  (load-theme 'doom-one t))

(use-package faces
  :straight nil
  :preface
  (defvar --font-name
    "Fira Code")
  (defvar --default-font
    (font-spec :family --font-name :size 12 :weight 'medium))
  (defvar --fixed-pitch-font
    (font-spec :family --font-name :size 10 :weight 'light))
  (defvar --variable-pitch-font
    (font-spec :family --font-name :size 10 :weight 'light))
  :config

  ;; (add-to-list 'default-frame-alist '(font . (font-face-attributes --default-font)))

  (apply 'set-face-attribute 'default nil (font-face-attributes --default-font))
  (apply 'set-face-attribute 'fixed-pitch nil (font-face-attributes --fixed-pitch-font))
  (apply 'set-face-attribute 'variable-pitch nil (font-face-attributes --variable-pitch-font)))

(use-package visual-fill-column)

(use-package smart-mode-line
  :straight (:host github :repo "vzarytovskii/smart-mode-line" :branch "master")
  :config
  (setq sml/theme 'respectful
        sml/no-confirm-load-theme t
        sml/modified-char "*"
        sml/shorten-directory t
        sml/shorten-modes t)
  (sml/setup))

(use-package mini-modeline
  :delight
  :straight (:host github :repo "kiennq/emacs-mini-modeline" :branch "master")
  :after smart-mode-line
  :config
  (setq mini-modeline-enhance-visual nil
        mini-modeline-display-gui-line nil)
  (mini-modeline-mode t))

(use-package display-line-numbers
  :ensure nil
  :hook (prog-mode-hook . display-line-numbers-mode)
  :config
  (setq-default display-line-numbers-width 5))

(use-package mixed-pitch
  :diminish)

(use-package solaire-mode
  :hook (after-init-hook . solaire-global-mode)
  :config
  (setq solaire-mode-auto-swap-bg t)
  (solaire-global-mode +1))

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
  :disabled t
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

(use-package highlight-numbers
  :hook (prog-mode-hook . highlight-numbers-mode))

(use-package digit-groups
  :config
  (digit-groups-global-mode t))

(use-package highlight-escape-sequences
  :hook (prog-mode . hes-mode))

(use-package whitespace
  :delight
  :disabled t
  :hook (prog-mode-hook . whitespace-mode)
  :config

  (set-face-background 'whitespace-space nil)
  (set-face-foreground 'whitespace-space "grey21")

  (set-face-background 'whitespace-newline nil)
  (set-face-foreground 'whitespace-newline "grey21")


  (setq-default whitespace-style
                '(face spaces
                       space-mark
                       tabs
                       newline
                       trailing-space-before
                       tab
                       space-after-tab
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

(use-package rainbow-identifiers
  :hook (prog-mode-hook . rainbow-identifiers-mode))

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

;;(use-package rainbow-delimiters
;;  :hook (prog-mode-hook . rainbow-delimiters-mode))

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

;; Custom colouring for languages:
(use-package prism
  :disabled t
  :straight (prism :host github :repo "alphapapa/prism.el" :branch "master")
  :hook (fsharp-mode-hook . prism-whitespace-mode)
  :hook ((elisp-mode-hook csharp-mode-hook) . prism-mode)
  :config
  (prism-set-colors :num 16
    :desaturations (cl-loop for i from 0 below 16
                            collect (* i 2.5))
    :lightens (cl-loop for i from 0 below 16
                       collect (* i 2.5))
    :colors (list "dodgerblue" "medium sea green" "sandy brown")

    :comments-fn
    (lambda (color)
      (prism-blend color
                   (face-attribute 'font-lock-comment-face :foreground) 0.25))

    :strings-fn
    (lambda (color)
      (prism-blend color "white" 0.5))))

(provide 'ui)
;;; ui.el ends here
