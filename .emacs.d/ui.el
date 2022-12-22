;;; ui.el --- UI Configuration. -*- lexical-binding: t; -*-
;;; Commentary:
;; User Interface config (theme, fonts, linenum, etc), for main config, see config.el

;;; Code:

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
  (add-to-list 'default-frame-alist '(internal-border-width . 0)))

(use-package fullframe
  :after magit
  :config
  (fullframe magit-status magit-mode-quit-window))

(use-package all-the-icons)

(use-package kaolin-themes
  :disabled t)

(use-package doom-themes
  :disabled t
  :config
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; (load-theme 'doom-one t)

  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

;; If we running Linux, use GTK theme when switching

(use-package modus-themes
  :init
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-italic-constructs nil
        modus-themes-bold-constructs nil
        modus-themes-mixed-fonts nil
        modus-themes-subtle-line-numbers nil
        modus-themes-deuteranopia nil
        modus-themes-tabs-accented t
        modus-themes-variable-pitch-ui nil
        modus-themes-inhibit-reload t
        modus-themes-fringes 'subtle
        modus-themes-syntax '(green-strings)
        modus-themes-hl-line '(intense accented)
        modus-themes-paren-match '(bold)
        modus-themes-completions '((matches   . (extrabold))
                                   (selection . (semibold accented))
                                   (popup     . (accented intense)))
        modus-themes-region '(bg-only no-extend)
        modus-themes-diffs nil)

  ;; Load the theme files before enabling a theme
  (modus-themes-load-themes))

(defvar dark-theme 'modus-vivendi)
(defvar light-theme 'modus-operandi)

(use-package dbus
  :after modus-themes
  :if (and *sys/is-linux* (not *sys/is-wsl*))
  :preface
  (defun call-process-string (program &rest args)
    "Call process`PROGRAM' with `ARGS' and return the output as string."
    (with-temp-buffer
      (apply #'call-process program nil t nil args)
      (buffer-string)))
  (defun set-theme-from-gtk ()
    "Set modus theme by checking whether GTK theme is dark."
    (message "Setting GTK theme")
    (let ((gtk-theme (downcase
                      (call-process-string "gsettings"
                                           "get"
                                           "org.gnome.desktop.interface"
                                           "gtk-theme"))))
      (message "Gtk theme: %s" gtk-theme)
      (if (or (string-match-p "dark"  gtk-theme)
              (string-match-p "black" gtk-theme))
          (load-theme dark-theme t)
        (load-theme light-theme t))))

  (defun gtk-theme-changed (path _ _)
    "DBus handler to detect when the GTK theme has changed."
    (when (string-equal path "/org/gnome/desktop/interface/gtk-theme")
      (set-theme-from-gtk)))
  :config
  (dbus-register-signal
   :session
   "ca.desrt.dconf"
   "/ca/desrt/dconf/Writer/user"
   "ca.desrt.dconf.Writer"
   "Notify"
   #'gtk-theme-changed)
  (set-theme-from-gtk))

;; If we running anything else (including WSL), we use location to switch theme.
(use-package theme-changer
  :after modus-themes
  :if (or *sys/is-wsl* (not *sys/is-linux*))
  :init
  (setq calendar-location-name "Prague, CR"
        calendar-latitude 50.0755
        calendar-longitude 14.4378)
  :config
  (change-theme light-theme dark-theme))

(use-package faces
  :straight nil
  :preface

  ;; (defun my-dpi ()
  ;;   (let* ((attrs (car (display-monitor-attributes-list)))
  ;;          (size (assoc 'mm-size attrs))
  ;;          (sizex (cadr size))
  ;;          (res (cdr (assoc 'geometry attrs)))
  ;;          (resx (- (caddr res) (car res)))
  ;;          dpi)
  ;;     (catch 'exit
  ;;       ;; in terminal
  ;;       (unless sizex
  ;;         (throw 'exit 10))
  ;;       ;; on big screen
  ;;       (when (> sizex 1000)
  ;;         (throw 'exit 10))
  ;;       ;; DPI
  ;;       (* (/ (float resx) sizex) 25.4))))

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
       ((> dpi 160) 18)
       (t 14))))

  (message "Initial preferred font size: %d" (my-preferred-font-size))

  (defvar --font-name
    "Fira Code")
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

(use-package visual-fill-column)

;; Modeline

(use-package doom-modeline
  :disabled t
  :hook (after-init-hook . doom-modeline-mode)
  :config
  (setq doom-modeline-height 25
        doom-modeline-bar-width 4
        doom-modeline-hud nil
        doom-modeline-window-width-limit fill-column
        doom-modeline-project-detection 'auto
        doom-modeline-buffer-file-name-style 'auto
        doom-modeline-icon *sys/gui*
        doom-modeline-major-mode-icon t
        doom-modeline-major-mode-color-icon t
        doom-modeline-buffer-state-icon t
        doom-modeline-buffer-modification-icon t
        doom-modeline-unicode-fallback nil
        doom-modeline-minor-modes nil
        doom-modeline-enable-word-count nil
        doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode)
        doom-modeline-buffer-encoding t
        doom-modeline-indent-info nil
        doom-modeline-checker-simple-format t
        doom-modeline-number-limit 99
        doom-modeline-vcs-max-length 12
        oom-modeline-workspace-name t
        doom-modeline-persp-name t
        doom-modeline-display-default-persp-name nil
        doom-modeline-persp-icon t
        doom-modeline-lsp t
        doom-modeline-github t
        doom-modeline-github-interval (* 30 60)
        doom-modeline-modal-icon t
        doom-modeline-mu4e nil
        doom-modeline-gnus t
        doom-modeline-gnus-timer 2
        doom-modeline-irc nil
        doom-modeline-irc-stylize 'identity
        doom-modeline-env-version t
        doom-modeline-env-enable-python t
        doom-modeline-env-enable-ruby t
        doom-modeline-env-enable-perl t
        doom-modeline-env-enable-go t
        doom-modeline-env-enable-elixir t
        doom-modeline-env-enable-rust t
        doom-modeline-env-python-executable "python"
        doom-modeline-env-ruby-executable "ruby"
        doom-modeline-env-perl-executable "perl"
        doom-modeline-env-go-executable "go"
        doom-modeline-env-elixir-executable "iex"
        doom-modeline-env-rust-executable "rustc"
        doom-modeline-env-load-string "..."
        doom-modeline-before-update-env-hook nil
        doom-modeline-after-update-env-hook nil))

(use-package smart-mode-line
  ;; :disabled t
  :straight (:host github :repo "vzarytovskii/smart-mode-line" :branch "master")
  :config
  (setq sml/theme 'respectful
        sml/no-confirm-load-theme t
        sml/modified-char "*"
        sml/shorten-directory t
        sml/shorten-modes t)
  (sml/setup))

(use-package mini-modeline
  ;; :disabled t
  :delight
  :straight (:host github :repo "andersjohansson/emacs-mini-modeline" :branch "29-mode-line-faces")
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

(use-package dimmer
  :disabled t
  :config
  (setq dimmer-adjustment-mode :both
	dimmer-watch-frame-focus-events t
        dimmer-exclusion-predicates '(window-minibuffer-p)
        dimmer-exclusion-regexp-list '("^\\*Minibuf-[0-9]+\\*" "^*Messages*")
        dimmer-fraction 0.35)

  (dimmer-configure-company-box)
  (dimmer-configure-which-key)
  (dimmer-configure-org)
  (dimmer-configure-posframe)
  (dimmer-configure-magit)

  (dimmer-mode t))

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
  :straight (:host github :repo "minad/goggles" :branch "main")
  :config
  (goggles-mode)
  (setq-default goggles-pulse t))

(use-package hl-line
  :hook ((vterm-mode-hook . (lambda() (setq-local global-hl-line-mode nil)))
         (comint-mode-hook . (lambda () (setq-local global-hl-line-mode nil)))
         (text-mode-hook . global-hl-line-mode)
         (prog-mode-hook . global-hl-line-mode)))

(use-package highlight-indent-guides
  :disabled t
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

(use-package highlight-numbers
  :hook (prog-mode-hook . highlight-numbers-mode))

(use-package digit-groups
  :disabled t
  :config
  (digit-groups-global-mode t))

(use-package visual-fill-column)

(use-package highlight-escape-sequences
  :hook (after-init-hook . hes-mode))

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
  :disabled t
  :hook (write-file-hooks . delete-trailing-whitespace)
  :hook (prog-mode-hook . whitespace-cleanup-mode)
  :bind (("<remap> <just-one-space>" . cycle-spacing)))

(use-package unicode-troll-stopper
  :delight
  :hook (prog-mode-hook . unicode-troll-stopper-mode))

(use-package aggressive-indent
  :delight
  :straight (:host github :repo "skangas/aggressive-indent-mode" :branch "important-fix")
  :hook (emacs-lisp-mode-hook . aggressive-indent-mode)
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
  :disabled t
  :delight
  :config
  (progn
    (require 'smartparens-config)
    (smartparens-global-mode 1)
    (show-paren-mode t)))

(use-package parinfer
  :disabled t
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
                                        ;(add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
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
               ;; ("C-k" . 'awesome-pair-kill)
               ("M-RET" . 'awesome-pair-jump-out-pair-and-newline)))
  :hook (((prog-mode-hook web-mode-hook conf-mode-hook yaml-mode-hook editorconfig-mode-hook vue-mode-hook) . awesome-pair-mode)
         ((c++-mode-hook java-mode-hook rust-mode-hook) . (lambda () (local-set-key (kbd "<") '+prog/insert-angle)))
         (rust-mode-hook . (lambda () (local-set-key (kbd "|") '+prog/insert-rust-closure))))
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
  :hook ((elisp-mode-hook) . prism-mode)
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
