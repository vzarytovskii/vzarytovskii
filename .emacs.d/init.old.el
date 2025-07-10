
(use-package flx)

(use-package prescient
  :config
  (setq prescient-filter-method '(literal regexp initialism fuzzy))
  (setq prescient-sort-length-enable nil)
  (prescient-persist-mode +1))

(use-package ivy
  :defer t
  :delight
  :after flx
  :preface
  (defun my/swiper (all)
      "Run `swiper' or `swiper-all'.
If a region is selected, the selected text is provided as initial input to
`swiper'. Otherwise, `swiper' is started without any initial input.
If ALL is non-nil, `swiper-all' is run."
      (interactive "P")
      (if all ; C-u
          (swiper-all)
        (if (use-region-p)
            (progn
              (deactivate-mark)
              (swiper (buffer-substring-no-properties
                       (region-beginning) (region-end))))
          (swiper))))
  (defun counsel-goto-local-home ()
    "Go to the $HOME of the local machine."
    (interactive)
    (ivy--cd "~/"))
  :init
  (use-package amx :defer t)
  (use-package counsel :delight :config (counsel-mode 1))
  (use-package swiper :defer t)
  (ivy-mode 1)
  :bind (("M-x" . counsel-M-x)
         ("C-s" . my/swiper)
         ("C-x b" . counsel-switch-buffer)
         ("C-x C-b" . counsel-switch-buffer)
         ("C-x f" . counsel-recentf)
         ("C-x C-f" . counsel-find-file)
         (:map swiper-map
               ("M-a" . 'swiper-avy))
         (:map ivy-minibuffer-map
               ("C-r" . ivy-previous-line-or-history)
               ("M-RET" . ivy-immediate-done))
         (:map counsel-find-file-map
               ("C-~" . counsel-goto-local-home)))
  :custom
  (ivy-height 10)
  (ivy-on-del-error-function nil)
  (ivy-magic-slash-non-match-action 'ivy-magic-slash-non-match-create)
  :config
  (setq ivy-use-virtual-buffers nil
        ivy-wrap t
        ivy-count-format "(%d/%d) "
        enable-recursive-minibuffers t)
  (push '(completion-at-point . ivy--regex-fuzzy) ivy-re-builders-alist) ;; This doesn't seem to work...
  (push '(swiper . ivy--regex-ignore-order) ivy-re-builders-alist)
  (push '(counsel-M-x . ivy--regex-ignore-order) ivy-re-builders-alist)

  ;; Set minibuffer height for different commands
  (setf (alist-get 'counsel-projectile-ag ivy-height-alist) 15)
  (setf (alist-get 'counsel-projectile-rg ivy-height-alist) 15)
  (setf (alist-get 'swiper ivy-height-alist) 15)
  (setf (alist-get 'counsel-switch-buffer ivy-height-alist) 7))

(use-package ivy-prescient
  :after (:all ivy prescient)
  :config
  (setq ivy-prescient-sort-commands
        '(:not swiper
               counsel-grep
               counsel-rg
               counsel-projectile-rg
               ivy-switch-buffer
               counsel-switch-buffer))

  (setq ivy-prescient-retain-classic-highlighting t)
  (ivy-prescient-mode +1))

(use-package ivy-rich
  :after ivy
  :init
  (ivy-rich-mode 1)
  :config
  (setq ivy-rich-display-transformers-list
        '(counsel-M-x
          (:columns
           ((counsel-M-x-transformer (:width 40))
            (ivy-rich-counsel-function-docstring (:face font-lock-doc-face)))))))

(use-package all-the-icons-ivy-rich
  :after ivy-rich
  :init (all-the-icons-ivy-rich-mode 1))

(use-package ivy-xref
  :after ivy
  :init
  (when (>= emacs-major-version 27)
    (setq xref-show-definitions-function #'ivy-xref-show-defs))
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

(use-package ivy-posframe
  :disabled t
  :delight
  :straight (:host github :repo "tumashu/ivy-posframe" :branch "master")
  :after ivy
  :config
  (setq ivy-posframe-parameters
        `((min-width . 100)
          (min-height . ,ivy-height)
          (left-fringe . 1)
          (right-fringe . 1)
          (internal-border-width . 10))
        ivy-display-functions-alist
        '((counsel-git-grep)
          (counsel-rg)
          (swiper)
          (counsel-irony . ivy-display-function-overlay)
          (ivy-completion-in-region . ivy-display-function-overlay)
          (t . ivy-posframe-display-at-frame-top-center)))
  (setq ivy-posframe-display-functions-alist
        '((complete-symbol . ivy-posframe-display-at-point)
          (t               . ivy-posframe-display-at-frame-top-center)))
  (ivy-posframe-mode 1))

(use-package dired
  :straight nil
  :config
  (setq dired-recursive-deletes 'always
        delete-by-moving-to-trash t
        dired-dwim-target t
        dired-listing-switches
        "-AGhlv --group-directories-first --time-style=long-iso"))

(use-package dired-x :straight nil)

;; Addtional syntax highlighting for dired
(use-package diredfl
  :after dired
  :hook
  (dired-mode-hook-hook . diredfl-mode))

;; Narrow a dired buffer to the files matching a stringx.
(use-package dired-narrow
  :after dired
  :bind (:map dired-mode-map ("C-c C-n" . dired-narrow)))

;; A poor man's treemacs
(use-package dired-subtree
  :after dired
  :bind (:map dired-mode-map ("TAB" . dired-subtree-toggle)))

;; Drop-in replacement for find-dired
(use-package fd-dired
  :after dired
  :bind (:map dired-mode-map ("C-c C-f" . fd-dired)))

(use-package dirvish
  :after dired
  :config
  (dirvish-override-dired-mode)
  ;;(dirvish-peek-mode)
  (setq dirvish-header-style 'normal)
  :bind (:map dired-mode-map
              ("SPC" . dirvish-show-history)
              ([remap dired-do-copy] . dirvish-yank)
              ("o" . dirvish-other-buffer)))
;; UI config

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
        modus-themes-diffs nil))

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
  :hook (write-file-hook . delete-trailing-whitespace)
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
          (eq ?\" (char-syntax (char-after (point))))))))) ;;"

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

;; Org config


;; Git config
(use-package git-modes
  :defer t)

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
  	magit-section-initial-visibility-alist
        '((unpulled . show)
          (unpushed . show)
          (untracked . show)
          (unstaged . show)
          (pullreqs . show)
          (issues . show)
          (stashes . show)
          (todos . show)
          (recent . show)))

  (magit-add-section-hook 'magit-status-sections-hook 'forge-insert-assigned-pullreqs nil t)
  (magit-add-section-hook 'magit-status-sections-hook 'forge-insert-assigned-issues nil t)

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

(use-package forge
  :after magit
  :config
  (setq forge-topic-list-limit '(100 . -10)))


(use-package git-commit-insert-issue
  :config
  (add-hook 'git-commit-mode-hook 'git-commit-insert-issue-mode))

(use-package graphql)

(use-package gh-notify
  :after (:all magit forge transient)
  :config
  (transient-insert-suffix 'forge-dispatch '(1)
    ["GitHub"
     ("g n" "notifications" gh-notify)]))

(use-package ghub)

(use-package magithub
  :disabled t
  :after (:all magit ghub)
  :config
  (magithub-feature-autoinject t)
  (setq magithub-clone-default-directory "~/code"))

(use-package github-explorer
  :after graphql)

;; Have multiple packages for PR handling, just to test them out.
(use-package github-review
  ;; :disabled t
  :after (:all magit forge transient)
  :straight (:host github :repo "charignon/github-review" :files ("github-review.el"))
  :config
  (transient-insert-suffix 'forge-dispatch '(1)
    ["GitHub Review"
     ("p g" "github-review at point" github-review-forge-pr-at-point)]))

(use-package code-review
  ;; :disabled t
  ;;:straight (:host github :repo "vzarytovskii/code-review")
  :after (:all magit forge transient)
  :config
  (setq code-review-new-buffer-window-strategy #'switch-to-buffer)
  (transient-insert-suffix 'forge-dispatch '(1)
    ["Code Review"
     ("p c" "code-review at point" code-review-forge-pr-at-point)]))

(use-package pr-review
  ;; :disabled t
  :straight (:host github :repo "blahgeek/emacs-pr-review" :files (:defaults "graphql"))
  :after (:all magit forge transient)
  :config
  (transient-insert-suffix 'forge-dispatch '(1)
    ["PR Review"
     ("p p" "pr-review at point" pr-review-notification-open)]))

(use-package git-link
  :bind (("C-x C-g i")))

(use-package git-blamed)

(use-package blamer
  :disabled t
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 70)
  ;;:custom-face
  ;;(blamer-face ((t :foreground "#7a88cf"
  ;;:background nil
  ;;:height 140
  ;;:italic t)))
  :config
  (setq blamer-author-formatter " ✎ %s "
	blamer-datetime-formatter "[%s]"
	blamer-commit-formatter " ● %s"
	blamer-prettify-time-p t
	blamer-type 'both)

  (global-blamer-mode 1))

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
  :init (setq git-messenger:show-detail t)
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

;; TODO:Move to common packages
(use-package transient
  :config
  (setq transient-default-level 5))

;; Programming confg
(use-package eldoc
  :straight nil
  :delight)

;;; Code:

(use-package editorconfig
  :delight
  :config
  (editorconfig-mode 1))

(use-package xref
  :ensure nil
  :bind (;;("C-." . xref-find-definitions)
         ("C-;" . 'counsel-imenu)))

(use-package imenu-anywhere
  :bind ("C-." . imenu-anywhere))

(use-package dumb-jump
  :commands (dumb-jump-xref-activate)
  :preface
  (defun override-dumb-jump-prompt-user-for-choice (proj results)
    (let ((choices (--map (dumb-jump--format-result proj it) results)))
      (funcall dumb-jump-ivy-jump-to-selected-function results choices proj)))
  :hook (xref-backend-functions . dumb-jump-xref-activate)
  :config
  (advice-add 'dumb-jump-prompt-user-for-choice :override #'override-dumb-jump-prompt-user-for-choice))

(use-package smart-jump
  ;; TODO: Use quickpeek for smart-jump-keep.
  ;; Package wasn't updated for a long time, xref should be used instead.
  :disabled t
  :after dumb-jump
  :bind (("M-." . smart-jump-go)
         ("M-," . smart-jump-back)
         ("M-/" . smart-jump-references)
         ("M-?" . smart-jump-references))
  :custom (dumb-jump-selector 'ivy)
  :config
  (smart-jump-setup-default-registers)
  (smart-jump-register :modes 'lsp-ui-mode
                       :jump-fn 'lsp-ui-peek-find-definitions
                       :refs-fn 'lsp-ui-peek-find-references
                       :pop-fn 'pop-tag-mark
                       :should-jump t
                       :heuristic 'point
                       :async 500)
  (smart-jump-register :modes '(emacs-lisp-mode lisp-interaction-mode)
                       :jump-fn 'elisp-slime-nav-find-elisp-thing-at-point
                       :pop-fn 'pop-tag-mark
                       :should-jump t
                       :heuristic 'error
                       :async nil))

;; Flymake and flymake configs:

(use-package flycheck
  :delight
  :defer t
  :hook (after-init-hook . global-flycheck-mode)
  :config
  (setq flycheck-highlighting-mode 'sexps))

;; TODO: Temporary disable inline on-demand error display.
(use-package flycheck-inline
  :disabled t
  :after flycheck
  :preface

  (setq flycheck-ov (make-overlay 0 0))
  (overlay-put flycheck-ov 'invisible t)
  (overlay-put flycheck-ov 'face '(:extend t :foreground "gray50" :background "#ff0000"))

  (defun fc-inline-overlay (msg &optional pos err)
    ;; MSG - message from flycheck
    ;; POS - position marker
    ;; ERR - flycheck-error object, other (including level, id, message, filename) can be extracted.

    ;; TODO define face here dependung on level/severity.
    ;; TODO: Clear overlay when leaving the line.
    (let ((beg (point-at-eol))
          (end (+ 1 (point-at-eol))))
      ;; (end (min (point-max) (+ 1 (point-at-eol)))))
      (move-overlay flycheck-ov beg end)
      (overlay-put flycheck-ov 'after-string (format "\t//\s%s\n" msg))))
  ;;(ov-set (ov-line) 'after-string (propertize (format "%s" msg) 'face '(:extend t :foreground "gray50")) 'ovfc t)

  (defun fc-inline-overlay-clear ()
    (overlay-put flycheck-ov 'invisible t))

  :config
  (setq flycheck-inline-display-function 'fc-inline-overlay
        flycheck-inline-clear-function 'fc-inline-overlay-clear)
  :hook (flycheck-mode-hook . flycheck-inline-mode))

;; Company mode config:
(use-package company
  :delight
  :straight (:host github :repo "company-mode/company-mode" :branch "master")
  :hook (after-init-hook . global-company-mode)
  :bind (:map company-active-map
              ("C-w" . 'backward-kill-word))
  :config
  (setq company-minimum-prefix-length 2
        company-idle-delay (lambda ()
                             (if (company-in-string-or-comment) nil 0.1))
        company-require-match nil
        company-frontends
        '(company-pseudo-tooltip-unless-just-one-frontend-with-delay
          company-preview-frontend
          company-echo-metadata-frontend)
        company-tooltip-align-annotations t
        company-tooltip-limit 20
        company-tooltip-offset-display 'lines
        company-tooltip-flip-when-above t
        company-text-icons-add-background t
        company-show-quick-access 'right)

  (push 'company-capf company-backends))

(use-package company-quickhelp
  :after company
  :hook (global-company-mode-hook . company-quickhelp-mode)
  :config
  (setq pos-tip-use-relative-coordinates t
        company-quickhelp-delay 0.0))

(use-package company-box
  :disabled t
  :delight
  :after (:all all-the-icons company)
  :functions (my-company-box--make-line
              my-company-box-icons--elisp)
  :commands (company-box--get-color
             company-box--resolve-colors
             company-box--add-icon
             company-box--apply-color
             company-box--make-line
             company-box-icons--elisp)
  :hook (company-mode-hook . company-box-mode)
  :custom
  (company-box-backends-colors nil)
  (company-box-show-single-candidate t)
  (company-box-max-candidates 50)
  (company-box-doc-delay 0.3))

(use-package company-prescient
  :after (:all company prescient))

(use-package company-posframe
  :disabled t
  :delight
  :if (and (>= emacs-major-version 26)
           (display-graphic-p))
  :after (:all all-the-icons company)
  :config
  (company-posframe-mode 1))

(use-package copilot
  :after company
  :disabled t
  :straight (:host github :repo "zerolfx/copilot.el"
                   :files ("dist" "copilot.el"))
  :hook (prog-mode-hook . copilot-mode)
  :config
  (defun my-tab ()
    (interactive)
    (or (copilot-accept-completion)
        (company-indent-or-complete-common nil)))

  (delq 'company-preview-if-just-one-frontend company-frontends)
  (define-key company-mode-map (kbd "<tab>") 'my-tab)
  (define-key company-mode-map (kbd "TAB") 'my-tab)
  (define-key company-active-map (kbd "<tab>") 'my-tab)
  (define-key company-active-map (kbd "TAB") 'my-tab))

;; Snippets config
(use-package yasnippet
  :defer t
  :after company
  :delight yas-minor-mode
  :hook (prog-mode-hook . yas-global-mode)
  :config
  (defvar company-mode/enable-yas t
    "Enable yasnippet for all backends.")

  (defun company-mode/backend-with-yas (backend)
    (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))

  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

  (setq yas-snippet-dirs '("~/.emacs.d/snippets")))

(use-package yasnippet-snippets
  :defer t
  :after yasnippet)

;; Tree-sitter config
(use-package tsc
  :defer t
  :straight (tsc :host github :repo "ubolonton/emacs-tree-sitter" :files ("core/*.el")))

(use-package tree-sitter
  :defer t
  :if (executable-find "tree-sitter")
  :straight (tree-sitter :type git :host github :repo "ubolonton/emacs-tree-sitter" :files ("lisp/*.el"))
  :custom-face
  (tree-sitter-hl-face:method.call   ((t (:inherit font-lock-function-name-face))))
  (tree-sitter-hl-face:function.call ((t (:inherit font-lock-function-name-face))))
  (tree-sitter-hl-face:operator      ((t (:inherit default))))
  (tree-sitter-hl-face:type.builtin  ((t (:inherit font-lock-type-face))))
  (tree-sitter-hl-face:number        ((t (:inherit highlight-numbers-number))))
  :hook (((python-mode-hook
           typescript-mode-hook) . tree-sitter-mode)
         ((tree-sitter-after-on-hook
           python-mode-hook
           typescript-mode-hook) . tree-sitter-hl-mode))
  :config
  (add-to-list 'tree-sitter-major-mode-language-alist
               '(rustic-mode . rust)))

(use-package tree-sitter-langs
  :defer t
  :if (executable-find "tree-sitter")
  :straight (tree-sitter-langs :type git :host github :repo "ubolonton/emacs-tree-sitter" :files ("langs/*.el" "langs/queries"))
  :after tree-sitter)

;; LSP Configuration:

(use-package lsp-mode
  :commands (lsp lsp-deferred lsp-mode)
  :config
  (setq max-specpdl-size 32000) ;; A workaround when running gccemacs in WSL
  (setq lsp-auto-guess-root nil
        lsp-debounce-full-sync-notifications-interval 1.0
        lsp-diagnostic-package :flycheck
        lsp-diagnostics-attributes '((deprecated :strike-through t))
        lsp-document-sync-method 'incremental
        lsp-eldoc-enable-hover nil
        lsp-eldoc-enable-signature-help nil
        lsp-enable-file-watchers t
        lsp-enable-folding t
        lsp-enable-indentation t
        lsp-enable-on-type-formatting t
        lsp-enable-snippet nil
        lsp-enable-symbol-highlighting t
        lsp-enable-xref t
        lsp-flycheck-live-reporting t
        lsp-headerline-breadcrumbs-mode t
        lsp-idle-delay 1
        lsp-keep-workspace-alive nil
        lsp-lens-debounce-interval 1.5
        lsp-navigation 'both
        lsp-prefer-capf t
        lsp-prefer-flymake nil
        lsp-response-timeout 10
        lsp-semantic-highlighting t
        lsp-session-file "~/.lsp-sessions"
        lsp-signature-auto-activate t
        lsp-signature-render-all t
        lsp-headerline-breadcrumb-segments '(file symbols)))

(use-package lsp-treemacs :after lsp)

(use-package lsp-ui
  :delight
  :after lsp-mode
  :hook (lsp-after-open-hook . lsp-ui-mode)
  :hook (lsp-after-open-hook . lsp-lens-mode)
  :hook (lsp-after-open-hook . lsp-signature-mode)
  :hook (lsp-after-open-hook . lsp-ui-sideline-mode)
  :hook (lsp-after-open-hook . lsp-headerline-breadcrumb-mode)
  :bind (:map lsp-ui-mode-map
              ;; TODO: move to remap instead of specifying keys,
              ("C-;" . lsp-ui-imenu)
              ("C-." . lsp-ui-imenu)
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references))
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-header nil
        ;;lsp-ui-doc-border "green"
        lsp-ui-doc-max-height 50
        lsp-ui-doc-max-width 150
        lsp-ui-doc-include-signature t
        lsp-ui-doc-delay 0.0
        lsp-ui-doc-position 'bottom
        lsp-ui-doc-use-childframe t
        lsp-ui-flycheck-enable t
        lsp-ui-flycheck-list-position 'right
        lsp-ui-peek-enable t
        lsp-ui-peek-fontify 'always
        lsp-ui-peek-peek-height 30
        lsp-ui-peek-list-width 60
        lsp-ui-sideline-enable t
        lsp-ui-sideline-update-mode 'line
        lsp-ui-sideline-show-symbol nil
        lsp-ui-sideline-show-diagnostics t
        lsp-ui-sideline-show-hover t
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-sideline-delay 0.0
        lsp-ui-imenu-enable t
        lsp-ui-imenu-kind-position 'top))

(use-package ruled-switch-buffer
  :straight (:host github :repo "kzkn/ruled-switch-buffer" :branch "main")
  :config
  (ruled-switch-buffer-define fs-to-fsi
    :matcher (lambda (fn) (string-match ".fs$" fn))
    :mappers (lambda (fn) (replace-regexp-in-string "\\.fs$" ".fsi" fn)))
  (ruled-switch-buffer-define fsi-to-fs
    :matcher (lambda (fn) (string-match ".fsi$" fn))
    :mappers (lambda (fn) (replace-regexp-in-string "\\.fsi$" ".fs" fn))));; Language-specific configs:
;; .NET
(use-package dotnet
  :config
  (setenv "DOTNET_USE_POLLING_FILE_WATCHER" "true"))

(use-package sharper
  :straight (:host github :repo "sebasmonia/sharper" :branch "master")
  :bind ("C-x n" . sharper-main-transient))

(use-package fsharp-mode
  ;; :straight (:host github :repo "vzarytovskii/emacs-fsharp-mode" :branch "master")
  :defer t
  :after (:all sharper dotnet lsp-mode)
  :commands fsharp-mode
  :hook (fsharp-mode-hook . lsp)
  :hook (fsharp-mode-hook . dotnet-mode)
  :config
  (setq indent-tabs-mode nil
        truncate-lines t
        tab-width 4)
  (setq fsharp-doc-idle-delay 0.0
        fsharp-ac-use-popup t
        fsharp-ac-intellisense-enabled t
        fsharp-smart-indentation t
        fsharp-indent-offset 4
        inferior-fsharp-program "dotnet fsi"
        lsp-fsharp-server-runtime 'net-core
        ;;lsp-fsharp-server-install-dir "~/code/fsautocomplete/bin/release_netcore"
        lsp-fsharp-server-args '("--adaptive-lsp-server-enabled")
        lsp-fsharp-keywords-autocomplete t
        lsp-fsharp-external-autocomplete t
        lsp-fsharp-linter t
        lrsp-fsharp-union-case-stub-generation t
        lsp-fsharp-union-case-stub-generation-body "failwith \"TODO\""
        lsp-fsharp-record-stub-generation t
        lsp-fsharp-record-stub-generation-body "failwith \"TODO\""
        lsp-fsharp-interface-stub-generation t
        lsp-fsharp-interface-stub-generation-object-identifier "_"
        lsp-fsharp-interface-stub-generation-method-body "failwith \"TODO\""
        lsp-fsharp-unused-opens-analyzer t
        lsp-fsharp-unused-declarations-analyzer t
        lsp-fsharp-simplify-name-analyzer t
        lsp-fsharp-resolve-namespaces t
        lsp-fsharp-enable-reference-code-lens t
        lsp-fsharp-auto-workspace-init t
        lsp-fsharp-exclude-directories ["paket-files" ".git" "packages" "node_modules"]
        lsp-log-io t)
  (add-to-list 'company-transformers 'company-sort-prefer-same-case-prefix)
  (setq indent-region-function '(lambda (start end &optional indent-offset))))

;; Rust
(use-package rustic
  :defer t
  :hook ('rustic-mode-hook . 'lsp-mode)
  :config
  (setq rustic-lsp-server 'rust-analyzer
        rustic-lsp-client 'lsp-mode))

;; YAML
(use-package yaml-mode)

(use-package flymake-yamllint
  :straight (:host github :repo "shaohme/flymake-yamllint" :branch "main")
  :after (:all yaml-mode flymake)
  :hook (yaml-mode-hook . flymake-yamllint-setup))

;; JSON
(use-package json-mode
  :mode "\\.json\\'")

(use-package json-reformat
  :after json-mode)

(use-package json-snatcher
  :after json-mode)

;; Docker
(use-package docker
  :if (executable-find "docker"))

(use-package dockerfile-mode
  :config
  (setq dockerfile-mode-command "docker"))

(use-package docker-compose-mode
  :if (executable-find "docker-compose"))

;; Projectile
(use-package projectile
  :delight
  :hook (after-init-hook . projectile-mode)
  :bind ("C-c C-p" . 'projectile-command-map)
  :bind (:map projectile-command-map
              ("C-x C-f" . 'projectile-find-file)
              ("C-x f" . 'find-file))
  :preface
  (defvar +project/lsp-project-root-cache (make-hash-table :test 'equal)
    "Cached value of function `+project/lsp-project-root`.")

  (defun +project/lsp-project-root (&optional dir)
    (let* ((dir (or dir default-directory))
           (cache-key dir)
           (cache-value (gethash cache-key +project/lsp-project-root-cache)))
      (if (and cache-value (file-exists-p cache-value))
          cache-value
        (let* ((lsp-folders (lsp-session-folders (lsp-session)))
               (value (cl-find-if
                       (lambda (path)
                         (and
                          ;; fast filter to improve `ivy-rich-switch-buffer-root' performance, but not accurate
                          (string-prefix-p path (expand-file-name dir))
                          ;; double check if current dir in the lsp-project roots
                          (file-in-directory-p dir path)))
                       lsp-folders)))
          (puthash cache-key value +project/lsp-project-root-cache)
          value))))

  (defalias '+project/root 'projectile-project-root)

  (defun +project/projectile-buffer-filter (buffer)
    (let ((name (buffer-name buffer)))
      (or (and (string-prefix-p "*" name)
               (not (string-prefix-p "*eww*" name))
               (not (string-prefix-p "*ein: http" name))
               (not (string-prefix-p "*ein:notebooklist" name))
               (not (string-prefix-p "*vterm:" name))
               (not (string-prefix-p "*cider" name)))
          (string-match-p "magit.*:" name)
          (equal (buffer-name (current-buffer)) name))))

  (defun +project/projectile-buffer-filter-function (buffers)
    (cl-remove-if
     (lambda (buffer) (+project/projectile-buffer-filter buffer))
     buffers))

  :config
  (setq projectile-project-search-path '("~/code/")
        projectile-auto-discover t
        projectile-enable-caching t
        projectile-indexing-method 'alien
        projectile-globally-ignored-file-suffixes '("#" "~" ".swp" ".o" ".so" ".exe" ".dll" ".elc" ".pyc" ".jar")
        projectile-globally-ignored-directories '(".git" "node_modules" "__pycache__" ".vs")
        projectile-globally-ignored-files '("TAGS" "tags" ".DS_Store")
        projectile-completion-system 'ivy)

  (add-to-list 'projectile-project-root-files-functions #'+project/lsp-project-root))

(use-package counsel-projectile
  :after (:all counsel projectile)
  :bind ("C-x C-p" . counsel-projectile)
  ;; :bind ("C-x f" . counsel-projectile-find-file)
  :bind ("C-x s". counsel-projectile-rg))

;; Misc programming-related (i.e. tramp, devcontainers, etc)
(use-package codespaces
  :if (executable-find "gh")
  :config (codespaces-setup))