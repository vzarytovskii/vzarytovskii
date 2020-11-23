;;; ui.el --- UI Configuration.
;;; Commentary:
;; User Interface config (theme, fonts, linenum, etc), for main config, see config.el

;;; -*- lexical-binding: t -*-

;;; Code:

(use-package doom-themes
  :preface
  (defvar --default-font
    (font-spec :family "JetBrains Mono" :height 95 :weight 'normal))
  (defvar --fixed-pitch-font
    (font-spec :family "JetBrains Mono" :height 100 :weight 'semi-bold))
  (defvar --variable-pitch-font
    (font-spec :family "JetBrains Mono" :height 100 :weight 'normal))
  :config
  (setq-default display-line-numbers-width 3)

  (setq default-frame-alist
       `((left-fringe . 15)
         (right-fringe . 15)
         (internal-border-width . 0)
         (font . ,(font-xlfd-name --default-font))))

  (apply 'set-face-attribute 'default nil (font-face-attributes --default-font))
  (apply 'set-face-attribute 'fixed-pitch nil (font-face-attributes --fixed-pitch-font))
  (apply 'set-face-attribute 'variable-pitch nil (font-face-attributes --variable-pitch-font))

  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-tomorrow-night t)
  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

(use-package all-the-icons :if (display-graphic-p))

(use-package display-line-numbers
  :ensure nil
  :hook (prog-mode . display-line-numbers-mode))

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

(use-package hl-line
  :hook (after-init-hook . global-hl-line-mode))

(use-package hl-block-mode
  :config
  (setq hl-block-delay 0.3))

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
  :init
  (defface my-whitespace-face
    '((((class color) (background dark))
       :background nil :foreground "gray24")
      (((class color) (background light))
       :background nil :foreground "lightgray")
      (t :inverse-video t))
    "Face used to visualize SPACE."
    :group 'whitespace)
  
  (setq whitespace-space 'my-whitespace-face)
  
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
  :hook (prog-mode-hook . global-whitespace-cleanup-mode)
  :bind (("<remap> <just-one-space>" . cycle-spacing)))

(use-package unicode-troll-stopper
  :delight
  :hook (prog-mode-hook . unicode-troll-stopper-mode))

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
  (advice-add #'show-paren-function :after #'show-paren-off-screen)
  :hook (after-init-hook . show-paren-mode))

(use-package rainbow-delimiters
  :hook (prog-mode-hook . rainbow-delimiters-mode))

(provide 'ui)
;;; ui.el ends here
