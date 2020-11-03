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

(use-package beacon
  :config
  (beacon-mode 1)
  (setq beacon-lighter ""
        beacon-blink-delay 0.1
        bracon-blink-duration 0.50
        beacon-size 35
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
  :delight
  :delight highlight-symbol-mode
  :hook (highlight-symbol-mode-hook . highlight-symbol-nav-mode)
  :hook (prog-mode-hook . highlight-symbol-mode)
  :custom
  (highlight-symbol-highlight-single-occurrence nil)
  (highlight-symbol-idle-delay 0)
  (highlight-symbol-on-navigation-p t))

(use-package hl-todo
  :ensure
  :hook (prog-mode-hook . hl-todo-mode))

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
        '(
          (space-mark 32 [183] [46]))))

(use-package whitespace-cleanup-mode
  :delight
  :init
  (setq whitespace-cleanup-mode-only-if-initially-clean nil)
  :hook ((after-init . global-whitespace-cleanup-mode))
  :bind (("<remap> <just-one-space>" . cycle-spacing)))

(use-package paren
  :config
  (setq show-paren-style 'parenthesis)
  (setq show-paren-when-point-in-periphery nil)
  (setq show-paren-when-point-inside-paren nil)
  :hook (after-init-hook . show-paren-mode))

(provide 'ui)
;;; ui.el ends here
