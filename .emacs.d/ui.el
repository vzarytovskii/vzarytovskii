;;; ui.el --- UI Configuration.
;;; Commentary:
;; User Interface config (theme, fonts, linenum, etc), for main config, see config.el

;;; -*- lexical-binding: t -*-

;;; Code:

(use-package modus-vivendi-theme
  :after emacs
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
  
  (load-theme 'modus-vivendi t)
  :init
  (setq modus-vivendi-theme-distinct-org-blocks t
	modus-vivendi-theme-rainbow-headings t
	modus-vivendi-theme-slanted-constructs t
	modus-vivendi-theme-bold-constructs t
	modus-vivendi-theme-scale-headings t
	modus-vivendi-theme-scale-1 1.05
	modus-vivendi-theme-scale-2 1.1
	modus-vivendi-theme-scale-3 1.15
	modus-vivendi-theme-scale-4 1.2))

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

(provide 'ui)
;;; ui.el ends here
