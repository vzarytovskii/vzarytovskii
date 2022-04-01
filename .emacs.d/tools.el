;;; tools.el --- Emacs tools configuration. -*- lexical-binding: t; -*-
;;; Commentary:
;; Tools configuration (such as ripgrep, ivy, projectile, etc), for main config, see init.el

;;; Code:

(use-package deadgrep
  :preface
  (defun deadgrep--include-args (rg-args)
    (push "--color=auto" rg-args)
    (push "--hidden" rg-args)
    (push "--follow" rg-args))
  :if (executable-find "rg")
  :bind ("M-s" . 'deadgrep)
  :bind (:map deadgrep-mode-map
              ("M-e" . deadgrep-edit-mode)
              ("RET" . deadgrep-visit-result-other-window)
              ("o" . deadgrep-visit-result))
  :config
  (advice-add 'deadgrep--arguments :filter-return #'deadgrep--include-args))

(use-package flx)

(use-package prescient
  :config
  (setq prescient-filter-method '(literal regexp initialism fuzzy))
  (setq prescient-sort-length-enable nil)
  (prescient-persist-mode +1))

(use-package ivy
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
  (dired-mode-hook . diredfl-mode))

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

(provide 'tools)
;;; tools.el ends here
