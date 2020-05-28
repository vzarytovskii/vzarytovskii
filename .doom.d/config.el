
;;;  -*- lexical-binding: t; -*-

;; Font locking is the source of much slowness in Emacs. jit-lock-mode tries to
;; defer fontification until the user is idle. This should help... in theory.
;; TODO: Refactor some of use-package! to after! instead of use-package!
;; TODO: Ensure that packages are loaded deferred.
;; TODO: Reorder, sort, and join some settings into blocks by category.
;; TODO: Move some of these to separate modules, grouped by category.
;; TODO: Look at existing literate configs and how they're organized, e.g.: https://github.com/dangirsh/.doom.d

(setq jit-lock-defer-time 0    ; only defer while processing input
      jit-lock-stealth-time 2) ; fontify the rest of the buffer after a delay

(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

(setq bidi-paragraph-direction 'left-to-right)

(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
(setq package-check-signature nil)
(package-initialize)

(setq +doom-dashboard-banner-padding '(0 . 0))
(setq +doom-dashboard-functions
      '(doom-dashboard-widget-shortmenu))

(setq user-full-name "Vlad Zarytovskii"
      user-mail-address "vzaritovsky@hotmail.com"
      doom-themes-treemacs-line-spacing 0
      doom-themes-treemacs-enable-variable-pitch t
      doom-modeline-height 22
      doom-theme 'doom-acario-dark
      doom-font (font-spec :family "JetBrains Mono" :size 17)
      all-the-icons-scale-factor 1)


(setq org-directory "~/org/")
(setq display-line-numbers-type 'relative)

(setq split-width-threshold nil)
(setq split-width-threshold 160)

(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)
(setq initial-scratch-message nil)

(setq uniquify-buffer-name-style 'post-forward-angle-brackets) ; Show path if names are same
(setq adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*")
(setq adaptive-fill-first-line-regexp "^* *$")
(setq sentence-end "\\([。、！？]\\|……\\|[,.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
(setq sentence-end-double-space nil)
(setq delete-by-moving-to-trash t)    ; Deleting files go to OS's trash folder
(setq make-backup-files nil)          ; Forbide to make backup files
(setq auto-save-default nil)          ; Disable auto save
(setq set-mark-command-repeat-pop t)  ; Repeating C-SPC after popping mark pops it again
(setq track-eol t)      ; Keep cursor at end of lines.
(setq line-move-visual nil)    ; To be required by track-eol
(setq-default kill-whole-line t)  ; Kill line including '\n'
(setq-default indent-tabs-mode nil)   ; use space
(defalias 'yes-or-no-p #'y-or-n-p)

;; Binds:
(map! "C-w" 'doom/delete-backward-word)

(defun display-startup-echo-area-message ()
  (message ""))

(set-popup-rule! "*backtrace\*"      :size 0.5            :side 'bottom :select t :quit t :modeline t)
(set-popup-rule! "*doom:scratch"     :size 0.25 :vslot -4 :side 'bottom :select t :quit t :ttl nil :modeline nil)

;; Thin grey line separating windows
(set-face-background 'vertical-border "grey")
(set-face-foreground 'vertical-border (face-background 'vertical-border))

(use-package! doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t      ; if nil, bold is universally disabled
        doom-themes-enable-italic t)   ; if nil, italics is universally disabled
  ;; (load-theme 'doom-acario-dark t)
  ;; (load-theme 'doom-one-light t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; Auto theme switch
;; -- Automatically switch between ligh and dark theme based on time of day
(setq theme-autoswitch t)
(setq theme-autoswitch/light-theme 'doom-acario-light)
(setq theme-autoswitch/dark-theme 'doom-acario-dark)
(setq theme-autoswitch/day-start-hour 7)
(setq theme-autoswitch/day-end-hour 19)
(setq theme-autoswitch/sync-timer 300)
(if (and theme-autoswitch (display-graphic-p))
    (progn
      (defun sync-theme-with-time ()
        (setq theme-autoswitch/hour (string-to-number (substring (current-time-string) 11 13)))
        (if (member theme-autoswitch/hour (number-sequence theme-autoswitch/day-start-hour theme-autoswitch/day-end-hour))
            (setq theme-autoswitch/now theme-autoswitch/light-theme)
          (setq theme-autoswitch/now theme-autoswitch/dark-theme))
        (unless (and (boundp 'current-theme) (eq theme-autoswitch/now current-theme))
          (progn
            (setq current-theme theme-autoswitch/now)
            (load-theme theme-autoswitch/now t))))
      (sync-theme-with-time)
      (run-with-timer 0 theme-autoswitch/sync-timer #'sync-theme-with-time))
  (load-theme theme-autoswitch/dark-theme t))

(defun howdoi ()
  "Call `howdoi' and ask for help"
  (interactive)
  (let* ((query (read-from-minibuffer "Query: "))
         (docstring (shell-command-to-string (concat "howdoi " query)))
         (buffer-name "*How do I do it?*"))
    (when (get-buffer buffer-name)
      (kill-buffer buffer-name))
    (get-buffer-create buffer-name)
    (with-current-buffer buffer-name (insert docstring))
    (switch-to-buffer-other-window buffer-name)
    (special-mode)))

(defun restclient ()
  "Open the restclient buffer."
  (interactive)
  (with-current-buffer (get-buffer-create "*restclient*")
    (restclient-mode)
    (pop-to-buffer (current-buffer))))

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

(global-set-key (kbd "C-x |") 'toggle-window-split)

(defun vsplit-last-buffer ()
  "Split the window vertically and display the previous buffer."
  (interactive)
  (split-window-vertically)
  (other-window 1 nil)
  (switch-to-next-buffer))

(defun hsplit-last-buffer ()
  "Split the window horizontally and display the previous buffer."
  (interactive)
  (split-window-horizontally)
  (other-window 1 nil)
  (switch-to-next-buffer))

(global-set-key (kbd "C-x 2") 'vsplit-last-buffer)
(global-set-key (kbd "C-x 3") 'hsplit-last-buffer)

(defun duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated."
  (interactive "p")
  (save-excursion
    (if (region-active-p)
        (duplicate-region arg)
      (duplicate-current-line arg))))

(defun duplicate-region (num &optional start end)
  "Duplicates the region bounded by START and END NUM times.
If no START and END is provided, the current region-beginning
region-end is used."
  (interactive "p")
  (let* ((start (or start (region-beginning)))
         (end (or end (region-end)))
         (region (buffer-substring start end)))
    (goto-char start)
    (dotimes (i num)
      (insert region))))

(defun duplicate-current-line (num)
  "Duplicate the current line NUM times."
  (interactive "p")
  (when (eq (point-at-eol) (point-max))
    (goto-char (point-max))
    (newline)
    (forward-char -1))
  (duplicate-region num (point-at-bol) (1+ (point-at-eol))))

(global-set-key (kbd "C-c 2") 'duplicate-current-line-or-region)

(defun smarter-move-beginning-of-line (arg)
  "Move depending on ARG to beginning of visible line or not.
  From https://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/."
  (interactive "^p")
  (setq arg (or arg 1))
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))
  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(global-set-key [remap move-beginning-of-line] 'smarter-move-beginning-of-line)

(eval-after-load "enriched"
  '(defun enriched-decode-display-prop (start end &optional param)
     (list start end)))

(use-package! async
  :ensure t
  :init
  (dired-async-mode 1)
  (async-bytecomp-package-mode 1)
  :custom (async-bytecomp-allowed-packages '(all)))

(use-package! emacs
  :ensure nil
  :config
  (setq compilation-always-kill t
        compilation-ask-about-save nil
        compilation-context-lines 10
        compilation-window-height 100
        compilation-scroll-output 'first-error))

(defvar file-name-handler-alist-old file-name-handler-alist)

(setq-default gc-cons-threshold 402653184
              file-name-handler-alist nil
              gc-cons-percentage 0.6
              auto-window-vscroll nil
              message-log-max 16384)

(add-hook 'after-init-hook
          `(lambda ()
             (setq file-name-handler-alist file-name-handler-alist-old
                   gc-cons-threshold 800000
                   gc-cons-percentage 0.1)
             (garbage-collect)) t)
(setq inhibit-compacting-font-caches t)

(use-package! gcmh
  :ensure t
  :disabled t
  :init
  (setq gcmh-verbose nil)
  :config
  (gcmh-mode 1))

(use-package! expand-region
  :init
  (map! "C-=" 'er/expand-region))

(use-package! multiple-cursors
  :config
  (add-to-list 'mc/unsupported-minor-modes 'lispy-mode)
  :init
  (setq mc/always-run-for-all t)
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))

(use-package! projectile
  :ensure t
  :init
  (map! "C-c C-p" 'projectile-command-map)
  :custom
  (projectile-mode +1))

(use-package! iedit
  :init
  (map! "C-;" 'company-complete)
  (map! "M-i" 'iedit-mode))

(use-package! dap-mode
  :ensure t
  :after lsp-mode
  :hook ((dap-stopped . (lambda (arg) (call-interactively #'dap-hydra)))
         (dap-mode . dap-ui-mode)
         (dap-mode . dap-tooltip-mode))
  :config
  (dap-mode t))

(use-package! lsp-mode
  :ensure t
  :demand t
  :commands lsp lsp-deferred
  :hook ((lsp-after-open . lsp-enable-imenu)
         (lsp-mode . lsp-lens-mode)
         (lsp-mode . lsp-enable-which-key-integration))
  :config
  (setq lsp-navigation 'both
        lsp-signature-render-all t
        lsp-enable-symbol-highlighting t
        lsp-enable-snippet t
        lsp-enable-folding t
        lsp-enable-indentation t
        lsp-enable-on-type-formatting t
        lsp-enable-file-watchers t
        lsp-enable-xref t
        lsp-flycheck-live-reporting t
        lsp-prefer-capf t
        lsp-semantic-highlighting t
        lsp-eldoc-enable-hover nil
        lsp-eldoc-enable-signature-help nil
        lsp-document-sync-method 'incremental
        lsp-signature-render-all t)

  :custom
  (lsp-auto-guess-root t)
  (lsp-document-sync-method 'incremental) ;; none, full, incremental, or nil
  (lsp-response-timeout 10)
  (lsp-prefer-flymake nil))

(use-package! lsp-ui
  :ensure t
  :demand t
  :after lsp-mode
  :hook ((lsp-after-open . lsp-ui-mode))
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  (set-lookup-handlers! 'lsp-ui-mode :async t
    :definition #'lsp-ui-peek-find-definitions
    :references #'lsp-ui-peek-find-references)
  (setq lsp-ui-doc-enable nil
        lsp-ui-doc-header nil
        lsp-ui-doc-border "green"
        lsp-ui-doc-max-height 30
        lsp-ui-doc-max-width 120
        lsp-ui-doc-include-signature t
        lsp-ui-doc-delay 0.0
        lsp-ui-doc-position 'at-point
        lsp-ui-doc-use-childframe t
        lsp-ui-flycheck-enable t
        lsp-ui-flycheck-list-position 'right
        lsp-ui-peek-enable t
        lsp-ui-peek-fontify 'on-demand
        lsp-ui-peek-peek-height 20
        lsp-ui-peek-list-width 50
        lsp-ui-sideline-enable t
        lsp-ui-sideline-update-mode 'line
        lsp-ui-sideline-show-symbol nil
        lsp-ui-sideline-show-diagnostics t
        lsp-ui-sideline-show-hover t
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-sideline-delay 0.0
        lsp-ui-imenu-enable t
        lsp-ui-imenu-kind-position 'top)
  (if (display-graphic-p)
      (progn
        (setq lsp-ui-sideline-code-actions-prefix " ")
        (when (require 'xwidget nil 'noerror)
          (setq lsp-ui-doc-use-webkit t)))))

  (use-package! magit
    :ensure t
    :after magit
    :hook (add-hook 'git-commit-setup-hook #'git-commit-turn-on-flyspell)
    :config
    (setq magit-diff-refine-hunk t)
    (setq magit-commit-arguments '("--verbose"))
    (add-to-list 'magit-section-initial-visibility-alist '(unpulled . show))
    (add-to-list 'magit-section-initial-visibility-alist '(unpushed . show))
    (add-to-list 'magit-section-initial-visibility-alist '(untracked . show))
    (add-to-list 'magit-section-initial-visibility-alist '(unstaged . show))
    (add-to-list 'magit-section-initial-visibility-alist '(todos . show))
    (add-to-list 'magit-section-initial-visibility-alist '(recent . show))
    (magit-todos-mode))

  (use-package! imenu-list
    :config
    (setq imenu-list-auto-resize t)
    (setq imenu-list-focus-after-activation t)
    (setq imenu-list-after-jump-hook nil)
    (add-hook 'imenu-list-after-jump-hook #'recenter-top-bottom))

  (use-package! company
    :ensure t
    :after company
    :init (global-company-mode 1)
    :config
    (company-tng-configure-default)
    (setq company-idle-delay 0.0
          company-minimum-prefix-length 1
          company-transformers nil
          company-show-numbers nil)

    (setq company-frontends
          '(company-tng-frontend company-pseudo-tooltip-frontend company-echo-metadata-frontend))
    (set-company-backend! '(prog-mode)
      '(:separate company-lsp company-dabbrev company-files company-keywords company-yasnippet))
    (setq +lsp-company-backend '(company-lsp :with company-dabbrev :separate company-files company-keywords company-yasnippet)))

  (use-package! company-flx
    :ensure t
    :after company
    :init (company-flx-mode t))

  (use-package company-quickhelp
    :after company
    :ensure t
    ;; :init (company-quickhelp-mode t)
    :hook (prog-mode . (lambda ()
                         (when (window-system)
                           (company-quickhelp-local-mode))))
    :config
    (setq company-quickhelp-delay 0.1
          company-quickhelp-max-lines nil))

  (use-package! company-lsp
    :ensure t
    :after company
    :commands company-lsp
    :hook (lsp-after-open . (lambda()
                              (add-to-list (make-local-variable 'company-backends)
                                           'company-lsp)))
    :preface
    (defun push-company-lsp-backends ()
      "Push company-lsp to the backends."
      (general-pushnew
       '(company-lsp
         company-dabbrev
         company-files
         company-keywords
         company-yasnippet)
       company-backends))
    :init
    (setq company-lsp-async               t
          company-lsp-enable-recompletion t
          company-lsp-enable-snippet      t
          company-lsp-cache-candidates    'auto)
    (push-company-lsp-backends))

  (use-package! treemacs
    :ensure t
    :config
    (treemacs-follow-mode 1))

  (use-package highlight-symbol
    :delight highlight-symbol-mode
    :hook
    ((highlight-symbol-mode . highlight-symbol-nav-mode)
     (prog-mode . highlight-symbol-mode))
    :custom
    (highlight-symbol-highlight-single-occurrence nil)
    (highlight-symbol-idle-delay 0.25)
    (highlight-symbol-on-navigation-p t))

  (use-package! highlight-blocks
    :commands (highlight-blocks-mode highlight-blocks-now)
    :config
    (custom-theme-set-faces! nil
      `(highlight-blocks-depth-1-face :background ,(doom-color 'base1))
      `(highlight-blocks-depth-2-face :background ,(doom-lighten 'base1 0.03))
      `(highlight-blocks-depth-3-face :background ,(doom-lighten 'base1 0.06))
      `(highlight-blocks-depth-4-face :background ,(doom-lighten 'base1 0.09))
      `(highlight-blocks-depth-5-face :background ,(doom-lighten 'base1 0.12))
      `(highlight-blocks-depth-6-face :background ,(doom-lighten 'base1 0.15))
      `(highlight-blocks-depth-7-face :background ,(doom-lighten 'base1 0.17))
      `(highlight-blocks-depth-8-face :background ,(doom-lighten 'base1 0.2))
      `(highlight-blocks-depth-9-face :background ,(doom-lighten 'base1 0.23))))

                                        ;(use-package! highlight-escape-sequences
                                        ;  :defer 2
                                        ;  :commands highlight-escape-sequences-mode
                                        ;  :config
                                        ;  (hes-mode t))

  (use-package! undo-tree
    :init
    (setq undo-tree-auto-save-history t
          undo-tree-visualizer-diff t
          undo-tree-visualizer-timestamps t)
    :config
    (progn
      (defun my/undo-tree-restore-default ()
        (setq undo-tree-visualizer-diff t))
      (advice-add 'undo-tree-visualizer-quit :after #'my/undo-tree-restore-default))
    (global-undo-tree-mode 1)
    ;; make ctrl-z undo
    (global-set-key (kbd "C-z") 'undo)
    ;; make ctrl-Z redo
    (defalias 'redo 'undo-tree-redo)
    (global-set-key (kbd "C-S-z") 'redo))

  (use-package! deadgrep
    :if (executable-find "rg")
    :init
    (map! "M-s" #'deadgrep))

  (use-package! restclient
    :ensure t
    :config
    (add-to-list 'auto-mode-alist '("\\.restclient\\'" . restclient-mode)))

  (use-package! company-restclient
    :ensure t
    :after company
    :config
    (add-to-list 'company-backends 'company-restclient))

  (use-package! dimmer
    :hook (after-init . dimmer-mode)
    :init
    (setq dimmer-fraction 0.50)
    :config
    (dimmer-configure-magit)
    (dimmer-configure-posframe)
    (dimmer-mode t))

  (use-package ace-window
    :ensure t
    :config
    (global-set-key (kbd "<M-RET>") 'ace-window))

  (use-package! focus
    :hook (prog-mode . focus-mode)
    :disabled t
    :config
    (add-to-list 'focus-mode-to-thing '((prog-mode . defun) (text-mode . sentence)))
    (add-to-list 'focus-mode-to-thing '(lsp-mode . lsp-folding-range))
    (focus-mode t))

  (load-library "find-lisp")
  (after! org
    (remove-hook 'org-mode-hook #'org-superstar-mode)
    (add-to-list 'org-modules 'org-habit)
    (add-to-list 'org-modules 'org-id)
    (setq org-use-property-inheritance t ; We like to inhert properties from their parents
          org-catch-invisible-edits 'smart) ; Catch invisible edits
    (setq org-hide-leading-stars nil
          org-startup-indented nil
          org-adapt-indentation nil)
    (setq org-todo-keywords
          '((sequence
             "TODO(t)"
             "INPROGRESS(i@/!)"
             "|"
             "DONE(d!)")
            (sequence
             "[ ](T)"
             "[-](I)"
             "|"
             "[X](D)")))
    (setq org-capture-templates
          '(("t" "TODO" entry (file "tasks.org")
             "* TODO [#%^{priority}] %^{taskname}%? %^{CATEGORY}p
:PROPERTIES:
:CREATED: %U
:END:
" :empty-lines 1)

            ("s" "Scheduled TODO" entry (file "tasks.org")
             "* TODO %^{taskname}%? %^{CATEGORY}p
SCHEDULED: %^t
:PROPERTIES:
:CREATED: %U
:END:
" :empty-lines 1)

            ("d" "Deadline TODO" entry (file "tasks.org")
             "* TODO %^{taskname}%? %^{CATEGORY}p
DEADLINE: %^t
:PROPERTIES:
:CREATED: %U
:END:
" :empty-lines 1)

            ("p" "Priority TODO" entry (file "tasks.org")
             "* TODO [#A] %^{taskname}%? %^{CATEGORY}p
DEADLINE: %^t
:PROPERTIES:
:CREATED: %U
:END:
" :empty-lines 1)

            ("n" "Note" entry (file "notes.org")
             "* NOTE %?\n%U" :empty-lines 1))))

  (after! org-agenda
    (setq org-agenda-span 'week)
    (setq org-agenda-start-on-weekday 1)
    (setq org-agenda-block-separator "")
    (setq org-agenda-start-with-log-mode '(clock))
    (setq org-agenda-files
          (find-lisp-find-files "~/org/" "\.org$"))
    (setq org-agenda-files '("~/org/tasks.org"))
    (setq org-agenda-diary-file "~/.org/diary.org"
          org-agenda-dim-blocked-tasks t
          org-agenda-use-time-grid t
          org-agenda-hide-tags-regexp ":\\w+:"
          org-agenda-compact-blocks t
          org-agenda-block-separator nil
          org-agenda-skip-scheduled-if-done t
          org-agenda-skip-deadline-if-done t
          org-enforce-todo-checkbox-dependencies nil
          org-habit-show-habits t)
    (setq org-agenda-custom-commands
          '((" " "Agenda"
             ((agenda ""
                      ((org-agenda-span 'day)
                       (org-agenda-start-day nil)
                       (org-deadline-warning-days 365)))
              (todo "TODO"
                    ((org-agenda-overriding-header "Tasks")
                     (org-agenda-files '("~/org/tasks.org")))))))))
  (after! evil-org
    (remove-hook 'org-tab-first-hook #'+org-cycle-only-current-subtree-h))
