;;; config.el --- Emacs configuration
;;; Commentary:
;; Main Emacs config, for bootstrap, see init.el

;;; -*- lexical-binding: t -*-

;;; Code:
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(blink-cursor-mode -1)
(global-display-line-numbers-mode 1)
(global-subword-mode t)
(line-number-mode +1)
(column-number-mode t)
(size-indication-mode t)
(show-paren-mode t)

(add-to-list 'default-frame-alist
             '(vertical-scroll-bars . nil))

(defconst *sys/gui*
  (display-graphic-p)
  "Are we running on a GUI Emacs?")

(defmacro after! (package &rest body)
  "Evaluate BODY after PACKAGE."
  (declare (indent defun) (debug t))
  (if (symbolp package)
      (list (if (or (not (bound-and-true-p byte-compile-current-file))
                    (require package nil 'noerror))
                #'progn
              #'with-no-warnings)
            (let ((body (macroexp-progn body)))
              `(if (featurep ',package)
                   ,body
                 (eval-after-load ',package ',body))))
    (let ((p (car package)))
      (cond ((not (keywordp p))
             `(after! (:and ,@package) ,@body))
            ((memq p '(:or :any))
             (macroexp-progn
              (cl-loop for next in (cdr package)
                       collect `(after! ,next ,@body))))
            ((memq p '(:and :all))
             (dolist (next (cdr package))
               (setq body `((after! ,next ,@body))))
             (car body))))))

(defmacro plist-put! (plist &rest rest)
  "Set each PROP VALUE pair in REST to PLIST in-place."
  `(cl-loop for (prop value)
            on (list ,@rest) by #'cddr
            do ,(if (symbolp plist)
                    `(setq ,plist (plist-put ,plist prop value))
                  `(plist-put ,plist prop value))))

(defalias 'yes-or-no-p #'y-or-n-p)

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
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-x x") '+sidebar-toggle)

(use-package emacs
  :config
  (add-to-list 'default-frame-alist '(font . "JetBrains Mono 12"))
  (if *sys/gui*
      (progn
        (setq initial-frame-alist
              '(
                (tool-bar-lines . 0)
                (font . "JetBrains Mono 12")
                (width . 140) ; chars
                (height . 30) ; lines
                (left . 0)
                (top . 0)))
        (setq default-frame-alist
              '(
                (tool-bar-lines . 0)
                (font . "JetBrains Mono 12")
                (width . 140)
                (height . 30)
                (left . 0)
                (top . 0))))
    (progn
      (setq initial-frame-alist '( (tool-bar-lines . 0)))
      (setq default-frame-alist '( (tool-bar-lines . 0)))))
  (setq-default major-mode 'text-mode
                cursor-type 'box
                x-stretch-cursor t
                cursor-in-non-selected-windows nil
                compilation-always-kill t
                compilation-context-lines 10
                compilation-scroll-output 'first-error
                indent-tabs-mode nil
                fringes-outside-margins t
                left-fringe-width 8
                right-fringe-width 8
                indicate-buffer-boundaries 'left
                bidi-display-reordering nil)
  (setq compilation-ask-about-save nil
        compilation-window-height 100
        inhibit-startup-screen t
        inhibit-startup-message t
        inhibit-startup-echo-area-message t
        initial-scratch-message nil

        tab-width 4

        scroll-step 1
        scroll-margin 0
        scroll-conservatively 100000
        scroll-preserve-screen-position 1

        visible-bell 1

        frame-resize-pixelwise t
        show-trailing-whitespace t

        make-backup-files nil
        auto-save-default nil

        track-eol nil
        line-move-visual nil

        kill-whole-line t
        indent-tabs-mode nil
        truncate-lines t

        show-paren-style 'parenthesis

        load-prefer-newer t))

(use-package all-the-icons)

(use-package hydra)

(use-package solaire-mode
  :hook (after-init . solaire-global-mode)
  :hook (after-revert . turn-on-solaire-mode)
  :hook (minibuffer-setup . solaire-mode-in-minibuffer)
  :hook (change-major-mode . turn-on-solaire-mode)
  :hook (ediff-prepare-buffer . solaire-mode)
  :config
  (advice-add #'persp-load-state-from-file :after #'solaire-mode-restore-persp-mode-buffers)
  (setq solaire-mode-auto-swap-bg nil)
  (solaire-global-mode +1))

(use-package doom-themes
  :after solaire-mode
  :config
  (load-theme 'doom-one t))

(use-package doom-modeline
  :after doom-themes
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 1
        doom-modeline-buffer-file-name-style 'auto
        doom-modeline-icon nil
        doom-modeline-buffer-encoding t
        doom-modeline-indent-info t
        doom-modeline-checker-simple-format nil)
  (set-face-attribute 'mode-line nil :family "JetBrains Mono" :height 100)
  (set-face-attribute 'mode-line-inactive nil :family "JetBrains Mono" :height 100))

(use-package async
  :init
  (dired-async-mode 1)
  (async-bytecomp-package-mode 1)
  :custom (async-bytecomp-allowed-packages '(all)))

(use-package delight)

(use-package gcmh
  :delight
  :init
  (setq gcmh-verbose nil)
  :config
  (gcmh-mode 1))

(use-package shackle
  :hook (after-init . shackle-mode)
  :custom
  (shackle-default-size 0.5)
  (shackle-default-alignment 'below)
  (shackle-rules '((magit-status-mode    :select t :inhibit-window-quit t :same t)
                   (magit-log-mode       :select t :inhibit-window-quit t :same t)
                   ("*quickrun*"         :select t :inhibit-window-quit t :same t)
                   (profiler-report-mode :select t)
                   (apropos-mode         :select t :align t :size 0.3)
                   (help-mode            :select t :align t :size 0.4)
                   (comint-mode          :select t :align t :size 0.4)
                   (grep-mode            :select t :align t)
                   (rg-mode              :select t :align t)
                   ("*bm-bookmarks*"           :select t   :align t)
                   ("*Flycheck errors*"        :select t   :align t :size 10)
                   ("*Backtrace*"              :select t   :align t :size 15)
                   ("*Shell Command Output*"   :select nil :align t :size 0.4)
                   ("*Org-Babel Error Output*" :select nil :align t :size 0.3)
                   ("*Async Shell Command*"    :ignore t)
                   ("*package update results*" :select nil :align t :size 10)
                   ("*Process List*"           :select t   :align t :size 0.3)
                   ("*Help*"                   :select t   :align t :size 0.3)
                   ("*Occur*"                  :select t   :align right)
                   ("\\*ivy-occur .*\\*"       :regexp t :select t :align right))))

(use-package which-key
  :delight
  :disabled t
  :config
  (which-key-mode +1))

(use-package which-func
  :hook 'after-init-hook)

(use-package hungry-delete
  :defer 0.7
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
         ("C->" . 'mc/mark-next-like-this)
         ("C-<" . 'mc/mark-previous-like-this)
         ("C-c C-<" . 'mc/mark-all-like-this)))

(use-package drag-stuff
  :config
  (drag-stuff-define-keys)
  (drag-stuff-global-mode 1))

(use-package browse-kill-ring
  :config
  (browse-kill-ring-default-keybindings))

(use-package delsel
  :hook (after-init . delete-selection-mode))

(use-package text-mode
  :straight nil
  :custom
  (adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*")
  (adaptive-fill-first-line-regexp "^* *$")
  (sentence-end "\\([。、！？]\\|……\\|[,.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
  (sentence-end-double-space nil))

(use-package markdown-mode
  :init
  (advice-add #'markdown--command-map-prompt :override #'ignore)
  (advice-add #'markdown--style-map-prompt   :override #'ignore)
  :mode ("README\\(?:\\.md\\)?\\'" . gfm-mode)
  :hook (markdown-mode . auto-fill-mode)
  :custom
  (markdown-header-scaling t)
  (markdown-enable-wiki-links t)
  (markdown-italic-underscore t)
  (markdown-asymmetric-header t)
  (markdown-gfm-uppercase-checkbox t)
  (markdown-fontify-code-blocks-natively t))

(use-package markdown-toc
  :after markdown-mode)

(use-package autorevert
  :ensure nil
  :hook (after-init . global-auto-revert-mode)
  :custom
  (auto-revert-interval 3)
  (auto-revert-avoid-polling t)
  (auto-revert-verbose nil)
  (auto-revert-remote-files t)
  (auto-revert-check-vc-info t)
  (global-auto-revert-non-file-buffers t))

(use-package hl-line
  :hook (after-init . global-hl-line-mode))

(use-package hl-block-mode
  :config
  (setq hl-block-delay 0.3))

(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character))

(use-package ediff
  :hook (ediff-quit . winner-undo)
  :custom
  (ediff-diff-options "-w")
  (ediff-highlight-all-diffs t)
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-merge-split-window-function 'split-window-horizontally))


(use-package mwim
  :bind (("C-a" . 'mwim-beginning-of-code-or-line)
         ("C-e" . 'mwim-end-of-code-or-line)
         ("<home>" . 'mwim-beginning-of-code-or-line)
         ("<end>" . 'mwim-end-of-code-or-line)))

(use-package projectile
  :delight
  ;; :bind ("C-c C-p" . 'projectile-command-map)
  :hook (after-init . projectile-global-mode)
  :config
  (setq projectile-project-search-path '("~/code/")
        projectile-auto-discover t
        projectile-enable-caching t
        projectile-indexing-method 'alien
        projectile-globally-ignored-file-suffixes '("#" "~" ".swp" ".o" ".so" ".exe" ".dll" ".elc" ".pyc" ".jar")
        projectile-globally-ignored-directories '(".git" "node_modules" "__pycache__" ".vs")
        projectile-globally-ignored-files '("TAGS" "tags" ".DS_Store")
        projectile-completion-system 'ivy))

(use-package counsel-projectile
  :after (:all counsel projectile)
  :bind ("C-x C-p" . counsel-projectile)
  :bind ("C-x f" . counsel-projectile-find-file)
  :bind ("C-x s". counsel-projectile-rg))

(use-package dired
  :straight nil
  :init
  (let ((gls (executable-find "gls")))
    (when gls (setq insert-directory-program gls)))
  (setq dired-recursive-deletes 'top)
  :bind (:map dired-mode-map
              ([mouse-2] . dired-find-file             )
              ("C-c C-p" . wdired-change-to-wdired-mode)))

(use-package diredfl
  :after dired
  :hook ((after-init . diredfl-global-mode)))

(use-package uniquify
  :straight nil
  :init  ;; nicer naming of buffers for files with identical names
  (setq uniquify-buffer-name-style   'reverse
        uniquify-separator           " • "
        uniquify-after-kill-buffer-p t
        uniquify-ignore-buffers-re   "^\\*"))

(use-package diff-hl
  :after dired
  :hook ((dired-mode . diff-hl-dired-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh)))

(use-package dired-hacks-utils)

(use-package dired-filter
  :after dired
  :bind (:map dired-mode-map
              ("/" . dired-filter-map))
  :hook ((dired-mode . dired-filter-mode)
         (dired-mode . dired-filter-group-mode))
  :init (setq dired-filter-revert 'never
              dired-filter-group-saved-groups
              '(("default"
                 ("Git"
                  (directory . ".git")
                  (file . ".gitignore"))
                 ("Directory"
                  (directory))
                 ("PDF"
                  (extension . "pdf"))
                 ("LaTeX"
                  (extension "tex" "bib"))
                 ("Source"
                  (extension "c" "cpp" "hs" "rb" "py" "r" "cs" "el" "lisp" "html" "js" "css"))
                 ("Doc"
                  (extension "md" "rst" "txt"))
                 ("Org"
                  (extension . "org"))
                 ("Archives"
                  (extension "zip" "rar" "gz" "bz2" "tar"))
                 ("Images"
                  (extension "jpg" "JPG" "webp" "png" "PNG" "jpeg" "JPEG" "bmp" "BMP" "TIFF" "tiff" "gif" "GIF"))))))

(use-package dired-rainbow
  :after dired
  :config
  (dired-rainbow-define-chmod directory "#6cb2eb" "d.*")
  (dired-rainbow-define html        "#eb5286" ("css" "less" "sass" "scss" "htm" "html" "jhtm" "mht" "eml" "mustache" "xhtml"))
  (dired-rainbow-define xml         "#f2d024" ("xml" "xsd" "xsl" "xslt" "wsdl" "bib" "json" "msg" "pgn" "rss" "yaml" "yml" "rdata"))
  (dired-rainbow-define document    "#9561e2" ("docm" "doc" "docx" "odb" "odt" "pdb" "pdf" "ps" "rtf" "djvu" "epub" "odp" "ppt" "pptx"))
  (dired-rainbow-define markdown    "#ffed4a" ("org" "etx" "info" "markdown" "md" "mkd" "nfo" "pod" "rst" "tex" "textfile" "txt"))
  (dired-rainbow-define database    "#6574cd" ("xlsx" "xls" "csv" "accdb" "db" "mdb" "sqlite" "nc"))
  (dired-rainbow-define media       "#de751f" ("mp3" "mp4" "MP3" "MP4" "avi" "mpeg" "mpg" "flv" "ogg" "mov" "mid" "midi" "wav" "aiff" "flac"))
  (dired-rainbow-define image       "#f66d9b" ("tiff" "tif" "cdr" "gif" "ico" "jpeg" "jpg" "png" "psd" "eps" "svg"))
  (dired-rainbow-define log         "#c17d11" ("log"))
  (dired-rainbow-define shell       "#f6993f" ("awk" "bash" "bat" "sed" "sh" "zsh" "vim"))
  (dired-rainbow-define interpreted "#38c172" ("fsx" "py" "ipynb" "rb" "pl" "t" "msql" "mysql" "pgsql" "sql" "r" "clj" "cljs" "scala" "js"))
  (dired-rainbow-define compiled    "#4dc0b5" ("asm" "cl" "lisp" "el" "c" "h" "c++" "h++" "hpp" "hxx" "m" "cc" "cs" "cp" "cpp" "go" "fs" "fsy" "fsi" "f" "for" "ftn" "f90" "f95" "f03" "f08" "s" "rs" "hi" "hs" "pyc" ".java"))
  (dired-rainbow-define executable  "#8cc4ff" ("exe" "msi"))
  (dired-rainbow-define compressed  "#51d88a" ("7z" "zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
  (dired-rainbow-define packaged    "#faad63" ("deb" "rpm" "apk" "jad" "jar" "cab" "pak" "pk3" "vdf" "vpk" "bsp"))
  (dired-rainbow-define encrypted   "#ffed4a" ("gpg" "pgp" "asc" "bfe" "enc" "signature" "sig" "p12" "pem"))
  (dired-rainbow-define fonts       "#6cb2eb" ("afm" "fon" "fnt" "pfb" "pfm" "ttf" "otf"))
  (dired-rainbow-define partition   "#e3342f" ("dmg" "iso" "bin" "nrg" "qcow" "toast" "vcd" "vmdk" "bak"))
  (dired-rainbow-define vc          "#0074d9" ("git" "gitignore" "gitattributes" "gitmodules"))
  (dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*"))

(use-package dired-subtree)

(use-package dired-ranger)

(use-package iedit
  :bind ("M-i" . 'iedit-mode))

(use-package dired-narrow
  :after dired
  :bind (:map dired-narrow-map
              ("<down>"  . dired-narrow-next-file)
              ("<up>"    . dired-narrow-previous-file)
              ("<right>" . dired-narrow-enter-directory)))

(use-package dired-collapse
  :hook 'dired-mode-hook)

(use-package ranger
  :preface
  (setq ranger-cleanup-on-disable t
        ranger-show-hidden t
        ranger-modify-header t
        ranger-preview-file t))

(use-package vlf
  :init
  (defun ffap-vlf ()
    "Find file at point with VLF."
    (interactive)
    (let ((file (ffap-file-at-point)))
      (unless (file-exists-p file)
        (error "File does not exist: %s" file))
      ((vector )lf file))))

(use-package dumb-jump
  :commands dumb-jump-result-follow
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g b" . dumb-jump-back)
         ("M-g i" . dumb-jump-go-prompt)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config
  (setq dumb-jump-aggressive nil
        dumb-jump-selector 'ivy)
  (add-to-list 'xref-backend-functions 'dumb-jump-xref-activate t)
  (add-hook 'dumb-jump-after-jump-hook #'better-jumper-set-jump))

(use-package autorevert
  :straight nil
  :delight autorevert-mode)

(use-package eldoc
  :delight eldoc-mode)

(use-package magit
  :commands (magit-status magit-blame magit-mode)
  :bind (("C-x g" . magit-status)
         ("C-c C-g l" . magit-file-log)
         ("C-c C-g c" . magit-commit)
         ("C-c C-g f" . magit-grep))
  :config
  (setq magit-status-margin '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 18))
  (setq magit-diff-refine-hunk t)
  (setq magit-commit-arguments '("--verbose"))
  (setq magit-section-initial-visibility-alist
               '((unpulled . show)
                 (unpushed . show)
                 (untracked . show)
                 (unstaged . show)
                 (stashes . show)
                 (todos . show)
                 (recent . show)))
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
                 (set-fill-column 120)))))

(use-package magit-todos)

(use-package forge
  :after magit)

(use-package git-blamed)

(use-package git-timemachine)

(use-package git-messenger
  :init (setq git-messenger:show-detail t)
  :bind (:map vc-prefix-map
              ("p" . git-messenger:popup-message)))

(use-package git-gutter
  :delight
  :hook (after-init . global-git-gutter-mode)
  :init (setq git-gutter:visual-line t
              git-gutter:disabled-modes '(asm-mode image-mode)
              git-gutter:modified-sign "❚"
              git-gutter:added-sign "✚"
              git-gutter:deleted-sign "✖")

  :bind (("C-c v =" . git-gutter:popup-hunk)
         ("C-c p" . git-gutter:previous-hunk)
         ("C-c n" . git-gutter:next-hunk)))

(use-package gitattributes-mode)

(use-package gitconfig-mode)

(use-package gitignore-mode)

(use-package smerge-mode
  :delight
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
  :hook ((find-file . (lambda ()
                        (save-excursion
                          (goto-char (point-min))
                          (when (re-search-forward "^<<<<<<< " nil t)
                            (smerge-mode 1)))))
         (magit-diff-visit-file . (lambda ()
                                    (when smerge-mode
                                      (smerge-hydra/body))))))

(use-package transient
  :config
  (setq transient-default-level 5))

(use-package gist)

(use-package imenu-list
  :config
  (setq imenu-list-auto-resize t)
  (setq imenu-list-focus-after-activation t)
  (setq imenu-list-after-jump-hook nil)
  (add-hook 'imenu-list-after-jump-hook #'recenter-top-bottom))

(use-package highlight-symbol
  :delight
  :delight highlight-symbol-mode
  :hook (highlight-symbol-mode . highlight-symbol-nav-mode)
  :hook (prog-mode . highlight-symbol-mode)
  :custom
  (highlight-symbol-highlight-single-occurrence nil)
  (highlight-symbol-idle-delay 0)
  (highlight-symbol-on-navigation-p t))

(use-package undo-tree
  :delight
  :init
  (setq undo-tree-auto-save-history t
        undo-tree-visualizer-diff t
        undo-tree-visualizer-timestamps t)
  (global-undo-tree-mode t)
  :bind (("C-x u" . undo-tree-visualize)
         ("C-z" . 'undo)
         ("C-S-z" .'undo-tree-redo))
  :config
  (progn
    (defun my/undo-tree-restore-default ()
      (setq undo-tree-visualizer-diff t))
    (advice-add 'undo-tree-visualizer-quit :after #'my/undo-tree-restore-default)))

(use-package deadgrep
  :if (executable-find "rg")
  :bind ("M-s" . 'deadgrep)
  :bind (:map deadgrep-mode-map
              ("M-e" . deadgrep-edit-mode)
              ("RET" . deadgrep-visit-result-other-window)
              ("o" . deadgrep-visit-result)))

(use-package wgrep
  :custom
  (wgrep-auto-save-buffer t)
  (wgrep-change-readonly-file t))

(use-package dimmer
  :hook (after-init . dimmer-mode)
  :init
  (setq dimmer-fraction 0.50
        dimmer-exclusion-regexp-list
       '(".*Minibuf.*"
         ".*which-key.*"
         ".*NeoTree.*"
         ".*Messages.*"
         ".*Async.*"
         ".*Warnings.*"
         ".*company.*"
         ".*Company.*"
         ".*LV.*"
         ".*Ilist.*"))
  :config
  (dimmer-configure-magit)
  (dimmer-configure-posframe)
  (dimmer-configure-company-box)
  (dimmer-mode t))

(use-package emacs ;; Only window configurations
  :straight nil
  :ensure nil
  :bind (("C-x C-2" . 'vsplit-last-buffer)
         ("C-x 2"   . 'vsplit-current-buffer)
         ("C-x C-3" . 'hsplit-last-buffer)
         ("C-x 3"   . 'hsplit-current-buffer)
         ("C-x |"   . 'toggle-window-split))
  :preface
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
)

(use-package ace-window
  :bind (("C-x o" . 'ace-window)
         ("M-o" . 'ace-window))
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

(use-package windmove
  :bind (("C-c h" . windmove-left)
         ("C-c j" . windmove-down)
         ("C-c k" . windmove-up)
         ("C-c l" . windmove-right)))

(use-package ace-jump-mode
  :bind
  ("C-c j" . ace-jump-word-mode)
  ("C-c l" .  ace-jump-line-mode))

(use-package anzu
  :delight
  :hook ((after-init . global-anzu-mode))
  :bind (([remap query-replace] . anzu-query-replace)
         ([remap query-replace-regexp] . anzu-query-replace-regexp)))

(use-package zoom
  :delight
  :config
  (custom-set-variables
   '(zoom-size '(0.5 . 0.5))
   '(zoom-ignored-major-modes '(dired-mode markdown-mode))
   '(zoom-ignored-buffer-names '("zoom.el" "init.el"))
   '(zoom-ignored-buffer-name-regexps '("^*calc"))
   '(zoom-ignore-predicates '((lambda () (> (count-lines (point-min) (point-max)) 20)))))
  (zoom-mode 1))

(use-package smex
  :init
  (smex-initialize))

(use-package flx)

(use-package fuz)

(use-package snails
  :if *sys/gui*
  :straight (:host github :repo "manateelazycat/snails" :branch "master")
  :bind ("C-c p" . snails)
  :config
  (setq snails-default-backends '(snails-backend-buffer	snails-backend-recentf snails-backend-imenu snails-backend-rg snails-backend-projectile)))

(use-package posframe)

(use-package popwin)

(use-package point-history
  :delight
  :straight (:host github :repo "blue0513/point-history" :branch "master")
  :config
  (point-history-mode t))

(use-package prescient
  :config
  (prescient-persist-mode 1))

(use-package ivy
  :bind ("C-x b" . ivy-switch-buffer)
  :after flx
  :config
  (setq ivy-re-builders-alist
        '((swiper . regexp-quote)
          (t . ivy--regex-fuzzy)))
  (setq ivy-use-selectable-prompt t
        ivy-auto-select-single-candidate t
        ivy-rich-parse-remote-buffer nil
        +ivy-buffer-icons nil
        ivy-use-virtual-buffers nil
        ivy-magic-slash-non-match-action 'ivy-magic-slash-non-match-cd-selected
        ivy-height 25
        ivy-rich-switch-buffer-name-max-length 50))

(use-package ivy-prescient
  :config
  (ivy-prescient-mode 1)
  (setq ivy-prescient-enable-sorting t
        ivy-prescient-enable-filtering t))

(use-package ivy-posframe
  :disabled t
  :delight
  :after ivy
  :config
  (setq ivy-posframe-parameters
        `((min-width . 100)
          (min-height . ,ivy-height)
          (left-fringe . 0)
          (right-fringe . 0)
          (internal-border-width . 10))
        ivy-display-functions-alist
        '((counsel-git-grep)
          (counsel-rg)
          (swiper)
          (counsel-irony . ivy-display-function-overlay)
          (ivy-completion-in-region . ivy-display-function-overlay)
          (t . ivy-posframe-display-at-frame-center)))
  (setq ivy-posframe-display-functions-alist
        '((complete-symbol . ivy-posframe-display-at-point)
          (t               . ivy-posframe-display-at-frame-center)))
  (ivy-posframe-mode 1))

(use-package ivy-rich
  :after ivy
  :config
  (setq ivy-rich--display-transformers-list
        '(ivy-switch-buffer
          (:columns
           ((ivy-rich-candidate (:width 30 :face bold))
            (ivy-rich-switch-buffer-size (:width 7 :face font-lock-doc-face))
            (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
            (ivy-rich-switch-buffer-major-mode (:width 18 :face doom-modeline-buffer-major-mode))
            (ivy-rich-switch-buffer-path (:width 50)))
           :predicate
           (lambda (cand) (get-buffer cand)))
          +ivy/switch-workspace-buffer
          (:columns
           ((ivy-rich-candidate (:width 30 :face bold))
            (ivy-rich-switch-buffer-size (:width 7 :face font-lock-doc-face))
            (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
            (ivy-rich-switch-buffer-major-mode (:width 18 :face doom-modeline-buffer-major-mode))
            (ivy-rich-switch-buffer-path (:width 50)))
           :predicate
           (lambda (cand) (get-buffer cand)))
          counsel-M-x
          (:columns
           ((counsel-M-x-transformer (:width 40))
            (ivy-rich-counsel-function-docstring (:face font-lock-doc-face :width 80))))
          counsel-describe-function
          (:columns
           ((counsel-describe-function-transformer (:width 40))
            (ivy-rich-counsel-function-docstring (:face font-lock-doc-face :width 80))))
          counsel-describe-variable
          (:columns
           ((counsel-describe-variable-transformer (:width 40))
            (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face :width 80))))
          counsel-recentf
          (:columns
           ((ivy-rich-candidate (:width 100))
            (ivy-rich-file-last-modified-time (:face font-lock-doc-face))))))
  (plist-put! ivy-rich-display-transformers-list
              'ivy-switch-buffer
              '(:columns
                ((ivy-switch-buffer-transformer (:width 60))
                 (ivy-rich-switch-buffer-size (:width 7))
                 (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
                 (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
                 (ivy-rich-switch-buffer-project (:width 15 :face success))
                 (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
                :predicate
                (lambda (cand) (get-buffer cand)))))

(use-package counsel
  :bind ("M-x" . 'counsel-M-x)
  :bind ("C-x C-f" . 'counsel-find-file)
  :bind ("M-g s" . counsel-imenu)
  :delight
  :config
  (setq counsel-yank-pop-height 20
        counsel-org-goto-face-style 'org
        counsel-org-headline-display-style 'title
        counsel-org-headline-display-tags t
        counsel-org-headline-display-todo t
        counsel-find-file-ignore-regexp "\\(?:^[#.]\\)\\|\\(?:[#~]$\\)\\|\\(?:^Icon?\\)"
        counsel-describe-function-function 'helpful-callable
        counsel-describe-variable-function 'helpful-variable
        counsel-rg-base-command "rg -zS --no-heading --line-number --max-columns 1000 %s ."
        counsel-grep-base-command counsel-rg-base-command))

(use-package swiper
  :after ivy
  :bind (("C-s" . my/swiper))
  :config
  (progn
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
    (bind-key "M-a" #'swiper-avy swiper-map))

  (defun eh-ivy-open-current-typed-path ()
    (interactive)
    (when ivy--directory
      (let* ((dir ivy--directory)
             (text-typed ivy-text)
             (path (concat dir text-typed)))
        (delete-minibuffer-contents)
        (ivy--done path))))
  (define-key ivy-minibuffer-map (kbd "<return>") 'ivy-alt-done)
  (define-key ivy-minibuffer-map (kbd "C-f") 'eh-ivy-open-current-typed-path))

(use-package pulse
  :preface
  (defun my/pulse-line (&rest _)
    "Pulse the current line."
    (pulse-momentary-highlight-one-line (point)))
  (defun my/recenter-and-pulse (&rest _)
    "Recenter and pulse the current line."
    (recenter)
    (my/pulse-line))
  :init
  :hook ((counsel-grep-post-action
          dumb-jump-after-jump
          bookmark-after-jump
          imenu-after-jump) . my/recenter-and-pulse)
  :custom-face
  (pulse-highlight-start-face ((t (:inherit highlight))))
  (pulse-highlight-face ((t (:inherit highlight)))))

(use-package electric-operator
  :hook (python-mode . electric-operator-mode))

(use-package smartparens
  :delight
  :config
  (progn
    (require 'smartparens-config)
    (smartparens-global-mode 1)
    (show-paren-mode t)))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

 (use-package whitespace
   :delight
   :hook (prog-mode . whitespace-mode)
   :init
   (setq-default whitespace-style
                 '(tabs trailing space-before-tab
                        newline space-after-tab
                        newline-mark)))

(use-package whitespace-cleanup-mode
  :delight
  :init
  (setq whitespace-cleanup-mode-only-if-initially-clean nil)
  :hook ((after-init . global-whitespace-cleanup-mode))
  :bind (("<remap> <just-one-space>" . cycle-spacing)))

(use-package fill-function-arguments
  :commands (fill-function-arguments-dwim)
  :custom
  (fill-function-arguments-indent-after-fill t)
  :config
  (add-hook 'prog-mode-hook (lambda () (local-set-key (kbd "M-q") #'fill-function-arguments-dwim)))
  (add-hook 'sgml-mode-hook (lambda ()
                              (setq-local fill-function-arguments-first-argument-same-line t)
                              (setq-local fill-function-arguments-argument-sep " ")
                              (local-set-key (kbd "M-q") #'fill-function-arguments-dwim)))
  (add-hook 'emacs-lisp-mode-hook (lambda ()
                                    (setq-local fill-function-arguments-first-argument-same-line t)
                                    (setq-local fill-function-arguments-second-argument-same-line t)
                                    (setq-local fill-function-arguments-last-argument-same-line t)
                                    (setq-local fill-function-arguments-argument-separator " ")
                                    (local-set-key (kbd "M-q") #'fill-function-arguments-dwim))))

;; UI
(use-package ibuffer
  :bind (("C-x C-b" . ibuffer))
  :preface
  (defun ibuffer-switch-to-normal ()
    "ibuffer swith to normal filter groups."
    (ibuffer-switch-to-saved-filter-groups "Normal"))
  :init
  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (file-size-human-readable (buffer-size)))
  (setq
   ibuffer-formats
   '((mark modified read-only vc-status-mini " "
           (name 22 22 :left :elide)
           " "
           (size-h 9 -1 :right)
           " "
           (mode 12 12 :left :elide)
           " "
           vc-relative-file)
     (mark modified read-only vc-status-mini " "
           (name 22 22 :left :elide)
           " "
           (size-h 9 -1 :right)
           " "
           (mode 14 14 :left :elide)
           " "
           (vc-status 12 12 :left)
           " "
           vc-relative-file)))
  (setq ibuffer-saved-filter-groups
         '(("Normal"
            ("Dired"      (mode . dired-mode))
            ("Emacs"     (or
                          (name . "^\\*dashboard\\*$" )
                          (name . "^\\*scratch\\*$"   )
                          (name . "^\\*Messages\\*$"  )
                          (name . "^\\*Backtrace\\*$" )))
            ("Term"       (mode . vterm-mode))
            ("Text"      (or
                          (mode . org-mode)
                          (mode . markdown)
                          (mode . rst-mode)
                          (mode . text-mode)))
            ("TeX"        (mode . tex-mode))
            ("Languages" (or
                          (mode . emacs-lisp-mode)
                          (mode . fsharp-mode)
                          (mode . haskell-mode)
                          (mode . prog-mode)))
            ("Magit"      (name . "^magit"))
            ("Help"      (or
                          (name . "^\\*Help\\*$")
                          (name . "^\\*Apropos\\*$")
                          (name . "^\\*info\\*$")
                          (name . "^\\*helpful")))
            ("Custom"    (or
                          (mode . custom-mode)
                          (name . "^\\*Customize")))))
         ibuffer-show-empty-filter-groups nil
         ibuffer-default-sorting-mode     'filename/process)
  :hook ((ibuffer-mode . ibuffer-switch-to-normal)))

(use-package ibuffer-vc)

(use-package symbol-overlay
  :bind (("M-i"  . symbol-overlay-put)
         ("M-n"  . symbol-overlay-switch-forward)
         ("M-p"  . symbol-overlay-switch-backward)
         ("<f8>" . symbol-overlay-remove-all)
         ("<f7>" . symbol-overlay-mode)))

(use-package hl-todo
  :hook (after-init . global-hl-todo-mode)
  :bind (:map hl-todo-mode-map
         ("C-c t p" . hl-todo-previous)
         ("C-c t n" . hl-todo-next)
         ("C-c t o" . hl-todo-occur))
  :config
  (dolist (keyword '("BUG" "ISSUE"))
    (cl-pushnew `(,keyword . ,(face-foreground 'error)) hl-todo-keyword-faces)))

(use-package treemacs
  :init
  (after! winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
        treemacs-deferred-git-apply-delay      0.5
        treemacs-display-in-side-window        t
        treemacs-eldoc-display                 t
        treemacs-file-event-delay              5000
        treemacs-file-follow-delay             0.2
        treemacs-follow-after-init             t
        treemacs-git-command-pipe              ""
        treemacs-goto-tag-strategy             'refetch-index
        treemacs-indentation                   2
        treemacs-indentation-string            " "
        treemacs-is-never-other-window         nil
        treemacs-max-git-entries               5000
        treemacs-missing-project-action        'ask
        treemacs-no-png-images                 nil
        treemacs-no-delete-other-windows       t
        treemacs-project-follow-cleanup        nil
        treemacs-position                      'left
        treemacs-recenter-distance             0.1
        treemacs-recenter-after-file-follow    nil
        treemacs-recenter-after-tag-follow     nil
        treemacs-recenter-after-project-jump   'always
        treemacs-recenter-after-project-expand 'on-distance
        treemacs-show-cursor                   nil
        treemacs-show-hidden-files             t
        treemacs-silent-filewatch              nil
        treemacs-silent-refresh                nil
        treemacs-sorting                       'alphabetic-desc
        treemacs-space-between-root-nodes      t
        treemacs-tag-follow-cleanup            t
        treemacs-tag-follow-delay              1.5
        treemacs-width                         35)

  ;; The default width and height of the icons is 22 pixels. If you are
  ;; using a Hi-DPI display, uncomment this to double the icon size.
  ;; (treemacs-resize-icons 44)
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode t)
  (pcase (cons (not (null (executable-find "git")))
               (not (null treemacs-python-executable)))
    (`(t . t)
     (treemacs-git-mode 'deferred))
    (`(t . _)
     (treemacs-git-mode 'simple)))
  :bind (("M-0" . treemacs-select-window)
         ("C-x t 1" . treemacs-delete-other-windows)
         ("C-x t t" . treemacs)
         ("C-x t B" . treemacs-bookmark)
         ("C-x t C-t" . treemacs-find-file)
         ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :after treemacs projectile)

(use-package treemacs-icons-dired
  :after treemacs dired
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit)

;; Programming stuff:

(use-package yasnippet
  :disabled t
  :delight
  :after company
  :commands (yas-minor-mode)
  :hook ((prog-mode      . yas-minor-mode)
         (yas-minor-mode . (lambda ()
                             (add-to-list
                              'yas-snippet-dirs
                              (concat user-emacs-directory "snippets")))))
  :config
  (add-to-list 'company-backends #'company-yasnippet)
  (yas-reload-all))

(use-package yasnippet-snippets
  :disabled t
  :after yasnippet)

(use-package lsp-mode
  :hook (lsp-after-open . lsp-enable-imenu)
  :hook (lsp-after-open . lsp-lens-mode)
  :hook (lsp-after-open . lsp-headerline-breadcrumb-mode)
  ;; :hook (lsp-mode       . lsp-enable-which-key-integration)
  :commands (lsp lsp-deferred)
  :config
  (setq lsp-navigation 'both
        lsp-auto-guess-root t
        lsp-enable-symbol-highlighting t
        lsp-enable-snippet nil
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
        lsp-prefer-flymake nil
        lsp-response-timeout 10
        lsp-signature-auto-activate nil
        lsp-signature-render-all nil
        lsp-headerlin-breadcrumbs-mode 1
        lsp-headerline-breadcrumb-segments '(project file symbols))
  :custom
  (lsp-file-watch-threshold 2000)
  (read-process-output-max (* 1024 1024))
  (lsp-eldoc-hook nil))

(use-package lsp-ui
  :delight
  :after lsp-mode
  :commands lsp-ui-mode
  :hook (lsp-after-open . lsp-ui-mode)
  :hook (lsp-after-open . lsp-lens-mode)
  :hook (lsp-after-open . lsp-ui-sideline-mode)
  :custom-face
  (lsp-ui-doc-background ((t (:background nil))))
  (lsp-ui-doc-header ((t (:inherit (font-lock-string-face italic)))))
  :bind
  (("C-." . lsp-ui-sideline-apply-code-actions))
  (:map lsp-ui-mode-map
        ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
        ([remap xref-find-references] . lsp-ui-peek-find-references)
        ("C-c u" . lsp-ui-imenu)
        ("M-i" . lsp-ui-doc-focus-frame))
  (:map lsp-mode-map
        ("M-n" . forward-paragraph)
        ("M-p" . backward-paragraph))
  :config
  (setq lsp-ui-doc-enable nil
        lsp-ui-doc-header nil
        lsp-ui-doc-border "green"
        lsp-ui-doc-max-height 50
        lsp-ui-doc-max-width 150
        lsp-ui-doc-include-signature t
        lsp-ui-doc-delay 0.0
        lsp-ui-doc-position 'at-point
        lsp-ui-doc-use-childframe t
        lsp-ui-flycheck-enable t
        lsp-ui-flycheck-list-position 'right
        lsp-ui-peek-enable t
        lsp-ui-peek-fontify 'always
        lsp-ui-peek-peek-height 30
        lsp-ui-peek-list-width 60
        lsp-ui-sideline-enable t
        lsp-ui-sideline-update-mode 'line
        lsp-ui-sideline-show-symbol t
        lsp-ui-sideline-show-diagnostics t
        lsp-ui-sideline-show-hover t
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-sideline-delay 0.0
        lsp-ui-imenu-enable t
        lsp-ui-imenu-kind-position 'top)

  (if *sys/gui*
      (progn
        (setq lsp-ui-sideline-code-actions-prefix " ")
        (when (require 'xwidget nil 'noerror)
          (setq lsp-ui-doc-use-webkit t))))
  (defadvice lsp-ui-imenu (after hide-lsp-ui-imenu-mode-line activate)
    (setq mode-line-format nil)))

(use-package lsp-ivy
  :after (:all lsp-mode ivy)
  :commands lsp-ivy-workspace-symbol)

(use-package dap-mode
  :after lsp-mode
  :hook (dap-stopped . (lambda (arg) (call-interactively #'dap-hydra)))
  :hook (dap-mode . dap-ui-mode)
  :hook (dap-mode . dap-tooltip-mode)
  :commands (dap-debug dap-debug-edit-template)
  :bind (:map dap-mode-map
              (("<f12>" . dap-debug)
               ("<f8>" . dap-continue)
               ("<f9>" . dap-next)
               ("<M-f11>" . dap-step-in)
               ("C-M-<f11>" . dap-step-out)
               ("<f7>" . dap-breakpoint-toggle))))

(use-package eglot
  :commands eglot)

(use-package company
  :delight
  :hook (after-init . global-company-mode)
  :commands (company-mode global-company-mode company-complete
             company-complete-common company-manual-begin company-grab-line)
  :init
  (with-eval-after-load 'company
    (add-to-list 'company-transformers 'delete-consecutive-dups t))
  (setq company-tooltip-limit 20
        company-tooltip-idle-delay 0.3
        company-tooltip-flip-when-above t
        company-tooltip-align-annotations t
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case t
        company-dabbrev-other-buffers 'all
        company-minimum-prefix-length 1
        company-idle-delay 0.3
        company-require-match 'never)
  :config
  (global-company-mode +1)
  :custom
  (custom-set-faces
   '(company-preview
     ((t (:foreground "darkgray" :underline t))))
   '(company-preview-common
     ((t (:inherit company-preview))))
   '(company-tooltip
     ((t (:background "lightgray" :foreground "black"))))
   '(company-tooltip-selection
     ((t (:background "steelblue" :foreground "white"))))
   '(company-tooltip-common
     ((((type x)) (:inherit company-tooltip :weight bold))
      (t (:inherit company-tooltip))))
   '(company-tooltip-common-selection
     ((((type x)) (:inherit company-tooltip-selection :weight bold))
      (t (:inherit company-tooltip-selection))))))

(use-package company-lsp
  :after (:all company lsp)
  :commands company-lsp
  :config
  (push 'company-lsp company-backends)
  (setq company-transformers nil
        company-lsp-async t
        company-lsp-cache-candidates nil))

(use-package company-prescient
  :after (:all company prescient))

(use-package company-flx
  :after company)

(use-package company-dict
  :after company)

(use-package company-quickhelp
  :after company
  :bind (:map company-active-map ("C-c h" . company-quickhelp-manual-begin))
  :preface
  (setq pos-tip-use-relative-coordinates t
        company-quickhelp-delay 0.1)
  :config
  (company-quickhelp-mode))

(use-package company-box
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
  :hook (company-mode . company-box-mode)
  :custom
  (company-box-backends-colors nil)
  (company-box-show-single-candidate t)
  (company-box-max-candidates 50)
  (company-box-doc-delay 0.3)
  :config
  ;; Support `company-common'
  (defun my-company-box--make-line (candidate)
    (-let* (((candidate annotation len-c len-a backend) candidate)
            (color (company-box--get-color backend))
            ((c-color a-color i-color s-color) (company-box--resolve-colors color))
            (icon-string (and company-box--with-icons-p (company-box--add-icon candidate)))
            (candidate-string (concat (propertize (or company-common "") 'face 'company-tooltip-common)
                                      (substring (propertize candidate 'face 'company-box-candidate) (length company-common) nil)))
            (align-string (when annotation
                            (concat " " (and company-tooltip-align-annotations
                                             (propertize " " 'display `(space :align-to (- right-fringe ,(or len-a 0) 1)))))))
            (space company-box--space)
            (icon-p company-box-enable-icon)
            (annotation-string (and annotation (propertize annotation 'face 'company-box-annotation)))
            (line (concat (unless (or (and (= space 2) icon-p) (= space 0))
                            (propertize " " 'display `(space :width ,(if (or (= space 1) (not icon-p)) 1 0.75))))
                          (company-box--apply-color icon-string i-color)
                          (company-box--apply-color candidate-string c-color)
                          align-string
                          (company-box--apply-color annotation-string a-color)))
            (len (length line)))
      (add-text-properties 0 len (list 'company-box--len (+ len-c len-a)
                                       'company-box--color s-color)
                           line)
      line))
  (advice-add #'company-box--make-line :override #'my-company-box--make-line)

  ;; Prettify icons
  (defun my-company-box-icons--elisp (candidate)
    (when (derived-mode-p 'emacs-lisp-mode)
      (let ((sym (intern candidate)))
        (cond ((fboundp sym) 'Function)
              ((featurep sym) 'Module)
              ((facep sym) 'Color)
              ((boundp sym) 'Variable)
              ((symbolp sym) 'Text)
              (t . nil)))))
  (advice-add #'company-box-icons--elisp :override #'my-company-box-icons--elisp)

  (when (and *sys/gui*
             (require 'all-the-icons nil t))
    (declare-function all-the-icons-faicon 'all-the-icons)
    (declare-function all-the-icons-material 'all-the-icons)
    (declare-function all-the-icons-octicon 'all-the-icons)
    (setq company-box-icons-all-the-icons
          `((Unknown . ,(all-the-icons-material "find_in_page" :height 0.85 :v-adjust -0.2))
            (Text . ,(all-the-icons-faicon "text-width" :height 0.8 :v-adjust -0.05))
            (Method . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.05 :face 'all-the-icons-purple))
            (Function . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.05 :face 'all-the-icons-purple))
            (Constructor . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.05 :face 'all-the-icons-purple))
            (Field . ,(all-the-icons-octicon "tag" :height 0.8 :v-adjust 0 :face 'all-the-icons-lblue))
            (Variable . ,(all-the-icons-octicon "tag" :height 0.8 :v-adjust 0 :face 'all-the-icons-lblue))
            (Class . ,(all-the-icons-material "settings_input_component" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-orange))
            (Interface . ,(all-the-icons-material "share" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-lblue))
            (Module . ,(all-the-icons-material "view_module" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-lblue))
            (Property . ,(all-the-icons-faicon "wrench" :height 0.8 :v-adjust -0.05))
            (Unit . ,(all-the-icons-material "settings_system_daydream" :height 0.85 :v-adjust -0.2))
            (Value . ,(all-the-icons-material "format_align_right" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-lblue))
            (Enum . ,(all-the-icons-material "storage" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-orange))
            (Keyword . ,(all-the-icons-material "filter_center_focus" :height 0.85 :v-adjust -0.2))
            (Snippet . ,(all-the-icons-material "format_align_center" :height 0.85 :v-adjust -0.2))
            (Color . ,(all-the-icons-material "palette" :height 0.85 :v-adjust -0.2))
            (File . ,(all-the-icons-faicon "file-o" :height 0.85 :v-adjust -0.05))
            (Reference . ,(all-the-icons-material "collections_bookmark" :height 0.85 :v-adjust -0.2))
            (Folder . ,(all-the-icons-faicon "folder-open" :height 0.85 :v-adjust -0.05))
            (EnumMember . ,(all-the-icons-material "format_align_right" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-lblue))
            (Constant . ,(all-the-icons-faicon "square-o" :height 0.85 :v-adjust -0.05))
            (Struct . ,(all-the-icons-material "settings_input_component" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-orange))
            (Event . ,(all-the-icons-faicon "bolt" :height 0.8 :v-adjust -0.05 :face 'all-the-icons-orange))
            (Operator . ,(all-the-icons-material "control_point" :height 0.85 :v-adjust -0.2))
            (TypeParameter . ,(all-the-icons-faicon "arrows" :height 0.8 :v-adjust -0.05))
            (Template . ,(all-the-icons-material "format_align_center" :height 0.85 :v-adjust -0.2)))
          company-box-icons-alist 'company-box-icons-all-the-icons)))

(use-package company-posframe
  :delight
  :hook (company-mode . company-posframe-mode)
  :after company)

(use-package flycheck
  :preface
  (defun save-buffer-maybe-show-errors ()
    "Save buffer and show errors if any."
    (interactive)
    (save-buffer)
    (when (not flycheck-current-errors)
      (flycheck-list-errors)))
  :commands (flycheck-mode
             flycheck-next-error
             flycheck-previous-error
             flycheck-add-next-checker)
  ;; :bind (("C-x C-s" . save-buffer-maybe-show-errors))
  :hook (prog-mode . flycheck-mode)
  :init (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list
               flycheck-check-syntax-automatically '(save mode-enabled)
               flycheck-display-errors-delay 0.0
               flycheck-idle-change-delay 0.0)
  :bind (:map flycheck-error-list-mode-map
              ("C-n"    . flycheck-error-list-next-error)
              ("C-p"    . flycheck-error-list-previous-error)
              ("RET"    . flycheck-error-list-goto-error)
              ([return] . flycheck-error-list-goto-error))
  :config
  (defalias 'show-error-at-point-soon 'flycheck-show-error-at-point)
  (add-to-list 'flycheck-emacs-lisp-checkdoc-variables 'sentence-end-double-space))

(use-package flycheck-inline
  :after flycheck
  :hook (flycheck-mode . flycheck-inline-mode))

(use-package flycheck-package
  :after flycheck
  :init
  (after! elisp-mode
    (flycheck-package-setup)))

(use-package editorconfig
  :config (editorconfig-mode 1))

(use-package ggtags)

(use-package counsel-gtags)

(use-package dotnet)

(use-package csharp-mode
  :after (:all lsp-mode company flycheck dotnet omnisharp)
  :hook (csharp-mode . lsp)
  ;; :hook (csharp-mode . omnisharp-mode)
  :hook (csharp-mode . dotnet-mode)
  :hook (csharp-mode . company-mode)
  :hook (csharp-mode . flycheck-mode)
  :hook (csharp-mode . (lambda ()
                         (subword-mode)
                         (setq-local fill-function-arguments-first-argument-same-line t)
                         (setq-local fill-function-arguments-second-argument-same-line nil)
                         (setq-local fill-function-arguments-last-argument-same-line t)
                         (define-key csharp-mode-map [remap c-fill-paragraph] 'fill-function-arguments-dwim)))
  :config
  (setq lsp-csharp-server-path "~/omnisharp/run")
  (setq indent-tabs-mode nil
        c-syntactic-indentation t
        c-basic-offset 4
        truncate-lines t
        tab-width 4)
  (electric-pair-local-mode 1))

(use-package sharper
  :straight (:host github :repo "sebasmonia/sharper" :branch "master"))

(use-package omnisharp
  :disabled t
  :after (:all company flycheck)
  :hook (omnisharp-mode . company-mode)
  :hook (kill-buffer-hook #'+csharp-cleanup-omnisharp-server-h nil t)
  :preface
  (setq omnisharp-auto-complete-want-documentation nil
        omnisharp-eldoc-support t
        omnisharp-imenu-support t
        omnisharp-company-ignore-case t)
  :config
  (add-to-list 'company-backends #'company-omnisharp))

(use-package fsharp-mode
  ;;:straight (:host github :repo "vzarytovskii/emacs-fsharp-mode" :branch "master")
  :straight nil
  :load-path "~/code/elisp/emacs-fsharp-mode"
  :after (:all lsp-mode projectile)
  :commands fsharp-mode
  :hook (fsharp-mode . lsp)
  :hook (fsharp-mode . dotnet-mode)
  :config
  (setq indent-tabs-mode nil
        truncate-lines t
        tab-width 4)
  (setq fsharp-doc-idle-delay 0.0
        fsharp-ac-use-popup t
        fsharp-ac-intellisense-enabled nil
        fsharp-smart-indentation t
        fsharp-indent-offset 4
        inferior-fsharp-program "dotnet fsi"
        lsp-fsharp-server-runtime 'net-core
        lsp-fsharp-server-install-dir "~/code/fsharp/FsAutoComplete/bin/release_netcore/"
        lsp-fsharp-server-args '("--verbose")
        lsp-fsharp-keywords-autocomplete t
        lsp-fsharp-external-autocomplete t
        lsp-fsharp-linter t
        lsp-fsharp-union-case-stub-generation t
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
        lsp-fsharp-auto-workspace-init nil
        lsp-log-io t)
  (add-to-list 'company-transformers 'company-sort-prefer-same-case-prefix)
  (setq indent-region-function '(lambda (start end &optional indent-offset))))

(use-package nxml-mode
  :straight nil
  :magic "<\\?xml"
  :mode (("\\.\\(fs\\|cs\\|cc\\)proj$" . nxml-mode)
         ("\\.xml$" . nxml-mode)
         ("\\.props" . nxml-mode)))

(use-package json-mode
  :mode "\\.json$")

(use-package yaml-mode
  :mode "\\.yml$")

;; Spelling and stuff
(use-package bing-dict
  :bind (("C-c d" . bing-dict-brief))
  :init (setq bing-dict-show-thesaurus  'both
               bing-dict-vocabulary-save t
               bing-dict-cache-auto-save t))

(use-package ispell
  :init
  (setq ispell (executable-find "aspell"))

  (setq-default ispell-program-name ispell
                ispell-silently-savep t
                ispell-dictionary "english")
  (when (string-suffix-p "aspell" ispell)
    (setq-default ispell-extra-args '("--reverse"))))

(use-package flyspell
  :delight
  :if (executable-find "aspell")
  :hook
  ((org-mode yaml-mode markdown-mode git-commit-mode) . flyspell-mode)
  (prog-mode . flyspell-prog-mode)
  (before-save-hook . flyspell-buffer)
  (flyspell-mode . (lambda ()
                     (dolist (key '("C-;" "C-," "C-."))
                       (unbind-key key flyspell-mode-map))))
  :custom
  (flyspell-issue-message-flag nil)
  (ispell-program-name "aspell")
  (ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together"))
  :custom-face
  (flyspell-incorrect ((t (:underline (:color "#f1fa8c" :style wave)))))
  (flyspell-duplicate ((t (:underline (:color "#50fa7b" :style wave)))))
  :preface
  (defun message-off-advice (oldfun &rest args)
    "Quiet down messages in adviced OLDFUN."
    (let ((message-off (make-symbol "message-off")))
      (unwind-protect
          (progn
            (advice-add #'message :around #'ignore (list 'name message-off))
            (apply oldfun args))
        (advice-remove #'message message-off))))
  :config
  (advice-add #'ispell-init-process :around #'message-off-advice)
  (use-package flyspell-correct-ivy
    :bind ("C-M-:" . flyspell-correct-at-point)
    :config
    (when (eq system-type 'darwin)
      (progn
        (global-set-key (kbd "C-M-;") 'flyspell-correct-at-point)))
    (setq flyspell-correct-interface #'flyspell-correct-ivy)))

(use-package flyspell-popup
  :delight
  :after (flyspell))

(use-package synosaurus
  :delight
  :custom
  (synosaurus-backend 'synosaurus-backend-wordnet)
  (synosaurus-choose-method 'popup))

(provide 'config)
;;; config.el ends here
