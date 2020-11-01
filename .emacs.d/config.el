;;; config.el --- Emacs configuration
;;; Commentary:
;; Main Emacs config, for bootstrap, see init.el

;;; -*- lexical-binding: t -*-

;;; Code:

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

(defun +project/ivy-switch-buffer ()
  (interactive)
  (ivy-read "Switch to buffer: "
            (delete (buffer-name (current-buffer))
                    (when (+project/root)
                      (projectile-project-buffer-names)))
            :initial-input nil
            :action #'ivy--switch-buffer-action
            :caller '+project/ivy-switch-buffer))

(defun end-of-line-and-indented-new-line ()
  (interactive)
  (end-of-line)
  (newline-and-indent))

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

(defun copy-region-or-line (beg end)
  (interactive "r")
  (if mark-active
      (kill-ring-save beg end)
    (kill-ring-save (line-beginning-position) (line-end-position))))

(global-set-key (kbd "C-c 2") 'duplicate-current-line-or-region)
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "M-w") 'copy-region-or-line)
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "<S-return>") 'end-of-line-and-indented-new-line)
(global-set-key [delete] 'delete-forward-char)

(use-package async
  :init
  (dired-async-mode 1)
  (async-bytecomp-package-mode 1)
  :custom (async-bytecomp-allowed-packages '(all)))

(use-package gcmh
  :delight
  :hook (after-init . gcmh-mode)
  :init
  (setq gcmh-verbose nil)
  :config
  (if (boundp 'after-focus-change-function)
      (add-function :after after-focus-change-function
                    (lambda ()
                      (unless (frame-focus-state)
                        (gcmh-idle-garbage-collect))))
    (add-hook 'focus-out-hook 'gcmh-idle-garbage-collect))
  ;;(with-eval-after-load 'org
  ;;  (add-hook 'org-mode-hook (lambda () (setq-local gcmh-high-cons-threshold (* 2 gcmh-high-cons-threshold)))))
  (with-eval-after-load 'lsp-mode
    (add-hook 'lsp-mode-hook (lambda () (setq-local gcmh-high-cons-threshold (* 2 gcmh-high-cons-threshold))))))

(use-package emacs
  :bind ("C-k" . kill-whole-line)
  :bind ("C-k" . kill-whole-line)
  :config

  (setq posframe-gtk-resize-child-frames 'resize-mode)

  (setq locale-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  (set-language-environment "UTF-8")
  (set-default-coding-systems 'utf-8-unix)
  (set-terminal-coding-system 'utf-8-unix)
  (set-keyboard-coding-system 'utf-8-unix)
  (set-selection-coding-system 'utf-8-unix)

  (set-face-attribute 'default nil :family "JetBrains Mono" :height 95 :weight 'semi-bold)
  (set-face-attribute 'fixed-pitch nil :family "JetBrains Mono" :height 105 :weight 'semi-bold)
  (set-face-attribute 'variable-pitch nil :family "JetBrains Mono" :height 105 :weight 'normal)

  (add-to-list 'default-frame-alist '(font . "JetBrains Mono 9"))
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
                bidi-display-reordering nil
                display-line-numbers-width 3)

  (setq split-height-threshold nil)
  (setq split-width-threshold nil)
  (setq warning-minimum-level ':error)
  
  (setq compilation-ask-about-save nil
        compilation-window-height 100
        inhibit-default-init t
        inhibit-startup-screen t
        inhibit-startup-message t
        inhibit-startup-echo-area-message t
        initial-scratch-message nil

        tab-width 4

        scroll-step 1
        scroll-margin 0
        scroll-conservatively 101
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

(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-verions t
        auto-package-update-interval 5)  
  (auto-package-update-maybe))

(use-package delight)
(use-package all-the-icons)

(use-package hydra)

(use-package vterm)

(use-package dashboard
  :delight
  :config
  (setq dashboard-items '((recents  . 15)
                          (projects . 5)
                          (agenda . 5)))
  (dashboard-setup-startup-hook))

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-Iosvkem t)
  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

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

(use-package elisp-demos
  :init
  (advice-add 'describe-function-1 :after #'elisp-demos-advice-describe-function-1)
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

(use-package helpful
  :defines ivy-initial-inputs-alist
  :bind (("C-c C-d" . helpful-at-point)
         ("C-h f"   . helpful-callable) ;; replace built-in `describe-function'
         ("C-h k"   . helpful-key)
         ("C-h v"   . helpful-variable))
  :config
  (with-eval-after-load 'ivy
    (dolist (cmd '(helpful-callable
                   helpful-variable
                   helpful-function
                   helpful-macro
                   helpful-command))
      (cl-pushnew `(,cmd . "^") ivy-initial-inputs-alist)))

  (when (featurep 'elisp-demos)
    (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update)))

(use-package subword
  :straight nil
  :delight)

(use-package page-break-lines
  :delight)

(use-package recentf
  :config
  (setq recentf-auto-cleanup nil
        recentf-max-saved-items 200
        recentf-exclude '((expand-file-name package-user-dir)
                          ".cache"
                          ".cask"
                          ".elfeed"
                          "bookmarks"
                          "cache"
                          "ido.*"
                          "persp-confs"
                          "recentf"
                          "undo-tree-hist"
                          "url"
                          "COMMIT_EDITMSG\\'")))

(use-package which-key
  :delight
  :config
  (which-key-mode +1))

(use-package which-func
  :hook 'after-init-hook)

(use-package hungry-delete
  :delight
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
         ("C-S-d" . 'mc/mark-previous-like-this)
         ("C->" . 'mc/mark-next-like-this)
         ("C-<" . 'mc/mark-previous-like-this)
         ("C-c C-<" . 'mc/mark-all-like-this)))

(use-package drag-stuff
  :delight
  :config
  (drag-stuff-define-keys)
  (drag-stuff-global-mode 1))

(use-package browse-kill-ring
  :config
  (browse-kill-ring-default-keybindings))

(use-package delsel
  :hook (after-init . delete-selection-mode))

(use-package minor-mode-hack
  :commands show-minor-mode-map-priority)

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
  :delight
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-character ?\┊
        highlight-indent-guides-responsive 'top))

(use-package ediff
  :hook (ediff-quit . winner-undo)
  :custom
  (ediff-diff-options "-w")
  (ediff-highlight-all-diffs t)
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-merge-split-window-function 'split-window-horizontally))

(use-package comment-dwim-2
  :bind ("M-;" . comment-dwim-2)
  :config
  (setq cd2/region-command 'cd2/comment-or-uncomment-region))

(use-package mwim
  :bind (("C-a" . 'mwim-beginning-of-code-or-line)
         ("C-e" . 'mwim-end-of-code-or-line)
         ("<home>" . 'mwim-beginning-of-code-or-line)
         ("<end>" . 'mwim-end-of-code-or-line)))

(use-package fast-scroll
  :delight
  :hook (after-init . fast-scroll-mode)
  :config
  ;; TODO: Disable highlights, etc.
  (add-hook 'fast-scroll-start-hook (lambda () (flycheck-mode -1)))
  (add-hook 'fast-scroll-end-hook (lambda () (flycheck-mode 1)))
  (fast-scroll-mode 1)
  (fast-scroll-config))

(use-package projectile
  :delight
  :hook (after-init . projectile-mode)
  :bind ("C-<tab>" . projectile-next-project-buffer)
  :bind ("C-c C-p" . 'projectile-command-map)
  :config
  (setq projectile-project-search-path '("~/code/")
        projectile-auto-discover t
        projectile-enable-caching t
        projectile-indexing-method 'alien
        projectile-globally-ignored-file-suffixes '("#" "~" ".swp" ".o" ".so" ".exe" ".dll" ".elc" ".pyc" ".jar")
        projectile-globally-ignored-directories '(".git" "node_modules" "__pycache__" ".vs")
        projectile-globally-ignored-files '("TAGS" "tags" ".DS_Store")
        projectile-completion-system 'ivy)
  (with-eval-after-load 'projectile
    (add-to-list 'projectile-project-root-files-functions #'+project/lsp-project-root)
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
    (setq projectile-buffers-filter-function #'+project/projectile-buffer-filter-function)))

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
  :after (:all dired magit)
  :hook ((dired-mode . diff-hl-dired-mode)
         (magit-pre-refresh . diff-hl-magit-pre-refresh)
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
      (vlf file))))

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
        dumb-jump-force-searcher 'rg
        dumb-jump-selector 'ivy)
  (add-to-list 'xref-backend-functions 'dumb-jump-xref-activate t)
  (add-hook 'dumb-jump-after-jump-hook #'better-jumper-set-jump))

(use-package autorevert
  :straight nil
  :delight autorevert-mode)

(use-package eldoc
  :delight eldoc-mode)

(use-package magit
  :commands (magit magit-status magit-blame magit-mode magit-file-popup)
  :bind (("C-x g" . magit-status)
         ("C-x C-g r" . magit-run)
         ("C-x C-g l" . magit-file-log)
         ("C-x C-g c" . magit-commit)
         ("C-x C-g g" . magit-grep))
  :config
  (setq magit-status-margin '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 18))
  (setq magit-diff-refine-hunk t)
  (setq magit-commit-arguments '("--verbose"))
  (setq magit-section-initial-visibility-alist
        '((unpulled . show)
          (unpushed . show)
          (untracked . show)
          (unstaged . show)
          (stashes . hide)
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

(use-package magit-todos
  :after magit
  :hook (magit-mode . magit-todos-mode)
  :custom
  (magit-todos-exclude-globs '("node_modules" "*.json"))
  :config
  (setq magit-todos-auto-group-items 'always))

(use-package magit-delta
  :delight
  :after magit
  :config
  (setq magit-delta-hide-plus-minus-markers nil)
  (magit-delta-mode))

(use-package forge
  :after magit)

(use-package github-review
  :straight (:host github :repo "charignon/github-review" :files ("github-review.el")))

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

(use-package auto-dim-other-buffers
  :config
  (auto-dim-other-buffers-mode t))

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
            (if this-win-2nd (other-window 1)))))))

(use-package buffer-expose
  :bind (("M-<tab>" . 'buffer-expose))
  :config
  (setq buffer-expose-auto-init-aw t))

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
  ("C-c w" . ace-jump-word-mode)
  ("C-c i" .  ace-jump-line-mode))

(use-package anzu
  :delight
  :hook ((after-init . global-anzu-mode))
  :bind (([remap query-replace] . anzu-query-replace)
         ([remap query-replace-regexp] . anzu-query-replace-regexp)))


(use-package smex
  :bind ("M-x" . smex)
  :init
  (smex-initialize))

(use-package flx)

(use-package fuz)

(use-package posframe)

(use-package popwin)

(use-package point-history
  :delight
  :straight (:host github :repo "blue0513/point-history" :branch "master")
  :config
  (point-history-mode t))

(use-package prescient
  :disabled t
  :config
  (prescient-persist-mode 1))

(use-package ivy
  :bind ("C-x b" . ivy-switch-buffer)
  :after flx
  :config
  (setq ivy-re-builders-alist
        '((swiper . regexp-quote)
          (t . ivy--regex-fuzzy)))
  (setq ivy-initial-inputs-alist nil
        ivy-use-selectable-prompt t
        ivy-auto-select-single-candidate t
        ivy-rich-parse-remote-buffer nil
        +ivy-buffer-icons nil
        ivy-use-virtual-buffers t
        enable-recursive-minibuffers t
        ivy-dynamic-exhibit-delay-ms 250
        ivy-magic-slash-non-match-action 'ivy-magic-slash-non-match-cd-selected
        ivy-height 25
        ivy-rich-switch-buffer-name-max-length 50))

(use-package ivy-xref
  :init
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs)
  (setq xref-show-definitions-function #'ivy-xref-show-defs)
  :commands ivy-xref-show-xrefs)

(use-package ivy-prescient
  :disabled t
  :config
  :after (:all ivy prescient)
  (ivy-prescient-mode 1)
  (setq ivy-prescient-enable-sorting t
        ivy-prescient-enable-filtering t))

(use-package ivy-posframe
  :delight
  :disabled t
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
  ;; :bind ("M-x" . 'counsel-M-x)
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

(use-package counsel-gtags
  :after counsel
  :bind ("M-g t d" . 'counsel-gtags-find-definition)
  :bind ("M-g t r" . 'counsel-gtags-find-reference)
  :bind ("M-g t s" . 'counsel-gtags-find-symbol)
  :bind ("M-g t t" . 'counsel-gtags-command-dwim)
  :config
  (counsel-gtags-mode 1))

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

(use-package symbol-overlay
  :config
  (defun symbol-overlay-goto-first ()
    (interactive)
    (let* ((symbol (symbol-overlay-get-symbol))
           (keyword (symbol-overlay-assoc symbol))
           (a-symbol (car keyword))
           (before (symbol-overlay-get-list a-symbol 'car))
           (count (length before)))
      (symbol-overlay-jump-call 'symbol-overlay-basic-jump (- count))))

  (defun symbol-overlay-goto-last ()
    (interactive)
    (let* ((symbol (symbol-overlay-get-symbol))
           (keyword (symbol-overlay-assoc symbol))
           (a-symbol (car keyword))
           (after (symbol-overlay-get-list a-symbol 'cdr))
           (count (length after)))
      (symbol-overlay-jump-call 'symbol-overlay-basic-jump (- count 1))))

  (define-key symbol-overlay-map (kbd "<") 'symbol-overlay-goto-first)
  (define-key symbol-overlay-map (kbd ">") 'symbol-overlay-goto-last)
  (define-key symbol-overlay-map (kbd "h") 'nil)
  (define-key symbol-overlay-map (kbd "?") 'symbol-overlay-map-help))

(use-package paren
  :hook ((after-init . (lambda () (show-paren-mode -1)))
         (prog-mode . show-paren-local-mode))
  :config
  (setq show-paren-when-point-inside-paren t)
  (setq show-paren-when-point-in-periphery t)

  (defun show-paren-function-advice (fn)
    "Highlight enclosing parens."
    (cond ((looking-at-p "\\s(") (funcall fn))
          ((derived-mode-p 'python-mode)
           (save-excursion
             (ignore-errors
               (let* ((cur-pos (point))
                      (paren-open-pos (search-backward-regexp "\\s(" (point-min) t))
                      (paren-close-pos (and paren-open-pos (search-forward-regexp "\\s)" cur-pos t))))
                 (when (and paren-open-pos (not paren-close-pos))
                   (goto-char (1+ paren-open-pos))
                   (funcall fn))))))
          (t (save-excursion
               (ignore-errors (backward-up-list))
               (funcall fn)))))

  (advice-add 'show-paren-function :around #'show-paren-function-advice)

  (defun show-paren-local-mode ()
    (interactive)
    (setq-local show-paren-mode t)))

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

(use-package elec-pair
  :ensure nil
  :hook ((prog-mode
          web-mode
          conf-mode
          yaml-mode
          editorconfig-mode
          vue-mode
          cider-repl-mode
          minibuffer-setup
          ) . electric-pair-local-mode)
  :bind ("C-j" . newline-and-indent))

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

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package whitespace
  :delight
  :hook (prog-mode . whitespace-mode)
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
  :after yasnippet)

(use-package lsp-mode
  ;; :straight nil
  ;; :load-path "~/code/elisp/lsp-mode"
  :hook (lsp-after-open . lsp-enable-imenu)
  :hook (lsp-after-open . lsp-lens-mode)
  :hook (lsp-after-open . lsp-headerline-breadcrumb-mode)
  :hook (lsp-mode       . lsp-enable-which-key-integration)
  :commands (lsp lsp-deferred)
  :config
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
        lsp-headerline-breadcrumbs-mode nil
        lsp-idle-delay 1
        lsp-keep-workspace-alive nil
        lsp-navigation 'both
        lsp-prefer-capf t
        lsp-prefer-flymake nil
        lsp-response-timeout 10
        lsp-semantic-highlighting t
        lsp-session-file "~/.lsp-sessions"
        lsp-signature-auto-activate nil
        lsp-signature-render-all nil
        lsp-headerline-breadcrumb-segments '(file symbols))

  (use-package lsp-lens
    :straight nil
    :config
    (setq lsp-lens-debounce-interval 1.5))

  (use-package lsp-completion
    :straight nil
    :config
    (setq lsp-completion-provider :capf))

  (advice-add 'lsp :before (lambda (&rest _args)
                             (eval '(setf (lsp-session-server-id->folders (lsp-session)) (ht)))))
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

  (setq-default lsp-ui-doc-frame-parameters
                '((left . -1)
                  (top . -1)
                  (no-accept-focus . t)
                  (min-width . 0)
                  (width . 0)
                  (min-height . 0)
                  (height . 0)
                  (internal-border-width . 0)
                  (vertical-scroll-bars)
                  (horizontal-scroll-bars)
                  (left-fringe . 0)
                  (right-fringe . 0)
                  (menu-bar-lines . 0)
                  (tool-bar-lines . 0)
                  (line-spacing . 0.1)
                  (unsplittable . t)
                  (undecorated . t)
                  (minibuffer . nil)
                  (visibility . nil)
                  (mouse-wheel-frame . nil)
                  (no-other-frame . t)
                  (cursor-type)
                  (no-special-glyphs . t)))

  (when (featurep 'doom-themes)
    (set-face-background 'lsp-ui-doc-background (doom-color 'bg-alt)))

  (defun +lsp/lsp-ui-doc--make-request-advice nil
    "Request the documentation to the LS."
    (when (and (not (bound-and-true-p lsp-ui-peek-mode))
               (lsp--capability "hoverProvider"))
      (-if-let (bounds (and (not (memq (char-after) '(?  ?\t ?\n ?\) ?\] ?\})))
                            (or (and (symbol-at-point) (bounds-of-thing-at-point 'symbol))
                                (and (looking-at "[[:graph:]]") (cons (point) (1+ (point)))))))
          (unless (equal lsp-ui-doc--bounds bounds)
            (lsp--send-request-async
             (lsp--make-request "textDocument/hover" (lsp--text-document-position-params))
             (lambda (hover) (lsp-ui-doc--callback hover bounds (current-buffer)))))
        (lsp-ui-doc--hide-frame))))

  (advice-add 'lsp-ui-doc--make-request :override '+lsp/lsp-ui-doc--make-request-advice)

  (defun +lsp/toggle-doc-show ()
    "Popup/Hide hover information"
    (interactive)
    (if lsp-ui-doc-mode
        (progn
          (message "lsp-ui-doc disabled")
          (lsp-ui-doc-hide)
          (lsp-ui-doc-mode -1))
      (message "lsp-ui-doc enabled")
      (lsp-ui-doc-mode 1)
      (lsp-ui-doc-show)))

  (set-face-foreground 'lsp-ui-sideline-code-action "#FF8C00")

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
  :commands eglot-ensure
  :config
  (add-hook 'eglot--managed-mode-hook
            (lambda ()
              (when (bound-and-true-p read-process-output-max)
                (setq-local read-process-output-max (* 1024 1024))))))

(use-package eldoc-box
  ;; :hook (lsp-mode . eldoc-box-hover-at-point-mode)
  :config
  (setq lsp-signature-function 'eldoc-message))

(use-package company
  :delight
  :hook (after-init . global-company-mode)
  :init
  (setq company-frontends '(company-pseudo-tooltip-frontend
                            company-echo-metadata-frontend)
        company-tooltip-limit 20
        company-tooltip-idle-delay 0.5
        company-tooltip-flip-when-above t
        company-tooltip-align-annotations t
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case t
        company-dabbrev-other-buffers 'all
        company-minimum-prefix-length 3
        company-idle-delay 0.5
        company-require-match 'never)
  :config
  (global-company-mode +1))


(use-package company-quickhelp
  :if (or (< emacs-major-version 26)
          (not (display-graphic-p)))
  :after company
  :bind (:map company-active-map ("C-c h" . company-quickhelp-manual-begin))
  :hook (global-company-mode . company-quickhelp-mode)
  :preface
  (setq pos-tip-use-relative-coordinates t
        company-quickhelp-delay 0.3))

(use-package company-quickhelp-terminal
  :if (not (display-graphic-p))
  :after company
  :hook (global-company-mode . company-quickhelp-terminal-mode))

(use-package company-box
  :delight
  :if (and (>= emacs-major-version 26)
           (display-graphic-p))
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

(use-package flymake-diagnostic-at-point
  :after flymake
  :hook (flymake-mode . flymake-diagnostic-at-point-mode))

(use-package popwin)

(use-package quick-peek)

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
              flycheck-display-errors-delay 0.0)
  :bind (:map flycheck-error-list-mode-map
              ("C-n"    . flycheck-error-list-next-error)
              ("C-p"    . flycheck-error-list-previous-error)
              ("RET"    . flycheck-error-list-goto-error)
              ([return] . flycheck-error-list-goto-error))
  :config
  (require 'popwin)
  (setq flycheck-indication-mode 'left-fringe
        flycheck-emacs-lisp-load-path 'inherit
        flycheck-idle-change-delay 2
        flycheck-check-syntax-automatically '(save mode-enabled))

  (define-fringe-bitmap 'my-flycheck-fringe-indicator
    (vector #b00111111
            #b00111111
            #b00111111
            #b00111111
            #b00111111
            #b00111111
            #b00111111
            #b00111111
            #b00111111
            #b00111111
            #b00111111
            #b00111111
            #b00111111
            #b00111111
            #b00111111
            #b00111111
            #b00111111))
  (let ((bitmap 'my-flycheck-fringe-indicator))
    (flycheck-define-error-level 'error
      :severity 2
      :overlay-category 'flycheck-error-overlay
      :fringe-bitmap bitmap
      :error-list-face 'flycheck-error-list-error
      :fringe-face 'flycheck-fringe-error)
    (flycheck-define-error-level 'warning
      :severity 1
      :overlay-category 'flycheck-warning-overlay
      :fringe-bitmap bitmap
      :error-list-face 'flycheck-error-list-warning
      :fringe-face 'flycheck-fringe-warning)
    (flycheck-define-error-level 'info
      :severity 0
      :overlay-category 'flycheck-info-overlay
      :fringe-bitmap bitmap
      :error-list-face 'flycheck-error-list-info
      :fringe-face 'flycheck-fringe-info))

  (push '("^\\*Flycheck.+\\*$"
          :regexp t
          :dedicated t
          :position bottom
          :stick t
          :noselect t)
        popwin:special-display-config)

  (with-eval-after-load 'flycheck
    ;; Display Flycheck errors in GUI tooltips
    (if (display-graphic-p)
        (use-package flycheck-posframe
          :after flycheck
          :hook (flycheck-mode . flycheck-posframe-mode))
      (use-package flycheck-popup-tip
        :after flycheck
        :hook (flycheck-mode . flycheck-popup-tip-mode)))

    ;; toggle flycheck window
    (defun +flycheck/toggle-flycheck-error-list ()
      "Toggle flycheck's error list window.
If the error list is visible, hide it.  Otherwise, show it."
      (interactive)
      (-if-let (window (flycheck-get-error-list-window))
          (quit-window nil window)
        (flycheck-list-errors)))

    (defun +flycheck/goto-flycheck-error-list ()
      "Open and go to the error list buffer."
      (interactive)
      (unless (get-buffer-window (get-buffer flycheck-error-list-buffer))
        (flycheck-list-errors)
        (switch-to-buffer-other-window flycheck-error-list-buffer)))

    (defun +flycheck/popup-errors ()
      "Show the error list for the current buffer."
      (interactive)
      (unless flycheck-mode
        (user-error "Flycheck mode not enabled"))
      ;; Create and initialize the error list
      (unless (get-buffer flycheck-error-list-buffer)
        (with-current-buffer (get-buffer-create flycheck-error-list-buffer)
          (flycheck-error-list-mode)))
      (flycheck-error-list-set-source (current-buffer))
      ;; Reset the error filter
      (flycheck-error-list-reset-filter)
      ;; Popup the error list in the bottom window
      (popwin:popup-buffer flycheck-error-list-buffer)
      ;; Finally, refresh the error list to show the most recent errors
      (flycheck-error-list-refresh))

    (defalias 'show-error-at-point-soon 'flycheck-show-error-at-point)
    (add-to-list 'flycheck-emacs-lisp-checkdoc-variables 'sentence-end-double-space)))

(use-package flycheck-inline
  :disabled t
  :after flycheck
  :hook (flycheck-mode . flycheck-inline-mode)
  :config
  (setq flycheck-inline-display-function
        (lambda (msg pos)
          (let* ((ov (quick-peek-overlay-ensure-at pos))
                 (contents (quick-peek-overlay-contents ov)))
            (setf (quick-peek-overlay-contents ov)
                  (concat contents (when contents "\n") msg))
            (quick-peek-update ov)))
        flycheck-inline-clear-function #'quick-peek-hide))

(use-package flycheck-package
  :after flycheck
  :init
  (after! elisp-mode
    (flycheck-package-setup)))

(use-package flyspell-correct
  :after flyspell
  :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper)))

(use-package flyspell-correct-ivy
  :after lyspell-correct)

(use-package editorconfig
  :delight
  :config (editorconfig-mode 1))

(use-package ggtags)

(use-package counsel-gtags)

(use-package dotnet
  :config
  (setenv "DOTNET_USE_POLLING_FILE_WATCHER" "true"))

(use-package sharper
  :straight (:host github :repo "sebasmonia/sharper" :branch "master")
  :bind
  ("C-c n" . sharper-main-transient))

(use-package csharp-mode
  :after (:all lsp-mode company flycheck dotnet omnisharp)
  :hook (csharp-mode . lsp-deferred)
  :hook (csharp-mode . omnisharp-mode)
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
  (setq lsp-csharp-server-path "~/code/csharp/omnisharp-roslyn/artifacts/scripts/OmniSharp.Stdio")
  (setq indent-tabs-mode nil
        c-syntactic-indentation t
        c-basic-offset 4
        truncate-lines t
        tab-width 4)
  (electric-pair-local-mode 1))


(use-package omnisharp
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
  :after (:all dotnet lsp-mode projectile)
  :commands fsharp-mode
  :hook (fsharp-mode . lsp-deferred)
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
        lsp-fsharp-auto-workspace-init t
        lsp-fsharp-exclude-directories ["paket-files" ".git" "packages" "node_modules" "tests/projects"]
        lsp-log-io t)
  (add-to-list 'company-transformers 'company-sort-prefer-same-case-prefix)
  (setq indent-region-function '(lambda (start end &optional indent-offset))))

(use-package haskell-mode
  :commands haskell-mode
  :hook (haskell-mode . flymake-mode)
  :hook (haskell-mode . flycheck-mode))

(use-package dante
  :after haskell-mode
  :commands dante-mode
  :hook (haskell-mode . dante-mode))

(use-package lsp-haskell
  :after haskell-mode
  :hook (haskell-mode . lsp-deferred)
  :hook (haskell-literate-mode . lsp-deferred)
  :config
  (setq lsp-haskell-server-path "haskell-language-server-wrapper"))

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

(use-package sh-mode
  :straight nil
  :after lsp
  :hook (sh-mode . lsp-deferred))

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
