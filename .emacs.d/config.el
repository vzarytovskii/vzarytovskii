(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(blink-cursor-mode -1)
(global-hl-line-mode +1)
(line-number-mode +1)
(global-display-line-numbers-mode 1)
(column-number-mode t)
(size-indication-mode t)

(defconst *sys/gui*
  (display-graphic-p)
  "Are we running on a GUI Emacs?")

(defmacro λ! (&rest body)
  "A shortcut for inline interactive lambdas."
  (declare (doc-string 1))
  `(lambda () (interactive) ,@body))

(defalias 'lambda! 'λ!)

(defmacro after! (package &rest body)
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

(defun smart-kill-whole-line (&optional arg)
  "A simple wrapper around `kill-whole-line' that respects indentation."
  (interactive "P")
  (kill-whole-line arg)
  (back-to-indentation))

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

(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(global-set-key [remap kill-whole-line] 'smart-kill-whole-line)
(global-set-key [remap move-beginning-of-line] 'smarter-move-beginning-of-line)
(global-set-key (kbd "C-x 2") 'vsplit-last-buffer)
(global-set-key (kbd "C-x 3") 'hsplit-last-buffer)
(global-set-key (kbd "C-x |") 'toggle-window-split)
(global-set-key (kbd "C-c 2") 'duplicate-current-line-or-region)
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-x x") '+sidebar-toggle)
(global-set-key [(meta up)] 'move-line-up)
(global-set-key [(meta down)] 'move-line-down)

(use-package emacs
  :config
  (set-frame-font "JetBrains Mono 12" nil t)
  (setq-default major-mode 'text-mode
		compilation-scroll-output t)
  (setq compilation-always-kill t
	compilation-ask-about-save nil
	compilation-context-lines 10
	compilation-window-height 100
	compilation-scroll-output 'first-error
	inhibit-startup-screen t
	inhibit-startup-message t
	inhibit-startup-echo-area-message t
	initial-scratch-message nil

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
	truncate-lines t))

(use-package all-the-icons)

(use-package hydra)

(use-package vscode-dark-plus-theme
  :after emacs
  :config
  (load-theme 'vscode-dark-plus t))

(use-package smart-mode-line-atom-one-dark-theme)

(use-package smart-mode-line
  :after emacs
  :config
  (setq sml/theme 'atom-one-dark
	sml/no-confirm-load-theme t)
  (add-hook 'after-init-hook 'sml/setup))

(use-package async
  :init
  (dired-async-mode 1)
  (async-bytecomp-package-mode 1)
  :custom (async-bytecomp-allowed-packages '(all)))

(use-package diminish
  :ensure t)

(use-package gcmh
  :diminish gcmh-mode
  :init
  (setq gcmh-verbose nil)
  :config
  (gcmh-mode 1))

(use-package no-littering)

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode +1))

(use-package which-func
  :defer t
  :hook 'after-init-hook)

(use-package expand-region
  :bind ("C-=" . 'er/expand-region))

(use-package multiple-cursors
  :config
  (add-to-list 'mc/unsupported-minor-modes 'lispy-mode)
  (setq mc/always-run-for-all t)
  :bind (("C-S-c C-S-c" . 'mc/edit-lines)
	 ("C-d" . 'mc/mark-next-like-this)
	 ("C->" . 'mc/mark-next-like-this)
	 ("C-<" . 'mc/mark-previous-like-this)
	 ("C-c C-<" . 'mc/mark-all-like-this)))

(use-package projectile
  :diminish projectile-mode
  :bind ("C-c C-p" . 'projectile-command-map)
  :custom
  (projectile-mode +1))

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
  (dired-rainbow-define interpreted "#38c172" ("py" "ipynb" "rb" "pl" "t" "msql" "mysql" "pgsql" "sql" "r" "clj" "cljs" "scala" "js"))
  (dired-rainbow-define compiled    "#4dc0b5" ("asm" "cl" "lisp" "el" "c" "h" "c++" "h++" "hpp" "hxx" "m" "cc" "cs" "cp" "cpp" "go" "f" "for" "ftn" "f90" "f95" "f03" "f08" "s" "rs" "hi" "hs" "pyc" ".java"))
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
  :after    dired
  :bind (:map dired-narrow-map
	      ("<down>"  . dired-narrow-next-file)
	      ("<up>"    . dired-narrow-previous-file)
	      ("<right>" . dired-narrow-enter-directory)))

(use-package dired-collapse
  :hook 'dired-mode-hook)

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
  (setq dumb-jump-default-project doom-emacs-dir
	dumb-jump-aggressive nil
	dumb-jump-selector
	(cond ((featurep! :completion ivy)  'ivy)
	      ((featurep! :completion helm) 'helm)
	      ('popup)))
  (add-hook 'dumb-jump-after-jump-hook #'better-jumper-set-jump))

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
  :diminish
  :hook (after-init . global-git-gutter-mode)
  :init (setq git-gutter:visual-line t
	       git-gutter:disabled-modes '(asm-mode image-mode)
	       git-gutter:modified-sign "❚"
	       git-gutter:added-sign "✚"
	       git-gutter:deleted-sign "✘")

  :bind (("C-c v =" . git-gutter:popup-hunk)
	 ("C-c p" . git-gutter:previous-hunk)
	 ("C-c n" . git-gutter:next-hunk)))

(use-package gitattributes-mode :defer t)

(use-package gitconfig-mode :defer t)

(use-package gitignore-mode :defer t)

(use-package smerge-mode
  :diminish
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
  :diminish highlight-symbol-mode
  :delight highlight-symbol-mode
  :hook (highlight-symbol-mode . highlight-symbol-nav-mode)
  :hook (prog-mode . highlight-symbol-mode)
  :custom
  (highlight-symbol-highlight-single-occurrence nil)
  (highlight-symbol-idle-delay 0)
  (highlight-symbol-on-navigation-p t))

(use-package undo-tree
  :diminish undo-tree-mode
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
  :bind ("M-s" . 'deadgrep))

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
	 ".*LV.*"
	 ".*Ilist.*"))
  :config
  (dimmer-configure-magit)
  (dimmer-configure-posframe)
  (dimmer-mode t))

(use-package ace-window
  :bind ("C-x o" . 'ace-window)
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

(use-package anzu
  :defer t
  :hook ((after-init . global-anzu-mode))
  :bind ([remap query-replace] . anzu-query-replace-regexp))

(use-package zoom
  :config
  (custom-set-variables
   '(zoom-size '(0.618 . 0.618)))
  (zoom-mode 1))

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
  :straight (:host github :repo "blue0513/point-history" :branch "master")
  :config
  (point-history-mode t))

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
	ivy-height 30
	ivy-rich-switch-buffer-name-max-length 50))

(use-package ivy-posframe
  :after ivy
  :disabled t
  :config
  (setq ivy-posframe-parameters
	`((min-width . 220)
	  (min-height . ,ivy-height)
	  (left-fringe . 0)
	  (right-fringe . 0)
	  (internal-border-width . 30))
	ivy-display-functions-alist
	'((counsel-git-grep)
	  (counsel-rg)
	  (swiper)
	  (counsel-irony . ivy-display-function-overlay)
	  (ivy-completion-in-region . ivy-display-function-overlay)
	  (t . ivy-posframe-display-at-frame-center)))
  (setq ivy-posframe-display-functions-alist
	'((complete-symbol . ivy-posframe-display-at-point)
	  (counsel-M-x     . ivy-posframe-display-at-frame-top-center)
	  (t               . ivy-posframe-display-at-frame-top-center)))
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
  :diminish counsel-mode
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

(use-package nav-flash
  :config
  (defun +advice/nav-flash-show (orig-fn &rest args)
    (ignore-errors (apply orig-fn args)))
  (advice-add 'nav-flash-show :around #'+advice/nav-flash-show))

(use-package smartparens
  :diminish smartparens-mode
  :config
  (progn
    (require 'smartparens-config)
    (smartparens-global-mode 1)
    (show-paren-mode t)))

(use-package whitespace
  :preface
  (defun no-trailing-whitespace ()
    "Turn off display of trailing whitespace in this buffer."
    (setq show-trailing-whitespace nil))
  :init
  ;; But don't show trailing whitespace in SQLi, inf-ruby etc.
  (dolist (hook '(artist-mode-hook
		  picture-mode-hook
		  special-mode-hook
		  Info-mode-hook
		  eww-mode-hook
		  term-mode-hook
		  vterm-mode-hook
		  comint-mode-hook
		  compilation-mode-hook
		  twittering-mode-hook
		  minibuffer-setup-hook
		  fundamental-mode))
    (add-hook hook #'no-trailing-whitespace))
  :diminish whitespace-mode)

(use-package whitespace-cleanup-mode
  :init
  (setq whitespace-cleanup-mode-only-if-initially-clean nil)
  (setq-default whitespace-style
		'(face tabs spaces trailing space-before-tab
		       newline indentation empty space-after-tab
		       space-mark tab-mark newline-mark))
  :hook ((after-init . global-whitespace-cleanup-mode))
  :diminish (whitespace-cleanup-mode)
  :bind (("<remap> <just-one-space>" . cycle-spacing)))

;; UI
(use-package ibuffer
  ;;:bind (("C-x C-b" . ibuffer))
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
			  (mode . haskell-mode)
			  (mode . javascript-mode)
			  (mode . lisp-mode)
			  (mode . python-mode)
			  (mode . ruby-mode)
			  (mode . rust-mode)
			  (mode . html-mode)
			  (mode . css-mode)
			  (mode . prog-mode)))
	    ("GNUs"      (or
			  (mode . message-mode)
			  (mode . bbdb-mode)
			  (mode . mail-mode)
			  (mode . gnus-group-mode)
			  (mode . gnus-summary-mode)
			  (mode . gnus-article-mode)
			  (name . "^\\.bbdb$")
			  (name . "^\\.newsrc-dribble")))
	    ("Magit"      (name . "^magit"))
	    ("Help"      (or
			  (name . "^\\*Help\\*$")
			  (name . "^\\*Apropos\\*$")
			  (name . "^\\*info\\*$")
			  (name . "^\\*helpful")))
	    ("Custom"    (or
			  (mode . custom-mode)
			  (name . "^\\*Customize")))
	    ("Helm"       (mode . helm-major-mode))
	    ))
	 ibuffer-show-empty-filter-groups nil
	 ibuffer-default-sorting-mode     'filename/process)
  :hook ((ibuffer-mode . ibuffer-switch-to-normal)))

(use-package ibuffer-sidebar
  :after ibuffer
  :bind ("C-x C-b" . 'ibuffer-sidebar-toggle-sidebar)
  :commands (ibuffer-sidebar-toggle-sidebar)
  :config
  (setq ibuffer-sidebar-use-custom-font t))

(use-package ibuffer-vc)

(use-package hl-line
  :defer t
  :hook ((after-init . global-hl-line-mode)))

(use-package symbol-overlay
  :bind (("M-i"  . symbol-overlay-put)
	 ("M-n"  . symbol-overlay-switch-forward)
	 ("M-p"  . symbol-overlay-switch-backward)
	 ("<f8>" . symbol-overlay-remove-all)
	 ("<f7>" . symbol-overlay-mode)))

(use-package treemacs
  :defer t
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
	treemacs-persist-file
	(no-littering-expand-var-file-name "treemacs-persist")
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
  :defer t
  :bind (:map yas-minor-mode-map
	      ("C-'" . yas-expand))
  :commands (yas-minor-mode)
  :hook ((prog-mode      . yas-minor-mode)
	 (yas-minor-mode . (lambda ()
			     (add-to-list
			      'yas-snippet-dirs
			      (concat user-emacs-directory "snippets")))))
  :config (yas-reload-all))

(use-package yasnippet-snippets
  :after yasnippet)

(use-package lsp-mode
  :defer t
  :commands lsp lsp-deferred
  :hook (lsp-after-open . lsp-enable-imenu)
  :hook (lsp-after-open . lsp-lens-mode)
  :hook (lsp-after-open . lsp-headerline-breadcrumbs-mode)
  :hook (lsp-mode . lsp-enable-which-key-integration)
  :hook (fsharp-mode . lsp)
  :config
  (setq lsp-navigation 'both
	lsp-auto-guess-root t
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
	lsp-prefer-flymake nil
	lsp-response-timeout 10
	lsp-signature-auto-activate nil
	lsp-signature-render-all nil)
  :custom
  (lsp-file-watch-threshold 2000)
  (read-process-output-max (* 1024 1024))
  (lsp-eldoc-hook nil))

(use-package lsp-ui
  :after lsp-mode
  :diminish
  :commands lsp-ui-mode
  :after lsp-mode
  :bind (("C-." . lsp-ui-sideline-apply-code-actions))
  ;; :hook (lsp-after-open . lsp-ui-mode)
  :hook (lsp-after-open . lsp-lens-mode)
  :custom-face
  (lsp-ui-doc-background ((t (:background nil))))
  (lsp-ui-doc-header ((t (:inherit (font-lock-string-face italic)))))
  :bind
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
	lsp-ui-sideline-enable nil
	lsp-ui-sideline-update-mode 'line
	lsp-ui-sideline-show-symbol nil
	lsp-ui-sideline-show-diagnostics t
	lsp-ui-sideline-show-hover nil
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
  :after (:all lsp ivy))

(use-package dap-mode
  :diminish
  :after lsp-mode
  :hook (dap-stopped . (lambda (arg) (call-interactively #'dap-hydra)))
  :hook (dap-mode . dap-ui-mode)
  :hook (dap-mode . dap-tooltip-mode)
  :bind (:map dap-mode-map
	      (("<f12>" . dap-debug)
	       ("<f8>" . dap-continue)
	       ("<f9>" . dap-next)
	       ("<M-f11>" . dap-step-in)
	       ("C-M-<f11>" . dap-step-out)
	       ("<f7>" . dap-breakpoint-toggle))))

(defvar company-backend-alist
  '((text-mode company-dabbrev company-yasnippet company-ispell company-files)
    (prog-mode company-capf company-yasnippet company-files)
    (conf-mode company-capf company-dabbrev-code company-yasnippet company-files)))

(defun add-company-backend (modes &rest backends)
 (declare (indent defun))
 (dolist (mode (nasy-enlist modes))
   (if (null (car backends))
       (setq company-backend-alist
	     (delq (assq mode company-backend-alist)
		   company-backend-alist))
     (setf (alist-get mode company-backend-alist)
	   backends))))

(defun company-backends ()
 (let (backends)
   (let ((mode major-mode)
	 (modes (list major-mode)))
     (while (setq mode (get mode 'derived-mode-parent))
       (push mode modes))
     (dolist (mode modes)
       (dolist (backend (append (cdr (assq mode company-backend-alist))
				(default-value 'company-backends)))
	 (push backend backends)))
     (delete-dups
      (append (cl-loop for (mode . backends) in company-backend-alist
		       if (or (eq major-mode mode)  ; major modes
			      (and (boundp mode)
				   (symbol-value mode))) ; minor modes
		       append backends)
	      (nreverse backends))))))

(defun company-init-backends-h ()
 "Set `company-backends' for the current buffer."
 (if (not company-mode)
     (remove-hook 'change-major-mode-after-body-hook #'company-init-backends-h 'local)
   (unless (eq major-mode 'fundamental-mode)
     (setq-local company-backends (company-backends)))
   (add-hook 'change-major-mode-after-body-hook #'company-init-backends-h nil 'local)))

(put 'company-init-backends-h 'permanent-local-hook t)

(use-package company
  :diminish company-mode
  :hook ((prog-mode) . company-mode)
  :commands (company-backends
	     company-init-backends-h
	     company-has-completion-p
	     company-toggle-auto-completion
	     company-complete
	     company-dabbrev-setup
	     company-whole-lines
	     company-dict-or-keywords
	     company-dabbrev-code-previous)
  :preface
  (defun company-has-completion-p ()
    "Return non-nil if a completion candidate exists at point."
    (and (company-manual-begin)
	 (= company-candidates-length 1)))

  (defun company-toggle-auto-completion ()
    "Toggle as-you-type code completion."
    (interactive)
    (require 'company)
    (setq company-idle-delay (unless company-idle-delay 0.2))
    (message "Auto completion %s"
	     (if company-idle-delay "enabled" "disabled")))

  (defun company-complete ()
    "Bring up the completion popup. If only one result, complete it."
    (interactive)
    (require 'company)
    (when (ignore-errors
	    (/= (point)
		(cdr (bounds-of-thing-at-point 'symbol))))
      (save-excursion (insert " ")))
    (when (and (company-manual-begin)
	       (= company-candidates-length 1))
      (company-complete-common)))

  (defun company-dabbrev-setup ()
    "Invokes `company-dabbrev-code' in prog-mode buffers and `company-dabbrev'
  everywhere else."
    (interactive)
    (call-interactively
     (if (derived-mode-p 'prog-mode)
	 #'company-dabbrev-code
       #'company-dabbrev)))

  (defun company-whole-lines (command &optional arg &rest ignored)
    "`company-mode' completion backend that completes whole-lines, akin to vim's
  C-x C-l."
    (interactive (list 'interactive))
    (require 'company)
    (pcase command
      (`interactive (company-begin-backend 'company-whole-lines))
      (`prefix      (company-grab-line "^[\t\s]*\\(.+\\)" 1))
      (`candidates
       (all-completions
	arg
	(delete-dups
	 (split-string
	  (replace-regexp-in-string
	   "^[\t\s]+" ""
	   (concat (buffer-substring-no-properties (point-min) (line-beginning-position))
		   (buffer-substring-no-properties (line-end-position) (point-max))))
	  "\\(\r\n\\|[\n\r]\\)" t))))))

  (defun company-dict-or-keywords ()
    "`company-mode' completion combining `company-dict' and `company-keywords'."
    (interactive)
    (require 'company-dict)
    (require 'company-keywords)
    (let ((company-backends '((company-keywords company-dict))))
      (call-interactively #'company-complete)))

  (defun company-dabbrev-code-previous ()
    (interactive)
    (require 'company-dabbrev)
    (let ((company-selection-wrap-around t))
      (call-interactively #'company-dabbrev-setup)
      (company-select-previous-or-abort)))
  :bind (:map company-active-map
	      ([tab] . smarter-yas-expand-next-field-complete)
	      ("TAB" . smarter-yas-expand-next-field-complete))
  :init
  (add-to-list 'completion-styles 'initials t)
  (setq company-tooltip-limit 10
	company-dabbrev-downcase nil
	company-dabbrev-ignore-case t
	company-global-modes '(not erc-mode message-mode help-mode gud-mode eshell-mode)
	company-backends '(company-capf)
	company-frontends'(company-pseudo-tooltip-frontend company-echo-metadata-frontend)
	company-dabbrev-other-buffers 'all
	company-tooltip-align-annotations t
	company-minimum-prefix-length 1
	company-idle-delay .2
	company-tooltip-idle-delay .2
	company-require-match 'never)
  :hook ((company-mode . company-init-backends-h))
  :config
  (global-company-mode +1)
  (defvar prev-whitespace-mode nil)
  (make-variable-buffer-local 'prev-whitespace-mode)
  (defvar show-trailing-whitespace nil)
  (make-variable-buffer-local 'show-trailing-whitespace)
  (defun pre-popup-draw ()
    "Turn off whitespace mode before showing company complete tooltip"
    (if whitespace-mode
	(progn
	  (setq my-prev-whitespace-mode t)
	  (whitespace-mode -1)))
    (setq show-trailing-whitespace show-trailing-whitespace)
    (setq show-trailing-whitespace nil))
  (defun post-popup-draw ()
    "Restore previous whitespace mode after showing company tooltip"
    (if prev-whitespace-mode
	(progn
	  (whitespace-mode 1)
	  (setq prev-whitespace-mode nil)))
    (setq show-trailing-whitespace show-trailing-whitespace))
  (advice-add 'company-pseudo-tooltip-unhide :before #'pre-popup-draw)
  (advice-add 'company-pseudo-tooltip-hide :after #'post-popup-draw)

  (defun company-backend-with-yas (backends)
    "Add :with company-yasnippet to company BACKENDS.
  Taken from https://github.com/syl20bnr/spacemacs/pull/179."
    (if (and (listp backends) (memq 'company-yasnippet backends))
	backends
      (append (if (consp backends)
		  backends
		(list backends))
	      '(:with company-yasnippet))))
  :diminish company-mode)

(use-package company-lsp
  :defer t
  :after (:all company lsp)
  :custom (company-lsp-cache-candidates 'auto))

(use-package company-box
  :diminish
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
    :hook (company-mode . company-posframe-mode))

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
  :hook ((after-init . global-flycheck-mode))
  :init (setq flycheck-display-errors-function
	       #'flycheck-display-error-messages-unless-error-list
	       flycheck-check-syntax-automatically '(save mode-enabled)
	       flycheck-display-errors-delay       0.25)
  :bind (:map flycheck-error-list-mode-map
	      ("C-n"    . flycheck-error-list-next-error)
	      ("C-p"    . flycheck-error-list-previous-error)
	      ("RET"    . flycheck-error-list-goto-error)
	      ([return] . flycheck-error-list-goto-error))
  :config (defalias 'show-error-at-point-soon
	    'flycheck-show-error-at-point)
  (add-to-list 'flycheck-emacs-lisp-checkdoc-variables 'sentence-end-double-space))


(use-package flycheck-package
  :after flycheck
  :init
  (after! elisp-mode
    (flycheck-package-setup)))


;; Spelling and stuff
(use-package bing-dict
  :bind (("C-c d" . bing-dict-brief))
  :init (setq bing-dict-show-thesaurus  'both
	       bing-dict-vocabulary-save t
	       bing-dict-cache-auto-save t
	       bing-dict-vocabulary-file
	       (no-littering-expand-var-file-name "bing-dict/vocabulary.org")
	       bing-dict-cache-file
	       (no-littering-expand-var-file-name "bing-dict/bing-dict-save.el")))

(use-package ispell
  :init
  (setq ispell (executable-find "aspell"))

  (setq-default ispell-program-name ispell
		ispell-silently-savep t
		ispell-dictionary "english"
		ispell-personal-dictionary
		(no-littering-expand-var-file-name "ispell/dictionary"))
  (when (string-suffix-p "aspell" ispell)
    (setq-default ispell-extra-args '("--reverse"))))

(use-package flyspell
  :diminish
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
