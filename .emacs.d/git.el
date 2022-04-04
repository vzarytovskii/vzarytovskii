;;; git.el --- Git-relate packages configuration. -*- lexical-binding: t; -*-
;;; Commentary:
;; All git-related packages config, for main config, see config.el

;;; Code:

(use-package git-modes
  :defer t)

(use-package magit
  :defer t
  :commands (magit magit-status magit-blame magit-mode magit-file-popup)
  :bind (("C-x g" . magit-status)
         ("C-c g" . magit-dispatch-popup))
  :config
  (setq magit-status-margin '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 18)
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

(use-package libgit)

(use-package magit-libgit
  :after (:all magit libgit))

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
  :after (:all magit forge))

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

(provide 'git)
;;; git.el ends here
