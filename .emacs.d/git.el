;;; git.el --- Git-relate packages configuration.
;;; Commentary:
;; All git-related packages config, for main config, see config.el

;;; -*- lexical-binding: t -*-

;;; Code:

(use-package git-modes
  :defer t)

(use-package magit
  :defer t
  :commands (magit magit-status magit-blame magit-mode magit-file-popup)
  :bind (("C-x g" . magit-status)
         ("C-x C-g r" . magit-run)
         ("C-x C-g l" . magit-file-log)
         ("C-x C-g c" . magit-commit)
         ("C-x C-g g" . magit-grep))
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
                 (set-fill-column 120)))))

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
  :config
  (setq magit-delta-hide-plus-minus-markers nil)
  (magit-delta-mode))

(use-package forge
  :after magit)

(use-package git-commit-insert-issue
  :config
  (add-hook 'git-commit-mode-hook 'git-commit-insert-issue-mode))

(use-package graphql)

(use-package gh-notify)

(use-package github-explorer
  :after graphql)

;; Have multiple packages for PR handling, just to test them out.
(use-package github-review
  :straight (:host github :repo "charignon/github-review" :files ("github-review.el")))

(use-package code-review
  :after (:all magit forge))

(use-package pr-review
  :straight (:host github :repo "blahgeek/emacs-pr-review" :files (:defaults "graphql"))
  :after (:all magit forge))

(use-package git-link
  :bind (("C-x C-g i")))

(use-package git-blamed)

(use-package blamer
  :ensure t
  :defer 20
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 70)
  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
                    :background nil
                    :height 140
                    :italic t)))
  :config
  (setq blamer-author-formatter " ✎ %s "
	blamer-datetime-formatter "[%s]"
	blamer-commit-formatter "● %s"
	blamer-prettify-time-p t
	blamer-type 'both)

  (global-blamer-mode 1))

(use-package git-timemachine
  :bind ("C-x v t" . git-timemachine-toggle))

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
              )

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
