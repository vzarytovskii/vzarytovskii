;;; git.el --- Git-relate packages configuration.
;;; Commentary:
;; All git-related packages config, for main config, see config.el

;;; -*- lexical-binding: t -*-

;;; Code:

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
  :hook (magit-mode-hook . magit-todos-mode)
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

(use-package git-commit-insert-issue
  :config
  (add-hook 'git-commit-mode-hook 'git-commit-insert-issue-mode))

(use-package github-explorer)

(use-package github-review
  :straight (:host github :repo "charignon/github-review" :files ("github-review.el")))

(use-package git-link
  :bind (("C-x C-g i")))

(use-package git-blamed)

(use-package git-timemachine)

(use-package git-messenger
  :init (setq git-messenger:show-detail t)
  :bind (:map vc-prefix-map
              ("p" . git-messenger:popup-message)))

(use-package git-gutter-fringe
  :delight
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

(use-package gitattributes-mode)

(use-package gitconfig-mode)

(use-package gitignore-mode)

(use-package diff-hl
  :disabled t
  :config
  (global-diff-hl-mode))

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
  :hook ((find-file-hook . (lambda ()
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

(provide 'git)
;;; git.el ends here
