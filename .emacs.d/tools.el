;;; tools.el --- Emacs tools configuration
;;; Commentary:
;; Tools configuration (such as ripgrep, ivy, projectile, etc), for main config, see init.el

;;; -*- lexical-binding: t -*-

;;; Code:

(use-package deadgrep
  :if (executable-find "rg")
  :bind ("M-s" . 'deadgrep)
  :bind (:map deadgrep-mode-map
              ("M-e" . deadgrep-edit-mode)
              ("RET" . deadgrep-visit-result-other-window)
              ("o" . deadgrep-visit-result)))

(use-package flx)

(use-package prescient
  :config
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
  :init
  (use-package amx :defer t)
  (use-package counsel :delight :config (counsel-mode 1))
  (use-package swiper :defer t)
  (ivy-mode 1)
  :bind (("C-s" . my/swiper)
         ("C-x b" . counsel-buffer-or-recentf)
         ("C-x C-b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         (:map swiper-map
               ("M-a" . 'swiper-avy))
         (:map ivy-minibuffer-map
               ("C-r" . ivy-previous-line-or-history)
               ("M-RET" . ivy-immediate-done))
         (:map counsel-find-file-map
               ("C-~" . counsel-goto-local-home)))
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-height 10)
  (ivy-on-del-error-function nil)
  (ivy-magic-slash-non-match-action 'ivy-magic-slash-non-match-create)
  (ivy-count-format "【%d/%d】")
  (ivy-wrap t)
  :config
  (defun counsel-goto-local-home ()
      "Go to the $HOME of the local machine."
      (interactive)
    (ivy--cd "~/")))

(use-package ivy-prescient
  :after (:all ivy prescient)
  :config (ivy-prescient-mode +1))

(provide 'tools)
;;; tools.el ends here
