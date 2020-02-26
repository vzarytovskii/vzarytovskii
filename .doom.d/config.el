(setq user-full-name "Vlad Zarytovskii"
      user-mail-address "vzaritovsky@hotmail.com")

(setq doom-font (font-spec :family "monospace" :size 14))
(setq doom-theme 'doom-one)
(setq org-directory "~/org/")
(setq display-line-numbers-type t)

(use-package! magit-gh-pulls
  :after magit
  :config
  (add-hook! 'magit-mode-hook 'turn-on-magit-gh-pulls))
