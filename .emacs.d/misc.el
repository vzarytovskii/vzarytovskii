;;; common.el --- Misc packages, like telega, notmuch, etc.
;;; Commentary:
;; Misc packages, for main config, see config.el

;;; -*- lexical-binding: t -*-

;;; Code:

(use-package telega
  :straight (:host github :repo "zevlg/telega.el" :branch "master")
  :load-path  "~/telega.el"
  :commands (telega)
  :defer t
  :config
  (setq telega-chat-show-avatars nil
        telega-root-show-avatars nil
        telega-user-show-avatars nil))

(provide 'misc)
;;; misc.el ends here
