;;; config.el --- Emacs configuration
;;; Commentary:
;; Main Emacs config, for bootstrap, see init.el

;;; -*- lexical-binding: t -*-

;;; Code:

;; Common functions and libs
(load-user "common.el")

;; Core packages
(load-user "core.el")

;; Tooling packages
(load-user "tools.el")

;; UI config
(load-user "ui.el")

;; Git config
(load-user "git.el")

;; Programming confg
(load-user "programming.el")

;; Misc (telegram, mail, calendar, etc0
(load-user "misc.el")

(provide 'config)
;;; config.el ends here
