;;; config.el --- Emacs configuration
;;; Commentary:
;; Main Emacs config, for bootstrap, see init.el

;;; -*- lexical-binding: t -*-

;;; Code:

;; Helper functions
(load-user "functions.el")

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

(provide 'config)
;;; config.el ends here
