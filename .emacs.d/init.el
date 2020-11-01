;;; init.el --- Emacs configuration bootstrap.
;;; Commentary:
;; Emacs bootstrap, for main configuration, see config.el

;;; -*- lexical-binding: t -*-

;;; Code:

(let ((minver 26))
  (unless (>= emacs-major-version minver)
    (error "Your Emacs is too old -- this configuration requires v%s or higher" minver)))

(unless (>= emacs-major-version 27)
  (message "Early init: Emacs Version < 27.0")
  (load (expand-file-name "early-init.el" user-emacs-directory)))

;; Load packaging related config
(load (expand-file-name "packaging.el" user-emacs-directory))

(setenv "PATH" (concat (getenv "PATH") ":~/.dotnet:~/.cabal/bin:~/.ghcup/bin"))
(setq exec-path (append exec-path '("~/.dotnet" "~/.cabal/bin" "~/.ghcup/bin")))

;; Load actual config
(load (expand-file-name "config.el" user-emacs-directory))

(provide 'init)
;;; init.el ends here
