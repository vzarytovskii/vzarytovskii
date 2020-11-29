;;; common.el --- Common functions and libs used in the config.
;;; Commentary:
;; Custom helper functions and libs, for main config, see config.el

;;; -*- lexical-binding: t -*-

;;; Code:

(use-package ov) ;; For manipulating overlays

(defun reload-init-file ()
    (interactive)	
    (load-file user-init-file))

(provide 'common)
;;; common.el ends here
