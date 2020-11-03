;;; functions.el --- Custom functions used in the config.
;;; Commentary:
;; Custom helper functions, for main config, see config.el

;;; -*- lexical-binding: t -*-

;;; Code:

(defun reload-init-file ()
    (interactive)	
    (load-file user-init-file))

(provide 'functions)
;;; functions.el ends here
