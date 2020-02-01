;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! snails
   :recipe ( :host github :repo "manateelazycat/snails" :no-byte-compile t))

(package! powershell
   :recipe ( :host github :repo "jschaf/powershell.el"))

(package! focus
   :recipe ( :host github :repo "larstvei/Focus"))

(package! dimmer
   :recipe ( :host github :repo "gonewest818/dimmer.el"))

(package! beacon
  :recipe ( :host github :repo "Malabarba/beacon"))

(package! sublimity
  :recipe ( :host github :repo "zk-phi/sublimity"))

(package! company-tabnine
  :recipe (:host github :repo "TommyX12/company-tabnine"))
