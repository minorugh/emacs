;;; 03_counsel.el --- Counsel configurations. -*- lexical-binding: t; no-byte-compile: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Counsel configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(leaf counsel
  :ensure t
  :defer-config (ivy-mode)
  :bind (("C-r" . swiper-thing-at-point)
		 ("C-s" . swiper-region)
		 ("C-:" . counsel-switch-buffer)
		 ("s-a" . counsel-ag)
		 ("s-r" . counsel-rg)
		 ("s-f" . counsel-fontawesome)
		 ("M-x" . counsel-M-x)
		 ("M-y" . counsel-yank-pop)
		 ("C-x m" . counsel--mark-ring)
		 ("C-x C-b" . ibuffer)
		 ("C-x C-f" . counsel-find-file)
		 ("C-x C-r" . counsel-recentf))
  :custom `((search-default-mode . nil)
			(ivy-use-virtual-buffers . t)
			(ivy-use-selectable-prompt . t)
			(enable-recursive-minibuffers . t)
			(counsel-find-file-ignore-regexp . (regexp-opt completion-ignored-extensions))
			(ivy-format-functions-alist . '((t . my:ivy-format-function-arrow))))
  :init
  (leaf fontawesome	:ensure t)

  (leaf ivy-rich :ensure t
	:hook (after-init-hook . ivy-rich-mode))

  (leaf amx	:ensure t
	:custom	`((amx-save-file . ,"~/.emacs.d/tmp/amx-items")
			  (amx-history-length . 20)))

  :config
  (defun swiper-region ()
	"If region is selected, `swiper-thing-at-point'.
  If the region isn't selected, `swiper'."
	(interactive)
	(if (not (use-region-p))
		(swiper)
      (swiper-thing-at-point)))

  (defun my:ivy-format-function-arrow (cands)
	"Transform into a string for minibuffer with CANDS."
	(ivy--format-function-generic
	 (lambda (str)
	   (concat (if (display-graphic-p)
				   (all-the-icons-octicon "chevron-right" :height 0.8 :v-adjust -0.05)
				 ">")
			   (propertize " " 'display `(space :align-to 2))
			   (ivy--add-face str 'ivy-current-match)))
	 (lambda (str)
	   (concat (propertize " " 'display `(space :align-to 2)) str))
	 cands
	 "\n"))

  (defun ad:counsel-ag (f &optional initial-input initial-directory extra-ag-args ag-prompt caller)
	(apply f (or initial-input
				 (and (not (thing-at-point-looking-at "^\\*+"))
                      (ivy-thing-at-point)))
           (unless current-prefix-arg
			 (or initial-directory default-directory))
           extra-ag-args ag-prompt caller))

  (with-eval-after-load "counsel"
	(require 'thingatpt nil t)
	(advice-add 'counsel-ag :around #'ad:counsel-ag)
	;; Make search trigger even with 2 characters
	(add-to-list 'ivy-more-chars-alist '(counsel-ag . 2)))

  (ivy-add-actions
   'counsel-ag
   '(("r" my-counsel-ag-in-dir "search in directory")))

  (defun my-counsel-ag-in-dir (_arg)
	"Search again with new root directory."
	(let ((current-prefix-arg '(4)))
      (counsel-ag ivy-text nil ""))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Migemo configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(leaf migemo
  :ensure t
  :hook (after-init-hook . migemo-init)
  :when (executable-find "cmigemo")
  :custom `((migemo-command . "cmigemo")
			(migemo-dictionary . "/usr/share/cmigemo/utf-8/migemo-dict"))
  :config
  (leaf swiper-migemo
	:doc "https://github.com/tam17aki/swiper-migemo"
	:el-get tam17aki/swiper-migemo
	:after migemo
	:global-minor-mode t
	:config
	(setq migemo-options '("--quiet" "--nonewline" "--emacs"))
	(add-to-list 'swiper-migemo-enable-command 'counsel-rg)
	(migemo-kill)
	(migemo-init)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; counsel related misc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Font-awesom
;; (leaf fontawesome :ensure t)

;; CSS
(leaf counsel-css
  :ensure t
  :hook (css-mode-hook . counsel-css-imenu-setup))

;; Tramp
(leaf counsel-tramp
  :ensure t
  :custom
  `((tramp-persistency-file-name . ,"~/.emacs.d/tmp/tramp")
	(tramp-default-method . "scp")
	(counsel-tramp-custom-connections
	 . '(/scp:xsrv:/home/minorugh/gospel-haiku.com/public_html/)))
  :config
  (defun my:tramp-quit ()
	"Quit tramp, if tramp connencted."
	(interactive)
	(when (get-buffer "*tramp/scp xsrv*")
	  (tramp-cleanup-all-connections)
	  (counsel-tramp-quit)
	  (message "Tramp Quit!"))))


(provide '03_counsel)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 03_counsel.el ends here
