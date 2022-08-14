;;; 06_utils.el --- Utility configurations. -*- lexical-binding: t; no-byte-compile: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

;; Key-chord
(leaf key-chord
  :ensure t
  :hook (after-init-hook . key-chord-mode)
  :chord (("df" . counsel-descbinds)
		  ("l;" . init-loader-show-log)
		  ("@@" . howm-list-all)
		  ("jk" . open-junk-file))
  :custom
  `((key-chord-two-keys-delay . 0.15)
	(key-chord-safety-interval-backward . 0.1)
	(key-chord-safety-interval-forward  . 0.25)))


;; auto-save-buffers
(leaf auto-save-buffers-enhanced
  :ensure t
  :custom
  `((auto-save-buffers-enhanced-exclude-regexps . '("^/ssh:" "^/scp:" "/sudo:"))
	(auto-save-buffers-enhanced-quiet-save-p . t)
	(auto-save-buffers-enhanced-save-scratch-buffer-to-file-p . t)
	(auto-save-buffers-enhanced-file-related-with-scratch-buffer . "~/.emacs.d/tmp/scratch")
	;; Disable to prevent freeze in tramp-mode
	(auto-save-buffers-enhanced-include-only-checkout-path . nil))
  :config
  (auto-save-buffers-enhanced t)
  (defun read-scratch-data ()
	(let ((file "~/.emacs.d/tmp/scratch"))
	  (when (file-exists-p file)
		(set-buffer (get-buffer "*scratch*"))
		(erase-buffer)
		(insert-file-contents file))))
  (read-scratch-data))


;; Sequential-command
(leaf sequential-command
  :el-get HKey/sequential-command
  :config
  (leaf sequential-command-config
	:hook (emacs-startup-hook . sequential-command-setup-keys)))


;; Syntax checking
(leaf flymake
  :hook (prog-mode-hook . flymake-mode)
  :config
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)
  (leaf flymake-posframe
	:el-get Ladicle/flymake-posframe
	:hook (flymake-mode-hook . flymake-posframe-mode)
	:custom (flymake-posframe-error-prefix . " ")))


;; Effective sorting
(leaf prescient
  :ensure t
  :hook (after-init-hook . prescient-persist-mode)
  :custom
  `((prescient-aggressive-file-save . t)
	(prescient-save-file . "~/.emacs.d/tmp/prescient-save"))
  :init
  (with-eval-after-load 'prescient
	(leaf ivy-prescient :ensure t :global-minor-mode t)
	(leaf company-prescient :ensure t :global-minor-mode t)))


;; Popup menu-item bindings
(leaf which-key
  :ensure t
  :hook (after-init-hook . which-key-mode)
  :custom (which-key-max-description-length . 40))


;; imenu-list
(leaf imenu-list
  :ensure t
  :bind ("<f2>" . imenu-list-smart-toggle)
  :custom
  `((imenu-list-size . 30)
	(imenu-list-position . 'left)
	(imenu-list-focus-after-activation . t)))


;; Popup window
(leaf popwin
  :ensure t
  :hook (after-init-hook . popwin-mode))


;; Edit multiple regions
(leaf iedit
  :ensure t
  :bind ("<insert>" . iedit-mode))


;; Extension for region
(leaf expand-region
  :ensure t
  :bind ("C-@" . er/expand-region))


;; Run command quickly
(leaf quickrun
  :ensure t
  :bind ("<f5>" . quickrun))


;; Restart emacs
(leaf restart-emacs
  :ensure t
  :bind ("C-x C-c" . restart-emacs))


;; Html editing
(leaf web-mode
  :ensure t
  :mode ("\\.js?\\'" "\\.html?\\'" "\\.php?\\'")
  :custom
  `((web-mode-markup-indent-offset . 2)
	(web-mode-css-indent-offset . 2)
	(web-mode-code-indent-offset . 2)))


;; automatically kill unnecessary buffers
(leaf tempbuf
  :el-get minorugh/tempbuf
  :config
  (add-hook 'dired-mode-hook 'turn-on-tempbuf-mode)
  (add-hook 'magit-mode-hook 'turn-on-tempbuf-mode))


;; undo redo
(leaf undo-fu
  :ensure t
  :bind (("C-_" . undo-fu-only-undo)
		 ("C-/" . undo-fu-only-redo)))


;; PS-printer
(defalias 'ps-mule-header-string-charsets 'ignore)
(setq ps-multibyte-buffer 'non-latin-printer
	  ps-paper-type 'a4
	  ps-font-size 9
	  ;; ps-font-family 'Helvetica
	  ps-font-family 'Courier
	  ps-line-number-font 'Courier
	  ps-printer-name nil
	  ps-print-header nil
	  ps-show-n-of-n t
	  ps-line-number t
	  ps-print-footer nil)


(when (require 'cua-base)
  (cua-mode 1)
  (setq cua-enable-cua-keys nil))


(provide '06_utils)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 06_utils.el ends here
