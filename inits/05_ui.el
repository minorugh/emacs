;;; 05_ui.el --- Graphical interface configurations. -*- lexical-binding: t; no-byte-compile: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Theme configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(leaf doom-themes
  :ensure t
  :hook (after-init-hook . (lambda () (load-theme 'doom-dracula t)))
  :custom
  (doom-themes-enable-italic . nil)
  (doom-themes-enable-bold . nil)
  :config
  (doom-themes-neotree-config)
  (doom-themes-org-config))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode-line
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(leaf doom-modeline
  :ensure t
  :hook (after-init-hook . doom-modeline-mode)
  :custom
  (doom-modeline-icon . t)
  (doom-modeline-major-mode-icon . nil)
  (doom-modeline-minor-modes . nil)
  :config
  (line-number-mode 0)
  (column-number-mode 0)
  (doom-modeline-def-modeline 'main
    '(bar window-number matches buffer-info remote-host buffer-position parrot selection-info)
    '(misc-info persp-name lsp github debug minor-modes input-method major-mode process vcs checker))
  :init
  (leaf nyan-mode
	:ensure t
	:config
	(nyan-mode 1)
	(nyan-start-animation)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Icons
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(leaf all-the-icons
  :ensure t
  :after doom-modeline
  :custom (all-the-icons-scale-factor . 0.9)
  :config
  (unless (member "all-the-icons" (font-family-list))
	(all-the-icons-install-fonts t)))

(leaf all-the-icons-dired
  :el-get jtbm37/all-the-icons-dired
  :after doom-modeline
  :hook (dired-mode-hook . all-the-icons-dired-mode))

(leaf all-the-icons-ivy-rich
  :ensure t
  :hook (after-init-hook . all-the-icons-ivy-rich-mode))

(leaf all-the-icons-ibuffer
  :ensure t
  :hook (ibuffer-mode-hook . all-the-icons-ibuffer-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Show line numbers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(leaf display-line-numbers
  :hook (lisp-interaction-mode-hook . my:disable-modes)
  :bind ("<f9>" . display-line-numbers-mode)
  :hook ((prog-mode-hook text-mode-hook) . display-line-numbers-mode)
  :init
  (setq display-line-numbers-width-start t)
  (defun my:disable-modes ()
	"Disable modes in scrtch buffer."
	(interactive)
	(display-line-numbers-mode 0)
	(flymake-mode 0)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Aggressive indent
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(leaf aggressive-indent
  :ensure t
  :hook ((emacs-lisp-mode-hook css-mode-hook) . aggressive-indent-mode))


(provide '05_ui)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 05_ui.el ends here
