;;; 07_dimmer.el --- Dimmer configurations. -*- lexical-binding: t; no-byte-compile: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Visually highlight the selected buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(leaf dimmer
  :ensure t
  :chord (".." . my:toggle-dimmer)
  :config
  (defvar my:dimmer-mode 1)
  (setq dimmer-buffer-exclusion-regexps '("^ \\*which-key\\|^ \\*LV\\|^ \\*.*posframe.*buffer.*\\*$"))
  (setq dimmer-fraction 0.6)

  (defun my:toggle-dimmer ()
	(interactive)
	(unless (one-window-p)
	  (if (setq my:dimmer-mode (not my:dimmer-mode))
		  (dimmer-on) (dimmer-off))))

  (defun dimmer-off ()
	(dimmer-process-all)
	(dimmer-mode -1))

  (defun dimmer-on ()
	(when my:dimmer-mode
	  (dimmer-mode 1)
	  (dimmer-process-all))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Split window configuration with dimmer control
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(leaf *sprit-window-configurations
  :bind (("C-q" . other-window-or-split)
		 ("C-x 2" . my:split-window-below)
		 ("C-x 1" . my:delete-other-windows)
		 ("C-x 0" . my:delete-window)
		 ("<C-return>" . window-swap-states)
		 ("<next>" . my:scroll-other-window)
		 ("<prior>" . my:scroll-other-window-down))
  :init
  (defun other-window-or-split ()
	"With turn on dimmer."
	(interactive)
	(when (one-window-p)
	  (split-window-horizontally)
	  (follow-mode 1)
	  (dimmer-mode 1))
	(other-window 1))

  (defun my:split-window-below ()
	"With turn on dimmer."
	(interactive)
	(split-window-below)
	(follow-mode 1)
	(dimmer-mode 1))

  (defun my:delete-window ()
	"With turn off dimmer."
	(interactive)
	(delete-window)
	(follow-mode -1)
	(dimmer-mode -1))

  (defun my:delete-other-windows ()
	"With turn off dimmer."
	(interactive)
	(delete-other-windows)
	(follow-mode -1)
	(dimmer-mode -1))

  (defun kill-other-buffers ()
	"Kill all other buffers."
	(interactive)
	(mapc 'kill-buffer (delq (current-buffer) (buffer-list)))
	(message "killl-other-buffers!"))

  (defun my:scroll-other-window ()
	"If there are two windows, `scroll-other-window'."
	(interactive)
	(when (one-window-p)
	  (scroll-up))
	(scroll-other-window))

  (defun my:scroll-other-window-down ()
	"If there are two windows, `scroll-other-window-down'."
	(interactive)
	(when (one-window-p)
	  (scroll-down))
	(scroll-other-window-down)))


(provide '07_dimmer)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 07_dimmer.el ends here
