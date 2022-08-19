;;; 00_base.el --- Default configurations.  -*- lexical-binding: t; no-byte-compile: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic configurations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Faster rendering by not corresponding to right-to-left language
(setq bidi-display-reordering nil)

;; Do not make a backup file like *.~
(setq make-backup-files nil)

;; Do not use auto save
(setq auto-save-default nil)
(setq auto-save-list-file-prefix nil)

;; Do not create lock file
(setq create-lockfiles nil)

;; Open symbolic link directly
(setq vc-follow-symlinks t)

;; Do not distinguish uppercase and lowercase letters on completion
(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

;; Point keeps its screen position when scroll
(setq scroll-preserve-screen-position t)

;; All warning sounds and flash are invalid
(setq ring-bell-function 'ignore)

;; Turn off warning sound screen flash
(setq visible-bell nil)

;; Copy text with mouse range selection
(setq mouse-drag-copy-region t)

;; Deleted files go to the trash
(setq delete-by-moving-to-trash t)

;; Tab width default
(setq tab-width 4)

;; Limit the final word to a line break code (automatically correct)
(setq require-final-newline t)

;; Disallow adding new lines with newline at the end of the buffer
(setq next-line-add-newlines nil)

;; Make it easy to see when it is the same name file
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; Don't clear kill-ring when restart emacs
(setq savehist-additional-variables '(kill-ring))

;; Use the X11 clipboard
(setq select-enable-clipboard  t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cunstom configurations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my:linespacing ()
  "Set line spacing."
  (unless (minibufferp)
    (setq-local line-spacing 0.1)))
(add-hook 'buffer-list-update-hook #'my:linespacing)
;; defalias
(defalias 'exit 'save-buffers-kill-emacs)
(defalias 'yes-or-no-p 'y-or-n-p)

;; Set default modes for startup hook
(defun my:default-mode-hook ()
  "Set defult modes."
  (interactive)
  (recentf-mode 1)
  (save-place-mode 1)
  (savehist-mode 1)
  (winner-mode 1)
  (global-auto-revert-mode 1)
  (global-font-lock-mode 1)
  (global-hl-line-mode 1)
  (global-visual-line-mode 1))
(add-hook 'after-init-hook 'my:default-mode-hook)

;; Save the file specified code with basic utf-8 if it exists
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)

;; Font
(if (string-match "e590" (shell-command-to-string "uname -n"))
	(add-to-list 'default-frame-alist '(font . "Cica-19.5"))
  ;; For submachine
  (add-to-list 'default-frame-alist '(font . "Cica-15")))

;; Server start for emacs-client
(leaf server
  :require t
  :config
  (unless (server-running-p)
    (server-start)))

;; exec-path-from-shell
(leaf exec-path-from-shell
  :ensure t
  :when (memq window-system '(mac ns x))
  :hook (after-init-hook . exec-path-from-shell-initialize)
  :custom (exec-path-from-shell-check-startup-files . nil))

;; recentf
(leaf recentf
  :custom
  `((recentf-auto-cleanup . 'never)
    (recentf-exclude
     . '("\\.howm-keys" "Dropbox/backup" ".emacs.d/tmp/" ".emacs.d/elpa/" "/scp:"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(bind-key "C-x C-x" 'my:exchange-point-and-mark)
(bind-key "M-w" 'clipboard-kill-ring-save)
(bind-key "C-w" 'my:kill-whole-line-or-region)
(bind-key "s-c" 'clipboard-kill-ring-save)	 ;; Like mac
(bind-key "s-v" 'clipboard-yank)	 ;; Like mac
(bind-key "M-/" 'kill-this-buffer) ;; No inquiry

;; change-default-file-location
(setq recentf-save-file "~/.emacs.d/tmp/recentf")
(setq save-place-file "~/.emacs.d/tmp/places")
(setq savehist-file "~/.emacs.d/tmp/history")
(setq url-configuration-directory "~/.emacs.d/tmp/url")
(setq bookmark-file "~/.emacs.d/tmp/bookmarks")

;; Control cursor blinking
(setq blink-cursor-blinks 0)
(setq blink-cursor-interval 0.3)
(setq blink-cursor-delay 10)
(add-hook 'emacs-startup-hook 'blink-cursor-mode)

;; Display buffer name in title bar
(setq frame-title-format (format "emacs@%s : %%b" (system-name)))

;; Overwrite `C-w' to the whole-line-or-region
(defun my:kill-whole-line-or-region ()
  "If the region is active, to kill region.
If the region is inactive, to kill whole line."
  (interactive)
  (if (use-region-p)
	  (clipboard-kill-region (region-beginning) (region-end))
    (kill-whole-line)))

(defun my:exchange-point-and-mark ()
  "No mark active `exchange-point-and-mark'."
  (interactive)
  (exchange-point-and-mark)
  (deactivate-mark))

;; Set buffer that can not be killed
(with-current-buffer "*scratch*"
  (emacs-lock-mode 'kill))
(with-current-buffer "*Messages*"
  (emacs-lock-mode 'kill))


(provide '00_base)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 00_base.el ends here
