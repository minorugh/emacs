;;; 00_base.el --- Default configurations.  -*- lexical-binding: t; no-byte-compile: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic configurations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(leaf *basic-configurations
  :hook (after-init-hook . my:default-mode)
  :custom
  `(;; Faster rendering by not corresponding to right-to-left language
    (bidi-display-reordering . nil)
    ;; Do not make a backup file like *.~
    (make-backup-files . nil)
    ;; Do not use auto save
    (auto-save-default . nil)
    (auto-save-list-file-prefix . nil)
    ;; Do not create lock file
    (create-lockfiles . nil)
    ;; Open symbolic link directly
    (vc-follow-symlinks . t)
    ;; Do not distinguish uppercase and lowercase letters on completion
    (completion-ignore-case . t)
    (read-file-name-completion-ignore-case . t)
    ;; Point keeps its screen position when scroll
    (scroll-preserve-screen-position . t)
    ;; All warning sounds and flash are invalid
    (ring-bell-function . 'ignore)
    ;; Turn off warning sound screen flash
    (visible-bell . nil)
    ;; Copy text with mouse range selection
    (mouse-drag-copy-region . t)
    ;; Deleted files go to the trash
    (delete-by-moving-to-trash . t)
    ;; Tab width default
    (tab-width . 4)
	;; Limit the final word to a line break code (automatically correct)
	(require-final-newline . t)
	;; Disallow adding new lines with newline at the end of the buffer
	(next-line-add-newlines . nil)
	;; Make it easy to see when it is the same name file
	(uniquify-buffer-name-style . 'post-forward-angle-brackets)
	;; Don't clear kill-ring when restart emacs
	(savehist-additional-variables . '(kill-ring))
	;; Use the X11 clipboard
	(select-enable-clipboard  . t))
  :init
  (defun my:linespacing ()
	(unless (minibufferp)
      (setq-local line-spacing 0.1)))
  (add-hook 'buffer-list-update-hook #'my:linespacing)
  ;; defalias
  (defalias 'exit 'save-buffers-kill-emacs)
  (defalias 'yes-or-no-p 'y-or-n-p)

  ;; Set default modes for startup hook
  (defun my:default-mode ()
	(interactive)
	(recentf-mode 1)
	(save-place-mode 1)
	(savehist-mode 1)
	(winner-mode 1)
	(global-auto-revert-mode 1)
	(global-font-lock-mode 1)
	(global-hl-line-mode 1)
	(global-visual-line-mode 1))

  ;; Save the file specified code with basic utf-8 if it exists
  (set-language-environment "Japanese")
  (prefer-coding-system 'utf-8)

  ;; Font
  (if (string-match "e590" (shell-command-to-string "uname -n"))
	  (add-to-list 'default-frame-alist '(font . "Cica-19.5"))
	;; For submachine
	(add-to-list 'default-frame-alist '(font . "Cica-15"))))

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
;; Custom configurations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(leaf *user-custom-configuration
  :bind (("C-x C-x" . my:exchange-point-and-mark)
		 ("M-w" . clipboard-kill-ring-save)
		 ("C-w" . my:kill-region)
		 ("s-c" . clipboard-kill-ring-save)	 ;; Like mac
		 ("s-v" . clipboard-yank)	 ;; Like mac
		 ("M-/" . kill-this-buffer)) ;; No inquiry
  :custom
  ;; change-default-file-location
  `((recentf-save-file . "~/.emacs.d/tmp/recentf")
	(save-place-file . "~/.emacs.d/tmp/places")
	(savehist-file . "~/.emacs.d/tmp/history")
	(url-configuration-directory . "~/.emacs.d/tmp/url")
	(bookmark-file . "~/.emacs.d/tmp/bookmarks"))
  :init
  ;; Control cursor blinking
  (setq blink-cursor-blinks 0)
  (setq blink-cursor-interval 0.3)
  (setq blink-cursor-delay 10)
  (add-hook 'emacs-startup-hook 'blink-cursor-mode)
  ;; Display buffer name in title bar
  (setq frame-title-format (format "emacs@%s : %%b" (system-name)))

  ;; Overwrite `C-w' to the whole-line-or-region
  (defun my:kill-region ()
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
	(emacs-lock-mode 'kill)))


(provide '00_base)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 00_base.el ends here
