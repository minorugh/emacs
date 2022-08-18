;;; 10_custom.el --- User custom functions. -*- lexical-binding: t; no-byte-compile: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User custom functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(leaf *user-custom-functions
  :bind (("<f3>" . thunar-open)
		 ("<f4>" . terminal-open)
		 ("<f6>" . counsel-linux-app)
		 ("<f8>" . toggle-menu-bar-mode-from-frame)
		 ("<muhenkan>" . minibuffer-keyboard-quit)
		 ("S-<return>" . toggle-scratch))
  :init
  (defun thunar-open ()
	"Open thunar with current dir."
	(interactive)
	(shell-command (concat "xdg-open " default-directory)))

  (defun terminal-open ()
	"Open termninal with current dir."
	(interactive)
	(let ((dir (directory-file-name default-directory)))
	  ;; (shell-command (concat "gnome-terminal --maximize --working-directory " dir))))
	  (shell-command (concat "gnome-terminal --working-directory " dir))))

  (defun toggle-scratch ()
	"Toggle current buffer and *scratch* buffer."
	(interactive)
	(if (not (string= "*scratch*" (buffer-name)))
		(progn
		  (setq toggle-scratch-prev-buffer (buffer-name))
		  (switch-to-buffer "*scratch*"))
	  (switch-to-buffer toggle-scratch-prev-buffer)))

  (defun my:delete-file-if-no-contents ()
	"If the file is empty, it will be deleted automatically."
	(when (and (buffer-file-name (current-buffer))
			   (= (point-min) (point-max)))
	  (delete-file
	   (buffer-file-name (current-buffer)))))
  (if (not (memq 'my:delete-file-if-no-contents after-save-hook))
      (setq after-save-hook
			(cons 'my:delete-file-if-no-contents after-save-hook)))

  (defun my:delete-this-file ()
	"Delete the current file, and kill the buffer."
	(interactive)
	(unless (buffer-file-name)
	  (error "No file is currently being edited"))
	(when (yes-or-no-p (format "Really delete '%s'?"
							   (file-name-nondirectory buffer-file-name)))
	  (delete-file (buffer-file-name))
	  (kill-this-buffer)))

  ;; Automatically open root permission file with sudo
  ;; https://ameblo.jp/grennarthmurmand1976/entry-12151018656.html
  (defun file-root-p (filename)
	"Return t if file FILENAME created by root."
	(eq 0 (nth 2 (file-attributes filename))))

  (defadvice find-file (around my:find-file activate)
	"Open FILENAME using tramp's sudo method if it's root permission."
	(if (and (file-root-p (ad-get-arg 0))
			 (not (file-writable-p (ad-get-arg 0)))
			 (y-or-n-p (concat (ad-get-arg 0)
							   " is root permission. Open it as root? ")))
		(my:find-file-sudo (ad-get-arg 0))
	  ad-do-it))

  (defun my:find-file-sudo (file)
	"Opens FILE with root privileges."
	(interactive "F")
	(set-buffer (find-file (concat "/sudo::" file)))))


(provide '10_custom)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 10_custom.el ends here
