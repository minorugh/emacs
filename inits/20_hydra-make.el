;;; 20_hydra-make.el --- Make command configurations. -*- lexical-binding: t; no-byte-compile:t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make command configurations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(leaf *user-make-configulation
  :mode (("\\.mak\\'" "makefile\\'") . makefile-mode)
  :hydra
  (hydra-make
   (:hint nil :exit t)
   "
     make:_k_  _u_psftp  _m_ove  _d_raft  _b_klog  _g_it  _s_ort  _c_lean  🐾
"
   ("k" my:make-k)
   ("u" my:make-upsftp)
   ("m" my:make-move)
   ("d" my:make-draft)
   ("b" my:make-bklog)
   ("g" my:make-git)
   ("s" my:make-sort)
   ("c" my:make-clean)
   ("<muhenkan>" nil))
  :init
  (setq compilation-always-kill t)
  (setq compilation-finish-functions 'compile-autoclose)

  (defun compile-autoclose (buffer string)
	"Automatically close the compilation buffer."
	(cond ((string-match "finished" string)
		   (bury-buffer "*compilation*")
		   (delete-other-windows)
		   (message "Build successful."))
		  (t (message "Compilation exited abnormally: %s" string)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; My make command functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my:make-k ()
  "Make k."
  (interactive)
  (compile "make -k"))

(defun my:make-draft ()
  "Make kinnei draft."
  (interactive)
  (compile "make df"))

(defun my:make-upsftp ()
  "Make upfstp."
  (interactive)
  (compile "make up"))

(defun my:make-move ()
  "Make move."
  (interactive)
  (compile "make mv"))

(defun my:make-bklog ()
  "Make bklog."
  (interactive)
  (compile "make bk"))

(defun my:make-git ()
  "Make git."
  (interactive)
  (compile "make git"))

(defun my:make-sort ()
  "Make sort for filelist."
  (interactive)
  (compile "make sort")
  (find-file "~/Dropbox/GH/upsftp/filelist.txt")
  (goto-char (point-min)))

(defun my:make-clean ()
  "Make clean."
  (interactive)
  (compile "make clean"))


(provide '20_hydra-make)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 20_hydra-make.el ends here
