;;; 50_open-junk-file.el --- open junk file mode configurations. -*- lexical-binding: t; no-byte-compile: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; open-junk-file configurations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(leaf open-junk-file
  :ensure t
  :custom  `((open-junk-file-format . "~/Dropbox/howm/junk/%Y%m%d.")
			 (open-junk-file-find-file-function . 'find-file)))


;; Open last created disposable file in one shot
;; https://qiita.com/zonkyy/items/eba6bc64f66d278f0032
(leaf em-glob
  :require t
  :config
  (defvar junk-file-dir "~/Dropbox/howm/junk/")
  (defun open-last-junk-file ()
	"Open last created junk-file."
	(interactive)
	(find-file
	 (car
	  (last (eshell-extended-glob
			 (concat
			  (file-name-as-directory junk-file-dir)
			  "*.*.*")))))))


(provide '50_open-junk-file)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 50_open-junk-file.el ends here

