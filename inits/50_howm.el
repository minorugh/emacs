;;; 50_howm.el --- howm mode configurations. -*- lexical-binding: t; no-byte-compile: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Howm configurations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(leaf howm
  :ensure t
  :hook (after-init-hook . howm-mode)
  :bind (:howm-view-summary-mode-map
		 ("<return>" . my:howm-summary-view-mode-open)
		 ([backtab] . howm-view-summary-previous-section))
  :init
  (setq howm-view-title-header "#"
		howm-directory "~/Dropbox/howm"
		howm-file-name-format "%Y/%m/%Y%m%d%H%M.md")
  :custom `((howm-view-split-horizontally . t)
			(howm-view-summary-persistent . nil)
			(howm-normalizer . 'howm-sort-items-by-reverse-date)
			(howm-user-font-lock-keywords
			 . '(("memo:" . (0 'compilation-error))
				 ("note:" . (0 'compilation-info))
				 ("tech" . (0 'font-lock-constant-face)))))
  :config
  (defun my:howm-summary-view-mode-open ()
	(interactive)
	(howm-view-summary-open)
	(view-mode 1)))


(provide '50_howm)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 50_howm.el ends here
