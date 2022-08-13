;;; 90_selected.el --- Selected configurations. -*- lexical-binding: t; no-byte-compile: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Selected configurations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(leaf selected
  :ensure t
  :hook ((after-init-hook . selected-global-mode)
		 (activate-mark-hook . my:activate-selected)
		 (activate-mark-hook . (lambda () (setq my:ime-flag current-input-method) (my:ime-off)))
		 (deactivate-mark-hook . (lambda () (unless (null my:ime-flag) (my:ime-on)))))
  :bind (:selected-keymap
		 (";" . comment-dwim)
		 ("c" . clipboard-kill-ring-save)
		 ("s" . swiper-thing-at-point)
		 ("t" . google-translate-auto)
		 ("T" . chromium-translate)
		 ("W" . my:weblio)
		 ("k" . my:koujien)
		 ("e" . my:eijiro)
		 ("g" . my:google))
  :init
  (defvar my:ime-flag nil)
  (defun my:activate-selected ()
	"Active selected."
	(selected-global-mode 1)
	(selected--on)
	(remove-hook 'activate-mark-hook #'my:activate-selected))

  (defun my:ime-on ()
	"IME on."
	(interactive)
	(when (null current-input-method) (toggle-input-method)))

  (defun my:ime-off ()
	"IME off."
	(interactive)
	(deactivate-input-method))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; user-dictionary-configurations
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(with-eval-after-load 'selected
  (defun my:google (str)
	(interactive (list (my:get-region nil)))
	(browse-url (format "https://www.google.com/search?hl=ja&q=%s"
						(upcase (url-hexify-string str)))))

  (defun my:koujien (str)
	"Open koujien with STR."
	(interactive (list (my:get-region nil)))
	(browse-url (format "https://sakura-paris.org/dict/広辞苑/prefix/%s"
						(upcase (url-hexify-string str)))))

  (defun my:weblio (str)
	"Open weblio with STR."
	(interactive (list (my:get-region nil)))
	(browse-url (format "https://www.weblio.jp/content/%s"
						(upcase (url-hexify-string str)))))

  (defun my:eijiro (str)
	"Open eijiro with STR."
	(interactive (list (my:get-region nil)))
	(browse-url (format "https://eow.alc.co.jp/%s/UTF-8/"
						(upcase (url-hexify-string str)))))

  (defun my:get-region (r)
	"Get search word from region with R."
	(buffer-substring-no-properties (region-beginning) (region-end))))


(provide '90_selected)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 90_selected.el ends here
