;;; 09_mozc.el --- Japanese mozc configurations. -*- lexical-binding: t; no-byte-compile: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Japanese mozc configurations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(leaf mozc
  :ensure t
  :hook (after-init-hook . mozc-mode)
  :bind ("<hiragana-katakana>" . toggle-input-method)
  (:mozc-mode-map
   ("," . (lambda () (interactive) (mozc-insert-str "、")))
   ("." . (lambda () (interactive) (mozc-insert-str "。")))
   ("?" . (lambda () (interactive) (mozc-insert-str "？")))
   ("!" . (lambda () (interactive) (mozc-insert-str "！"))))
  :custom `((default-input-method . "japanese-mozc")
			(mozc-helper-program-name . "mozc_emacs_helper")
			(mozc-leim-title . "あ"))
  :init
  (defadvice toggle-input-method (around toggle-input-method-around activate)
	"Input method function in key-chord.el not to be nil."
	(let ((input-method-function-save input-method-function))
	  ad-do-it
	  (setq input-method-function input-method-function-save)))

  (defun mozc-insert-str (str)
	"STR Immediately confirmed by punctuation."
	(interactive)
	(mozc-handle-event 'enter)
	(insert str))

  (leaf mozc-cursor-color
	:el-get minorugh/mozc-cursor-color
	:hook (after-init-hook . mozc-cursor-color-setup))

  (leaf mozc-cand-posframe
	:ensure t :after mozc :require t
	:custom	(mozc-candidate-style . 'posframe)
	:init
	(leaf posframe :ensure t)))


;; Add space between full-width and half-width
(leaf pangu-spacing
  :ensure t :after mozc
  :hook ((markdown-mode-hook text-mode-hook) . pangu-spacing-mode)
  :config
  (setq pangu-spacing-include-regexp
		(rx (or (and (or (group-n 3 (any "。，！？；：「」（）、"))
						 (group-n 1 (or (category japanese))))))
			(group-n 2 (in "a-zA-Z")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom configurations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(leaf *cus-mozc-tool
  :bind (("s-t" . my:mozc-dictionary-tool)
		 ("s-d" . my:mozc-word-regist)
		 ;; ("s-h" . my:mozc-hand-writing)
		 ("s-h" . chromium-tegaki))
  :init
  (defun select-mozc-tool ()
	"Select mozc-tool."
	(interactive)
	(counsel-M-x "my:mozc- "))

  (defun my:mozc-config-dialog ()
	"Open `mozc-config-dialog'."
	(interactive)
	(compile "/usr/lib/mozc/mozc_tool --mode=config_dialog")
	(delete-other-windows))

  (defun my:mozc-dictionary-tool ()
	"Open `mozc-dictipnary-tool'."
	(interactive)
	(compile "/usr/lib/mozc/mozc_tool --mode=dictionary_tool")
	(delete-other-windows))

  (defun my:mozc-word-regist ()
	"Open `mozc-word-regist'."
	(interactive)
	(compile "/usr/lib/mozc/mozc_tool --mode=word_register_dialog")
	(delete-other-windows))

  (defun my:mozc-hand-writing ()
	"Open `mozc-hand-writing'."
	(interactive)
	(compile "/usr/lib/mozc/mozc_tool --mode=hand_writing")
	(delete-other-windows))

  (defun chromium-tegaki ()
	"Chromium tegaki site."
	(interactive)
	(browse-url "https://mojinavi.com/tegaki")))


(provide '09_mozc)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 09_mozc.el ends here
