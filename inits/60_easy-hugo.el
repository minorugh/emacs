;;; 60_easy-hugo.el --- Easy-Hugo configurations. -*- lexical-binding: t; no-byte-compile: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Easy-Hugo configurations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(leaf easy-hugo
  :ensure t
  :bind (("C-c C-e" . easy-hugo)
		 ("C-x p" . easy-hugo-preview)
		 ("C-x P" . easy-hugo-publish)
		 (:easy-hugo-mode-map
		  ([tab] . easy-hugo-no-help)
		  ("<return>" . easy-hugo-view)
		  ("o" . easy-hugo-open-basedir)
		  ("r" . easy-hugo-rename)
		  ("e" . my:edit-easy-hugo)))
  :config
  ;; Sort-publishday on startup
  (setq easy-hugo--sort-char-flg nil)
  (setq easy-hugo--sort-time-flg nil)
  (setq easy-hugo--sort-publishday-flg 1)
  :init
  ;; Main blog (=blog1)
  (setq easy-hugo-basedir "~/Dropbox/minorugh/snap/")
  (setq easy-hugo-url "https://snap.minorugh.com")
  (setq easy-hugo-sshdomain "xsrv")
  (setq easy-hugo-root "/home/minorugh/minorugh.com/public_html/snap/")
  (setq easy-hugo-previewtime "300")
  ;; Bloglist
  (setq easy-hugo-bloglist
		'(;; blog2 setting
		  ((easy-hugo-basedir . "~/Dropbox/GH/gg/")
		   (easy-hugo-url . "https://gg.gospel-haiku.com")
		   (easy-hugo-sshdomain . "xsrv")
		   (easy-hugo-root . "/home/minorugh/gospel-haiku.com/public_html/gg/"))
		  ;; blog3 setting
		  ((easy-hugo-basedir . "~/Dropbox/GH/blog/")
		   (easy-hugo-url . "https://blog.gospel-haiku.com")
		   (easy-hugo-sshdomain . "xsrv")
		   (easy-hugo-root . "/home/minorugh/gospel-haiku.com/public_html/blog/"))
		  ;; blog4 setting
		  ((easy-hugo-basedir . "~/Dropbox/GH/es/")
		   (easy-hugo-url . "https://es.gospel-haiku.com")
		   (easy-hugo-sshdomain . "xsrv")
		   (easy-hugo-root . "/home/minorugh/gospel-haiku.com/public_html/es/"))
		  ;; blog5 setting
		  ((easy-hugo-basedir . "~/Dropbox/minorugh/bible/")
		   (easy-hugo-url . "https://bible.minorugh.com")
		   (easy-hugo-sshdomain . "xsrv")
		   (easy-hugo-root . "/home/minorugh/minorugh.com/public_html/bible/"))
		  ;; blog6 setting
		  ((easy-hugo-basedir . "~/Dropbox/minorugh/tube/")
		   (easy-hugo-url . "https://tube.minorugh.com")
		   (easy-hugo-sshdomain . "xsrv")
		   (easy-hugo-root . "/home/minorugh/minorugh.com/public_html/tube/"))
		  ;; blog7 setting
		  ((easy-hugo-basedir . "~/Dropbox/minorugh/ryo/")
		   (easy-hugo-url . "https://ryo.minorugh.com")
		   (easy-hugo-sshdomain . "xsrv")
		   (easy-hugo-root . "/home/minorugh/minorugh.com/public_html/ryo/"))))

  ;; Customize for my help menu
  (setq easy-hugo-help-line 4
		easy-hugo-help "
  n .. New blog post    r .. Rename file     p .. Preview          g .. Refresh
  d .. Delete post      a .. Search blog ag  P .. Publish clever   e .. Edit easy-hugo
  c .. Open config      s .. Sort time       < .. Previous blog    > .. Next bloge
  N .. No help [tab]    / .. Select postdir  o .. Open base dir    v .. View window
")
  :preface
  (leaf popup :ensure t)
  (leaf request	:ensure t
	:custom (request-storage-directory . "~/.emacs.d/tmp/request"))

  (defun my:edit-easy-hugo ()
	"Edit setting file for 'easy-hugo'."
	(interactive)
	(find-file "~/.emacs.d/inits/60_easy-hugo.el")
	(forword-line 2)))


(provide '60_easy-hugo)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 60_easy-hugo.el ends here
