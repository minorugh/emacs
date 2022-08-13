;;; 99_my:dired.el --- My dired configurations. -*- lexical-binding: t; no-byte-compile: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; my dired configurations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my:dotfiles-dir ()
  "Open dotfile dir."
  (interactive)
  (setq dired-listing-switches "-lgGhFA")
  (find-file "~/src/github.com/minorugh/dotfiles/"))

(defun my:root-dir ()
  "Open root dir."
  (interactive)
  (setq dired-listing-switches "-lgGhFA")
  (find-file "/"))

(defun my:emacs.d-dir ()
  "Open root dir."
  (interactive)
  (setq dired-listing-switches "-lgGhFA")
  (find-file "src/github.com/minorugh/emacs.d/"))

(defun my:github.io-dir ()
  "Open root dir."
  (interactive)
  (setq dired-listing-switches "-lgGhFA")
  (find-file "src/github.com/minorugh/minorugh.github.io/"))

(defun my:scr-dir ()
  "Open scr dir."
  (interactive)
  (setq dired-listing-switches "-lgGhFA")
  (find-file "~/src/"))

(defun my:junk-dir ()
  "Open junk file dir."
  (interactive)
  (find-file "~/Dropbox/howm/junk/"))

(defun my:home-dir ()
  "Open hoge dir."
  (interactive)
  (find-file "~/"))

(defun my:dropbox ()
  "Open dropbox dir."
  (interactive)
  (find-file "~/Dropbox/"))

(defun my:xsrv-dir ()
  "Open xsrv dir."
  (interactive)
  (find-file "~/Dropbox/xsrv/"))

(defun my:emacs-dir ()
  "Open `.emacs.d' dir."
  (interactive)
  (setq dired-listing-switches "-lgGhFA")
  (find-file "~/src/github.com/minorugh/dotfiles/.emacs.d/"))

(defun my:gh-dir ()
  "Open GH dir."
  (interactive)
  (find-file "~/Dropbox/GH/"))

(defun my:inits-dir ()
  "Open inits dir."
  (interactive)
  (find-file "~/src/github.com/minorugh/dotfiles/.emacs.d/inits/"))

(defun my:org-dir ()
  "Open org dir."
  (interactive)
  (find-file "~/Dropbox/org/"))

(defun mozc-dir ()
  "Open mozc dir."
  (interactive)
  (setq dired-listing-switches "-lgGhFA")
  (find-file "~/Dropbox/mozc/.mozc/"))

(defun my:open-capture ()
  "Open `org-capture' file."
  (interactive)
  (find-file "~/Dropbox/org/capture.org")
  (goto-char (point-min)))

(defun my:diary ()
  "Open diary file."
  (interactive)
  (find-file "~/Dropbox/GH/dia/diary.txt")
  (goto-char (point-min)))

(defun my:d_kukai ()
  "Open d_select file."
  (interactive)
  (find-file "~/Dropbox/GH/d_select/tex/minoru_sen.txt")
  (goto-char (point-min)))

(defun my:w_kukai ()
  "Open w_select file."
  (interactive)
  (find-file "~/Dropbox/GH/w_select/tex/minoru_sen.txt")
  (goto-char (point-min)))

(defun my:m_kukai ()
  "Open m_select file."
  (interactive)
  (find-file "~/Dropbox/GH/m_select/tex/mkukai.txt")
  (goto-char (point-min)))

(defun my:swan ()
  "Open s_select file."
  (interactive)
  (find-file "~/Dropbox/GH/swan/tex/swan.txt")
  (goto-char (point-min)))

(defun my:teirei ()
  "Open teirei file."
  (interactive)
  (find-file "~/Dropbox/GH/teirei/tex/teirei.txt")
  (goto-char (point-min)))

(defun my:kinnei ()
  "Open kinnei file."
  (interactive)
  (find-file "~/Dropbox/GH/kinnei/kinnei.txt")
  (goto-char (point-min)))

(defun my:kinnei-draft ()
  "Open kinnei draft file."
  (interactive)
  (find-file "~/Dropbox/GH/kinnei/draft.txt"))

(defun my:apvoice ()
  "Open apvoice file."
  (interactive)
  (find-file "~/Dropbox/GH/apvoice/apvoice.txt")
  (goto-char (point-min)))


(provide '99_my:dired)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 99_my:dired.el ends here
