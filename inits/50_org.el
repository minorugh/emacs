;;; 50_org.el --- Org mode configurations. -*- lexical-binding: t; no-byte-compile: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org configurations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(leaf org
  :hook (emacs-startup-hook . (lambda () (require 'org-protocol)))
  :chord (",," . org-capture)
  :bind (("C-c a" . org-agenda)
		 ("C-c c" . org-capture)
		 ("C-c k" . org-capture-kill)
		 ("C-c o" . org-open-at-point)
		 ("C-c i" . org-edit-src-exit)
		 (:org-mode-map
		  ("C-c i" . org-edit-special)))
  :custom `((org-log-done . 'org)
			(timep-use-speed-commands . t)
			(org-src-fontify-natively . t)
			(org-startup-indented . t)
			(org-hide-leading-stars . t)
			(org-startup-folded . 'content)
			(org-indent-mode-turns-on-hiding-stars . nil)
			(org-indent-indentation-per-level . 4)
			(org-startup-folded . 'content)
			(org-agenda-files . '("~/Dropbox/org/task.org"))
			(org-agenda-span . 30))
  :config
  (defun my:howm-create-file ()
    "Make howm create file with 'org-capture'."
    (interactive)
    (format-time-string "~/Dropbox/howm/%Y/%m/%Y%m%d%H%M.md" (current-time)))
  ;; Caputure Settings
  (setq org-capture-templates
		'(("m" " Memo with howm" plain (file my:howm-create-file)
		   "# memo: %?\n%U %i")
		  ("n" " Note with howm" plain (file my:howm-create-file)
		   "# note: %?\n%U %i")
		  ("t" " Task" entry (file+headline "~/Dropbox/org/task.org" "TASK")
		   "** TODO %?\n SCHEDULED: %^t \n" :empty-lines 1 :jump-to-captured 1)
		  ("e" " Experiment" entry (file+headline "~/Dropbox/org/experiment.org" "Experiment")
		   "* %? %i\n#+BEGIN_SRC perl\n\n#+END_SRC\n\n%U")
		  ("p" " Code capture" entry (file+headline "~/Dropbox/org/capture.org" "Code")
		   "* %^{Title} \nSOURCE: %:link\nCAPTURED: %U\n\n#+BEGIN_SRC\n%i\n#+END_SRC\n" :prepend t)
		  ("L" " Link capture" entry (file+headline "~/Dropbox/org/capture.org" "Link")
		   "* [[%:link][%:description]] \nCAPTURED: %U\nREMARKS: %?" :prepend t)))
  (setq org-refile-targets
		'(("~/Dropbox/org/archives.org" :level . 1)
		  ("~/Dropbox/org/remember.org" :level . 1)
		  ("~/Dropbox/org/task.org" :level . 1)))
  :init
  ;; Maximize the org-capture buffer
  (defvar my:org-capture-before-config nil
    "Window configuration before 'org-capture'.")
  (defadvice org-capture (before save-config activate)
    "Save the window configuration before 'org-capture'."
    (setq my:org-capture-before-config (current-window-configuration)))
  (add-hook 'org-capture-mode-hook 'delete-other-windows))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Calendar configurations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(leaf calendar
  :leaf-defer t
  :bind (("<f7>" . calendar)
		 (:calendar-mode-map
		  ("<f7>" . calendar-exit)))
  :config
  (leaf japanese-holidays
	:ensure t
	:require t
	:hook ((calendar-today-visible-hook . japanese-holiday-mark-weekend)
		   (calendar-today-invisible-hook . japanese-holiday-mark-weekend)
		   (calendar-today-visible-hook . calendar-mark-today))
	:config
	(setq calendar-holidays
		  (append japanese-holidays holiday-local-holidays holiday-other-holidays))
	(setq calendar-mark-holidays-flag t)))


(provide '50_org)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 50_org.el ends here
