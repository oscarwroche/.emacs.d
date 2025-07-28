;; org-config.el

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :hook ((org-mode . turn-on-flyspell))
  :custom
  (org-log-done t)
  (org-log-note-clock-out t)
  (org-agenda-log-mode-items '(closed clock state))
  (org-agenda-files '("~/Dropbox/org/gtd.org" "~/Dropbox/org/tickler.org"))
  (org-capture-templates
   '(("t" "Todo [inbox]" entry
      (file+headline "~/Dropbox/org/inbox.org" "Tasks")
      "* TODO %i%?")
     ("T" "Tickler" entry
      (file+headline "~/Dropbox/org/tickler.org" "Tickler")
      "* TODO %i%?")))
  (org-refile-targets
   '(("~/Dropbox/org/gtd.org" :maxlevel . 3)
     ("~/Dropbox/org/someday.org" :level . 1)
     ("~/Dropbox/org/tickler.org" :maxlevel . 2)))
  (org-refile-use-outline-path 'file)
  (org-todo-keyword-faces
   '(("TODO" . org-warning)
     ("NEXT" . "blue")
     ("DOING" . "orange")
     ("CANCELLED" . (:foreground "purple" :weight bold))
     ("DONE" . "green")))
  :config
  (add-to-list 'org-modules 'org-habit)
  (define-key global-map (kbd "C-c l") 'org-store-link)
  (define-key global-map (kbd "C-c a") 'org-agenda)
  (define-key global-map (kbd "C-c c") 'org-capture)
  (add-hook 'auto-save-hook 'org-save-all-org-buffers))

(provide 'org-config)
