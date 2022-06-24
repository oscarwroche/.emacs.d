(setq
   ns-command-modifier 'control
   ns-option-modifier 'meta
   ns-control-modifier 'super
   ns-function-modifier 'hyper)

;; -*- mode: elisp -*-

;; Disable the splash screen (to enable it agin, replace the t with 0)
(setq inhibit-splash-screen t)

;; Enable transient mark mode
(transient-mark-mode 1)

;; Org-mode

;; Enable Org mode
(require 'org)
;; Make Org mode work with files ending in .org
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
;; The above is the default in recent emacsen
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(setq org-agenda-files (list "~/Dropbox/org/gtd.org" "~/Dropbox/org/tickler.org"))
(setq org-agenda-log-mode-items '(closed clock state))
(global-set-key (kbd "C-c c") 'org-capture)
(add-hook 'org-mode-hook 'turn-on-flyspell)
(add-hook 'auto-save-hook 'org-save-all-org-buffers)
(setq org-log-note-clock-out t)
(setq org-capture-templates '(("t" "Todo [inbox]" entry
                               (file+headline "~/Dropbox/org/inbox.org" "Tasks")
                               "* TODO %i%?")
                              ("T" "Tickler" entry
                               (file+headline "~/Dropbox/org/tickler.org" "Tickler")
                               "* TODO %i%?")))
(setq org-refile-targets '(("~/Dropbox/org/gtd.org" :maxlevel . 3)
                           ("~/Dropbox/org/someday.org" :level . 1)
                           ("~/Dropbox/org/tickler.org" :maxlevel . 2)))
(setq org-refile-use-outline-path 'file)
(setq org-todo-keyword-faces
      '(("TODO" . org-warning) ("NEXT" . "blue") ("DOING" . "orange") ("CANCELLED" . (:foreground "purple" :weight bold)) ("DONE" . "green")))
(add-to-list 'org-modules 'org-habit)

;; Packages

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(solarized-theme xref-js2 js2-refactor js2-mode company-solidity solidity-mode docker-compose-mode dockerfile-mode terraform-mode web-mode web company tide typescript-mode magit json-mode nix-mode haskell-mode shell-pop geiser exec-path-from-shell lsp-mode ##)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Special character shortcuts

(defun tilde () (interactive) (insert "~"))
(global-set-key "\M-n" 'tilde)

(defun leftcurl () (interactive) (insert "{"))
(global-set-key "\M-(" 'leftcurl)

(defun rightcurl () (interactive) (insert "}"))
(global-set-key "\M-)" 'rightcurl)

(defun leftbrack () (interactive) (insert "["))
(global-set-key "\M-5" 'leftbrack)

(defun rightbrack () (interactive) (insert "]"))
(global-set-key [?\M-\°] 'rightbrack)

(defun pipe () (interactive) (insert "|"))
(global-set-key (kbd "M-L") 'pipe)

(defun backslash () (interactive) (insert "\\"))
(global-set-key (kbd "M-S") 'backslash)

(cd "/users/oscarroche/")
(setq ispell-program-name "/usr/local/bin/aspell")

(require 'cc-mode)

(ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(setq geiser-mit-binary "/usr/local/bin/scheme")
(setq geiser-active-implementations '(mit))

(add-hook 'flyspell-mode-hook #'flyspell-buffer)

(require 'nix-mode)
(add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;;; New shell shortcut

(defun new-shell ()
  (interactive)

  (let (
        (currentbuf (get-buffer-window (current-buffer)))
        (newbuf     (generate-new-buffer-name "*shell*"))
       )

   (generate-new-buffer newbuf)
   (set-window-dedicated-p currentbuf nil)
   (set-window-buffer currentbuf newbuf)
   (shell newbuf)
  )
  )

(define-key global-map "\M-s" 'new-shell)

;; Typescript

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (setq tide-format-options '(:tabSize 4 :indentSize 4))
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (setq tide-completion-enable-autoimport-suggestions t)
  (company-mode +1)
  )

;; Web mode

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))
(setq web-mode-enable-auto-quoting nil)
(add-hook 'before-save-hook 'tide-format-before-save)
(add-hook 'typescript-mode-hook #'setup-tide-mode)
;;(flycheck-add-mode 'typescript-tslint 'web-mode)

;;(add-hook 'tide-mode-hook 'display-line-numbers-mode)

;; Themes, fonts etc ...

(load-theme 'solarized-light t)

;; Set default font
(set-face-attribute 'default nil
                    :family "Source Code Pro"
                    :height 165
                    :weight 'normal
                    :width 'normal)

;; Set history to zsh history

(add-hook 'shell-mode-hook 'my-shell-mode-hook)
(defun my-shell-mode-hook ()
  (setq comint-input-ring-file-name "~/.zsh_history")
  (comint-read-input-ring t))

;; Rebind "history starting with ..."

(global-set-key (kbd "M-TAB") 'comint-previous-matching-input-from-input)
(global-set-key (kbd "M-S-TAB") 'comint-next-matching-input-from-input)

;; Case-sensitive autocomplete

(setq ac-ignore-case nil)
(setq dabbrev-case-fold-search nil)

;; Show line numbers

(global-display-line-numbers-mode)

;; Solidity

(require 'solidity-mode)
(require 'company-solidity)

;; JS

(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
(add-hook 'js2-mode-hook #'js2-refactor-mode)
(js2r-add-keybindings-with-prefix "C-c C-r")
(define-key js2-mode-map (kbd "C-k") #'js2r-kill)

;; js-mode (which js2 is based on) binds "M-." which conflicts with xref, so
;; unbind it.
(define-key js-mode-map (kbd "M-.") nil)

(add-hook 'js2-mode-hook (lambda ()
			   (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))

;; Remove directories from grep

(eval-after-load 'grep
  '(progn
     (add-to-list 'grep-find-ignored-directories "tmp")
     (add-to-list 'grep-find-ignored-directories "node_modules")
     (add-to-list 'grep-find-ignored-directories ".bundle")
     (add-to-list 'grep-find-ignored-directories "auto")
     (add-to-list 'grep-find-ignored-directories "elpa")
     (add-to-list 'grep-find-ignored-files "*.tsbuildinfo"))
)

;; Open gtd on launch

(find-file "~/Dropbox/org/gtd.org")
