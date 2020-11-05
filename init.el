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

;;;;Org mode configuration
;; Enable Org mode
(require 'org)
;; Make Org mode work with files ending in .org
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
;; The above is the default in recent emacsen

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(lsp-mode ##)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

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
