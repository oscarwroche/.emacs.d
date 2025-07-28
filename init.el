;; init.el

(add-to-list 'load-path "~/.emacs.d/lisp")

;; Disable UI noise early
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-message t)

;; Setup package and use-package
(require 'core)

;; Load modules
(require 'ui)
(require 'org-config)
(require 'dev)
(require 'langs)
(require 'shell)
(require 'keybinds)
(require 'completion)
(require 'gpt)

;; Open GTD file on startup
(find-file "~/Dropbox/org/gtd.org")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-done ((t (:foreground "gray50" :weight normal :strike-through t))))
 '(org-level-1 ((t (:foreground "LightSkyBlue" :weight bold :height 1.3))))
 '(org-level-2 ((t (:foreground "LightSteelBlue" :weight bold :height 1.2))))
 '(org-level-3 ((t (:foreground "LightGoldenrod" :weight bold :height 1.1))))
 '(org-todo ((t (:foreground "orange" :weight bold)))))
