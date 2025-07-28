;; ui.el

(use-package solarized-theme
  ;; You can switch themes if you prefer another
  :config
  ;; (load-theme 'solarized-light t)
  (load-theme 'deeper-blue t))

(set-face-attribute 'default nil :font "Fira Code" :height 130)

(global-display-line-numbers-mode 1)
(global-font-lock-mode 1)

;; Org custom faces
(custom-set-faces
 '(org-level-1 ((t (:foreground "LightSkyBlue" :weight bold :height 1.3))))
 '(org-level-2 ((t (:foreground "LightSteelBlue" :weight bold :height 1.2))))
 '(org-level-3 ((t (:foreground "LightGoldenrod" :weight bold :height 1.1))))
 '(org-todo ((t (:foreground "orange" :weight bold))))
 '(org-done ((t (:foreground "gray50" :weight normal :strike-through t)))))

(provide 'ui)
