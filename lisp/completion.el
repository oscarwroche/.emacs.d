;; completion.el

(use-package vertico
  :init
  (vertico-mode))

(use-package savehist
  :init
  (savehist-mode)) ;; saves minibuffer history

(use-package orderless
  :custom
  (completion-styles '(orderless)
                     completion-category-defaults nil))

;; Optional: show documentation popups while completing (M-x, etc.)
(use-package marginalia
  :after vertico
  :init (marginalia-mode))

(provide 'completion)
