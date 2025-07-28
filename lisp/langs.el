;; langs.el

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration '(emacs-lisp-mode . nil)))

(use-package go-mode
  :hook (go-mode . lsp-deferred))

(use-package rustic
  :custom (rustic-format-trigger 'on-save))

(use-package solidity-mode)
(use-package company-solidity)

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package geiser
  :custom
  (geiser-mit-binary "/usr/local/bin/scheme")
  (geiser-active-implementations '(mit)))

;; Cairo (manual LSP client)
(use-package cairo-mode
  :mode "\\.cairo\\'"
  :config
  (add-to-list 'lsp-language-id-configuration '(cairo-mode . "cairo"))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "scarb-cairo-language-server")
                    :activation-fn (lsp-activate-on "cairo")
                    :server-id 'scarb-cairo-language-server)))

(provide 'langs)
