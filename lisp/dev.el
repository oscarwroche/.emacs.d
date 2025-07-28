;; dev.el

(use-package company
  :hook (prog-mode . company-mode)
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0)
  (company-tooltip-align-annotations t))

(use-package flyspell
  :custom (ispell-program-name "/opt/homebrew/bin/aspell")
  :hook (flyspell-mode . flyspell-buffer))

(use-package magit
  :bind (("C-x g" . magit-status))
  :custom
  (magit-save-repository-buffers nil)
  (magit-list-refs-sortby "-creatordate"))

(use-package which-key
  :config (which-key-mode))

(use-package yasnippet
  :config (yas-global-mode 1))

(use-package web-mode
  :mode ("\\.js\\'" "\\.mjs\\'" "\\.cjs\\'" "\\.jsx\\'" "\\.ts\\'" "\\.tsx\\'")
  :custom (web-mode-enable-auto-quoting nil)
  :hook ((web-mode . prettier-js-mode)
         (web-mode . add-node-modules-path)))

(use-package prettier-js)

(use-package add-node-modules-path)

(use-package lsp-mode
  :hook (prog-mode . lsp-deferred)
  :custom
  (lsp-idle-delay 0.5)
  (lsp-log-io nil)
  (lsp-enable-snippet nil)
  (lsp-enable-symbol-highlighting nil)
  (gc-cons-threshold 100000000)
  (read-process-output-max (* 1024 1024)))

(use-package lsp-ui)

(provide 'dev)
