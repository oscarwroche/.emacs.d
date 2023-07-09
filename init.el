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
 '(lsp-treemacs-error-list-current-project-only t)
 '(package-selected-packages
   '(graphql-mode eglot sqlformat lsp-tailwindcss rustic lsp-ui treemacs-tab-bar which-key dap-mode helm-xref prettier-js solarized-theme xref-js2 js2-refactor js2-mode company-solidity solidity-mode docker-compose-mode dockerfile-mode terraform-mode web-mode web company tide typescript-mode magit json-mode nix-mode haskell-mode shell-pop geiser exec-path-from-shell lsp-mode))
 '(treemacs-no-delete-other-windows nil)
 '(warning-suppress-types
   '(((defvaralias losing-value rustic-indent-offset))
     (lsp-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#181a26" :foreground "gray80" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 130 :width normal :foundry "nil" :family "Source Code Pro")))))

(when (cl-find-if-not #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (mapc #'package-install package-selected-packages))

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
(setq ispell-program-name "/opt/homebrew/bin/aspell")

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

;; Web mode

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mjs\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.cjs\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))

;; Themes, fonts etc ...

;;(load-theme 'solarized-light t)
;;(load-theme 'solarized-dark t)
;;(run-at-time "20:00" (* 60 60 24) (lambda () (enable-theme 'solarized-dark)))
;;(run-at-time "08:00" (* 60 60 24) (lambda () (enable-theme 'solarized-light)))
(load-theme 'deeper-blue t)

;; Set default font
(set-face-attribute 'default nil
                    :family "Source Code Pro"
                    :height 150
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

;;(add-hook 'web-mode-hook 'prettier-js-mode)

(defun enable-minor-mode (my-pair)
  "Enable minor mode if filename match the regexp.  MY-PAIR is a cons cell (regexp . minor-mode)."
  (if (buffer-file-name)
      (if (string-match (car my-pair) buffer-file-name)
	  (funcall (cdr my-pair)))))

(add-hook 'web-mode-hook #'(lambda ()
                            (enable-minor-mode
                             '("\\.jsx?\\'" . prettier-js-mode))))

(add-hook 'web-mode-hook #'(lambda ()
                            (enable-minor-mode
                             '("\\.tsx?\\'" . prettier-js-mode))))

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

;; Magit

(setq magit-save-repository-buffers nil)

(define-derived-mode cairo-mode prog-mode "cairo"
        "Major mode for editing cairo files."
)

(add-to-list 'auto-mode-alist '("\\.cairo$" . cairo-mode))

;; LSP

(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq company-minimum-prefix-length 1
      company-idle-delay 0.0) ;; default is 0.2
;;(setq lsp-diagnostics-provider :none)
(add-hook 'web-mode-hook #'lsp)
;;(add-hook 'fundamental-mode-hook #'lsp)
(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (yas-global-mode)
  (add-to-list 'lsp-language-id-configuration '(cairo-mode . "cairo"))
  )
(lsp-treemacs-sync-mode 1)
(setq lsp-rust-server 'rust-analyzer)

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection "node /Users/oscarroche/node_modules/cairo-ls/out/server.js --stdio")
                  :activation-fn (lsp-activate-on "cairo")
                  :server-id 'cairo-ls))

(setq lsp-tailwindcss-add-on-mode t)

(setq lsp-enable-snippet nil)

;; Flycheck error display

(add-to-list 'display-buffer-alist
             `(,(rx bos "*Flycheck errors*" eos)
              (display-buffer-reuse-window
               display-buffer-in-side-window)
              (side            . bottom)
              (reusable-frames . visible)
              (window-height   . 0.33)))

;; Rust

(setq rustic-format-trigger 'on-save)
(setq rustic-rustfmt-args "+nightly")

;; SQL

(setq sqlformat-command 'pgformatter)
(setq sqlformat-args '("-s2" "-g"))

;; Eglot + C++

(require 'eglot)
(add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)

;; Web mode

(setq web-mode-enable-auto-quoting nil)

;; Open gtd on launch

(find-file "~/Dropbox/org/gtd.org")
