;; core.el

;; Modifier key settings for macOS
(setq
 ns-command-modifier 'control
 ns-option-modifier 'meta
 ns-control-modifier 'super
 ns-function-modifier 'hyper)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile (require 'use-package))
(setq use-package-always-ensure t)

(when (memq window-system '(mac ns x))
  (use-package exec-path-from-shell
    :config
    (exec-path-from-shell-initialize)))

(provide 'core)
