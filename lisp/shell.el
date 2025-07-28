;; shell.el

(defun new-shell ()
  (interactive)
  (let ((currentbuf (get-buffer-window (current-buffer)))
        (newbuf (generate-new-buffer-name "*shell*")))
    (generate-new-buffer newbuf)
    (set-window-dedicated-p currentbuf nil)
    (set-window-buffer currentbuf newbuf)
    (shell newbuf)))

(global-set-key (kbd "M-s") 'new-shell)

(add-hook 'shell-mode-hook
          (lambda ()
            (setq comint-input-ring-file-name "~/.zsh_history")
            (comint-read-input-ring t)))

(provide 'shell)
