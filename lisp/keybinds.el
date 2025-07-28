;; keybinds.el

(global-set-key (kbd "M-n") (lambda () (interactive) (insert "~")))
(global-set-key (kbd "M-(") (lambda () (interactive) (insert "{")))
(global-set-key (kbd "M-)") (lambda () (interactive) (insert "}")))
(global-set-key (kbd "M-5") (lambda () (interactive) (insert "[")))
(global-set-key [?\M-\°] (lambda () (interactive) (insert "]")))
(global-set-key (kbd "M-L") (lambda () (interactive) (insert "|")))
(global-set-key (kbd "M-S") (lambda () (interactive) (insert "\\")))

(global-set-key (kbd "M-TAB") 'comint-previous-matching-input-from-input)
(global-set-key (kbd "M-S-TAB") 'comint-next-matching-input-from-input)

(provide 'keybinds)
