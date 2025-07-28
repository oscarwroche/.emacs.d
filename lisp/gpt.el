;; gpt.el

(use-package gptel
  :ensure t
  :custom
  (gptel-api-key (auth-source-pick-first-password :host "api.openai.com")) ;; or your API key directly
  (gptel-model "gpt-4") ;; or "gpt-3.5-turbo"
  :commands (gptel gptel-send gptel-menu)
  :bind (("C-c g" . gptel)))

(provide 'gpt)
