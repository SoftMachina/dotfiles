(add-to-list 'custom-theme-load-path "~/.config/emacs/themes/")

(load-theme 'tomorrow-night-paradise t)

(menu-bar-mode -1)
(tool-bar-mode -1)

(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode t)

(setq inhibit-startup-message t) 
(setq initial-scratch-message nil)

;; Set up package.el to work with MELPA
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(package-refresh-contents)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Download Evil
(unless (package-installed-p 'evil)
  (package-install 'evil))

;; Enable Evil
(require 'evil)
(evil-mode 1)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(company evil flycheck fzf lsp-mode lsp-ui projectile web-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "s-l") ;; optional
  :hook ((c-mode
          java-mode
          kotlin-mode
          c++-mode
	  web-mode
	  typescript-mode
          ) . lsp)
  :commands lsp)

(use-package projectile
  :init (projectile-mode +1)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (setq projectile-project-search-path '("~/git/" "~/.config/" "~/programming/")))
(use-package company
  :hook (after-init . global-company-mode)
  :config
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.0)) ;; show completions immediately
(use-package lsp-ui
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-delay 0.5
	lsp-ui-doc-use-childframe t
        lsp-ui-doc-position 'at-point
        lsp-ui-sideline-enable nil
        lsp-ui-sideline-show-diagnostics t
        lsp-ui-sideline-show-hover t))
(define-key evil-normal-state-map (kbd "K") #'lsp-ui-doc-glance)

(use-package flycheck
  :init (global-flycheck-mode))

;; Prevent *compilation* buffer from opening in a new window
(add-to-list 'display-buffer-alist
             '("\\*compilation\\*"
               (display-buffer-reuse-window display-buffer-at-bottom)
               (window-height . 0.3)))

(defun compile-and-run ()
  "Compile and run the current C++ file with a default output name based on the buffer name."
  (interactive)
  (let* ((filename (file-name-base (buffer-name)))  ;; removes extension
         (output-name (read-string "Output binary name: " filename))
         (compile-command (concat "g++ " (buffer-name) " -o " output-name " && ./" output-name)))
    (compile compile-command)))

(global-set-key (kbd "C-c r") 'compile-and-run)

(use-package fzf
  :bind (("C-c f f" . fzf))
  :config
  (setq fzf/args "-x --color bw --print-query --margin=1,0 --no-hscroll"
        fzf/executable "fzf"
        fzf/git-grep-args "-i --line-number %s"
        fzf/grep-command "grep -nrH"
        fzf/position-bottom t
        fzf/window-height 15))

(use-package web-mode
  :mode (("\\.js\\'" . web-mode)
         ("\\.jsx\\'" . web-mode)
         ("\\.ts\\'" . web-mode)
         ("\\.tsx\\'" . web-mode)
         ("\\.vue\\'" . web-mode))
  :config
  (setq web-mode-enable-auto-quoting nil)) ;; optional, prevents unwanted quote insertion


(defun my/lsp-apply-first-code-action ()
  "Apply the first available LSP code action."
  (interactive)
  (let ((actions (lsp-code-actions-at-point)))
    (when actions
      (lsp--execute-code-action (car actions)))))

(global-set-key (kbd "C-c a") #'my/lsp-apply-first-code-action)

(defun my/lsp-format-buffer-on-save ()
  (add-hook 'before-save-hook #'lsp-format-buffer nil t))

(add-hook 'lsp-mode-hook #'my/lsp-format-buffer-on-save)
