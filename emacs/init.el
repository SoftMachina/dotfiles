;; Package setup - moved to top and optimized
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))

;; Initialize packages but don't refresh contents automatically
(package-initialize)

;; Only refresh package contents if use-package is not installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Theme and UI setup
(add-to-list 'custom-theme-load-path "~/.config/emacs/themes/")
(load-theme 'tomorrow-night-paradise t)

(menu-bar-mode -1)
(tool-bar-mode -1)

(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode t)

(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; Evil mode setup
(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

;; Projectile
(use-package projectile
  :ensure t
  :init (projectile-mode +1)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (setq projectile-project-search-path '("~/git/" "~/.config/" "~/programming/")))

;; Company mode
(use-package company
  :ensure t
  :hook (after-init . global-company-mode)
  :config
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.0))

;; Flycheck
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;; LSP Mode
(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((c-mode
          java-mode
          kotlin-mode
          c++-mode
          web-mode
          typescript-mode
          ) . lsp)
  :commands lsp
  :config
  (setq lsp-auto-guess-root t))

;; LSP UI
(use-package lsp-ui
  :ensure t
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

;; Key bindings - moved after package definitions
(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "K") #'lsp-ui-doc-glance)
  (define-key evil-normal-state-map (kbd "C-v r") #'lsp-rename))

;; FZF
(use-package fzf
  :ensure t
  :bind (("C-c f f" . fzf))
  :config
  (setq fzf/args "-x --color bw --print-query --margin=1,0 --no-hscroll"
        fzf/executable "fzf"
        fzf/git-grep-args "-i --line-number %s"
        fzf/grep-command "grep -nrH"
        fzf/position-bottom t
        fzf/window-height 15))

;; Web mode
(use-package web-mode
  :ensure t
  :mode (("\\.js\\'" . web-mode)
         ("\\.jsx\\'" . web-mode)
         ("\\.ts\\'" . web-mode)
         ("\\.tsx\\'" . web-mode)
         ("\\.vue\\'" . web-mode))
  :config
  (setq web-mode-enable-auto-quoting nil))

;; Prevent *compilation* buffer from opening in a new window
(add-to-list 'display-buffer-alist
             '("\\*compilation\\*"
               (display-buffer-reuse-window display-buffer-at-bottom)
               (window-height . 0.3)))

;; Custom functions
(defun compile-and-run ()
  "Compile and run the current C++ file with a default output name based on the buffer name."
  (interactive)
  (let* ((filename (file-name-base (buffer-name)))
         (output-name (read-string "Output binary name: " filename))
         (compile-command (concat "g++ " (buffer-name) " -o " output-name " && ./" output-name)))
    (compile compile-command)))


(defun compile-no-run ()
  (interactive)
  (let* ((filename (file-name-base (buffer-name)))
         (output-name (read-string "Output binary name: " filename))
         (compile-command (concat "g++ " (buffer-name) " -o " output-name)))
    (compile compile-command)))

(defun lsp-apply-code-action()
  "Apply the first available LSP code action."
  (interactive)
  (let ((actions (lsp-code-actions-at-point)))
    (when actions
      (lsp--execute-code-action (car actions)))))

(defun lsp-format-on-save ()
  (add-hook 'before-save-hook #'lsp-format-buffer nil t))

;; Key bindings
(global-set-key (kbd "C-c r") 'compile-and-run)
(global-set-key (kbd "C-x c") 'compile-no-run)
(global-set-key (kbd "C-c a") #'lsp-apply-code-action)

;; Hooks
(add-hook 'lsp-mode-hook #'lsp-format-on-save)

;; Custom variables (moved to end)
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
