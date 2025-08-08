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

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

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
          latex-mode
          LaTeX-mode
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

(use-package pdf-tools
  :ensure t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (pdf-tools-install)  ;; Compile and enable native PDF support
  (setq-default pdf-view-display-size 'fit-page)
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward))

(setq revert-without-query '(".*\\.pdf$"))
(setq auto-revert-use-notify t)

;; AUCTeX - Load this first before LSP configuration
(use-package auctex
  :ensure t
  :defer t
  :hook (LaTeX-mode . turn-on-reftex)
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  ;; Enable synctex for forward/inverse search
  (setq TeX-source-correlate-mode t)
  (setq TeX-source-correlate-start-server t)
  ;; Use built-in doc-view for PDF viewing (more stable)
  (setq TeX-view-program-selection '((output-pdf "Emacs"))
        TeX-view-program-list '(("Emacs" "%V"))))

;; LaTeX LSP support with texlab - Configure after AUCTeX
(with-eval-after-load 'lsp-mode
  (when (executable-find "texlab")
    (add-to-list 'lsp-language-id-configuration '(latex-mode . "latex"))
    (add-to-list 'lsp-language-id-configuration '(LaTeX-mode . "latex"))
    (lsp-register-client
     (make-lsp-client
      :new-connection (lsp-stdio-connection "texlab")
      :major-modes '(latex-mode LaTeX-mode)
      :server-id 'texlab))))


;; Company AUCTeX for LaTeX autocompletion
(use-package company-auctex
  :ensure t
  :after (company auctex)
  :config
  (company-auctex-init))

;; RefTeX for references and citations
(use-package reftex
  :ensure t
  :hook (LaTeX-mode . turn-on-reftex)
  :config
  (setq reftex-plug-into-AUCTeX t))

;; FZF
(use-package fzf
  :ensure t
  :bind (("C-c f" . fzf))
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

(defun compile-make()
(interactive)
  (let* ((default-directory (or (project-root (project-current)) default-directory))
         (makefile-found (or (file-exists-p "Makefile") (file-exists-p "makefile")))
         (target (read-string "Make target (default: all): ")))
    (if makefile-found
        (let ((command (if (string-empty-p target)
                           "make all"
                         (concat "make " target))))
          (compile command))
      (message "No Makefile found in current directory."))))

(defun compile-build ()
  (interactive)
  (let* ((filename (file-name-base (buffer-name)))
         (output-name (read-string "Output binary name: " filename))
         (compile-command (concat "g++ " (buffer-name) " -o " output-name)))
    (compile compile-command)))

(defun compile-run ()
  "Compile and run the current C++ file with a default output name based on the buffer name."
  (interactive)
  (let* ((filename (file-name-base (buffer-name)))
         (output-name (read-string "Output binary name: " filename))
         (compile-command (concat "g++ " (buffer-name) " -o " output-name " && ./" output-name)))
    (compile compile-command)))

(defun lsp-apply-code-action()
  "Apply the first available LSP code action."
  (interactive)
  (let ((actions (lsp-code-actions-at-point)))
    (when actions
      (lsp--execute-code-action (car actions)))))

(defun lsp-format-on-save ()
  (add-hook 'before-save-hook #'lsp-format-buffer nil t))

(defun latex-view ()
  "Open the compiled PDF in a new buffer inside Emacs using pdf-tools."
  (interactive)
  (let* ((tex-file (buffer-file-name))
         (pdf-file (concat (file-name-sans-extension tex-file) ".pdf")))
    (if (file-exists-p pdf-file)
        (find-file-other-window pdf-file)
      (message "PDF file not found."))))

;; Wrap the selected region in \\ruby{...}{} and place cursor in second {}.
;; Works with Evil visual mode.
(defun my/evil-ruby-wrap-visual (beg end)
  (interactive "r")
  (when (bound-and-true-p evil-mode)
    (let ((text (buffer-substring-no-properties beg end)))
      (delete-region beg end)
      (insert (format "\\ruby{%s}{}" text))
      (search-backward "{}")
      (forward-char 1)
      (evil-insert-state))))

;; Window management functions
(defun evil-window-decrease-height ()
  (interactive)
  (shrink-window 10))

(defun evil-window-increase-height ()
  (interactive)
  (enlarge-window 10))

(defun evil-window-decrease-width ()
  (interactive)
  (shrink-window-horizontally 10))

(defun evil-window-increase-width ()
  (interactive)
  (enlarge-window-horizontally 10))

;; Evil quit/save functions
(defun my/evil-quit-or-save ()
  "If buffer is modified, ask to save. Then kill the buffer."
  (interactive)
  (if (and (buffer-modified-p)
           (y-or-n-p (format "Buffer %s modified. Save? " (buffer-name))))
      (save-buffer))
  (kill-this-buffer))

(defun my/evil-write-quit ()
  "Save buffer and kill it."
  (interactive)
  (save-buffer)
  (kill-this-buffer))

;; Key bindings - moved after package definitions
(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "K") #'lsp-ui-doc-glance)
  (define-key evil-normal-state-map (kbd "C-v r") #'lsp-rename)
  ;; Window management key bindings
  (define-key evil-window-map (kbd "C-h") 'evil-window-decrease-width)
  (define-key evil-window-map (kbd "C-l") 'evil-window-increase-width)
  (define-key evil-window-map (kbd "C-j") 'evil-window-decrease-height)
  (define-key evil-window-map (kbd "C-k") 'evil-window-increase-height)

  ;; Evil ex commands
  (evil-ex-define-cmd "q" 'my/evil-quit-or-save)
  (evil-ex-define-cmd "wq" 'my/evil-write-quit))

;; Global key bindings TODO: think of better ones!!
(global-set-key (kbd "C-c b") 'compile-build)
(global-set-key (kbd "C-c m") 'compile-make)
(global-set-key (kbd "C-c c r") 'compile-run)
(global-set-key (kbd "C-c a") #'lsp-apply-code-action)
(global-set-key (kbd "C-c t") 'latex-view)
(global-set-key (kbd "C-c e") 'view-echo-area-messages)
(global-set-key (kbd "C-c k") #'my/evil-ruby-wrap-visual)


;; Hooks
(add-hook 'lsp-mode-hook #'lsp-format-on-save)

;; Custom variables (moved to end)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(auctex company-auctex reftex evil-collection company evil flycheck fzf lsp-mode lsp-ui projectile web-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
