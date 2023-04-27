;;; init.el --- Valtteri's personal Emacs configuration:

;;; Commentary:

;; Aiming to have everything defined in this file.  The goal is to
;; stay near to vanilla Emacs and add minimal set of extra packages
;; and configuration based on a) need and b) personal preference.

;;; Code:

;;; Package management ;;;

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;; We install `use-package` using `package`. All subsequent packages
;; will be installed using `use-package`.
(dolist (package '(use-package))
  (unless (package-installed-p package)
    (package-refresh-contents)
    (package-install package)))

;;; Theme and appearance ;;;

(load-theme 'tango-dark)
(set-face-attribute 'region nil :background "green4")
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(setq blink-cursor-blinks -1) ; blink forever

(use-package unicode-fonts
   :ensure t
   :config
    (unicode-fonts-setup))

;;; Custom functions ;;;

;; Copied from https://github.com/bbatsov/crux/blob/master/crux.el
(defun crux-transpose-windows (arg)
  "Transpose the buffers shown in two windows.
Prefix ARG determines if the current windows buffer is swapped
with the next or previous window, and the number of
transpositions to execute in sequence."
  (interactive "p")
  (let ((this-win (selected-window))
        (this-buffer (window-buffer)))
    (other-window arg)
    (set-window-buffer this-win (current-buffer))
    (set-window-buffer (selected-window) this-buffer)))

(defun cmd* (cmd errbuf)
  "Call CMD interactively on active region and print errors to ERRBUF."
  (shell-command-on-region
   (region-beginning)
   (region-end)
   cmd
   (current-buffer)
   t
   errbuf
   t))

(defun jet-pretty-edn ()
  "Pretty print edn on active region."
  (interactive)
  (cmd*
   "jet --pretty --edn-reader-opts '{:default tagged-literal}'"
   "*jet error buffer*"))

(defun jet-json->pretty-edn ()
  "Transform JSON on active region to pretty printed EDN."
  (interactive)
  (cmd*
   "jet --from json --keywordize --pretty --edn-reader-opts '{:default tagged-literal}'"
   "*jet error buffer*"))

(defun jet-edn->pretty-json ()
  "Transform EDN on active region to pretty printed JSON."
  (interactive)
  (cmd*
   "jet --to json --pretty --edn-reader-opts '{:default tagged-literal}'"
   "*jet error buffer*"))

;; Workaround to "too many open files"
(defun file-notify-rm-all-watches ()
  "Remove all existing file notification watches from Emacs."
  (interactive)
  (maphash
   (lambda (key _value)
     (file-notify-rm-watch key))
   file-notify-descriptors))

;;; Global configs and keybindings ;;;

;; Never tabs, always spaces
(setq-default indent-tabs-mode nil)

(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)
(setq mac-function-modifier 'hyper)
(setq ns-function-modifier 'hyper)
(setq ns-right-alternate-modifier nil)

(global-set-key (kbd "C-s-n") (lambda () (interactive) (forward-line 5)))
(global-set-key (kbd "C-s-p") (lambda () (interactive) (forward-line -5)))
(global-set-key (kbd "s-n") nil)
(global-set-key (kbd "C-x C-o") 'crux-transpose-windows)
(global-set-key (kbd "C-x M-o") 'previous-window-any-frame)
(global-set-key (kbd "C-c C-l") 'xwidget-webkit-browse-url)

;; Linear undo & redo (emacs28)
(global-set-key (kbd "C-_") 'undo-only)
(global-set-key (kbd "M-_") 'undo-redo)

(show-paren-mode t)
(delete-selection-mode t)
(projectile-mode t)
(add-hook 'before-save-hook 'whitespace-cleanup)

;; Store all backup and autosave files in the tmp dir
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(use-package expand-region
  :ensure t
  :bind (("C-M-SPC" . er/expand-region)
         ("C-M-<backspace>" . er/contract-region)))

(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  :config
  (setq vertico-sort-function 'vertico-sort-alpha)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  )

(use-package marginalia
  :ensure t
  :bind
  (:map minibuffer-local-map
        ("M-A" . marginalia-cycle))
  :init (marginalia-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package flyspell
  :ensure t
  :bind
  (("C-M-;" . flyspell-correct-word-before-point))
  :hook
  ((prog-mode . flyspell-prog-mode)
   (org-mode . flyspell-mode)
   (markdown-mode . flyspell-mode)))

(use-package langtool
  :ensure t
  :bind
  (("C-c ge" . 'langtool-check)
   ("C-c gd" . 'langtool-check-done)
   ("C-c gp" . 'langtool-show-message-at-point)
   ("C-c gr" . 'langtool-correct-buffer))
  :config
  (setq langtool-http-server-host "localhost")
  (setq langtool-http-server-port 8981)
  (setq langtool-default-language "en-US"))

;;; Global utils ;;;

(use-package rg
  :ensure t)

(use-package projectile
  :ensure t
  :bind-keymap ("C-c p" . projectile-command-map))

(use-package magit
  :ensure t)

(use-package code-review
  :ensure t)

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package smartparens
  :ensure t
  :bind
  (("C-<right>" . 'sp-forward-slurp-sexp)
   ("C-<left>" . 'sp-forward-barf-sexp))
  :config
  (require 'smartparens-config)
  (smartparens-global-strict-mode))

(use-package company
  :ensure t
  :config
  (global-company-mode t))

(use-package rainbow-mode
  :ensure t)

;;; Org ;;;

(use-package org
  :ensure t
  :custom
  (org-agenda-files '("~/org"))
  (org-default-notes-file "~/org/memo.org")
  :config
  (global-set-key (kbd "C-c l") 'org-store-link)
  (global-set-key (kbd "C-c a") 'org-agenda)
  (global-set-key (kbd "C-c c") 'org-capture))

;;; JSON ;;;

;; Flycheck uses json-python-json with this by default on OSX
(use-package json-mode
  :ensure t
  :mode "\\.json\\'"
  :custom
  (json-reformat:indent-width 2)
  (js-indent-level 2))

;;; YAML mode ;;;

;; Flycheck uses yaml-ruby with this by default on OSX
(use-package yaml-mode
  :ensure t
  :mode
  ("\\.yml\\'" . 'yaml-mode)
  ("\\.yaml\\'" . 'yaml-mode))

;;; Cloudformation ;;;

(use-package flycheck-cfn
  :ensure t
  :config
  (flycheck-cfn-setup))

(use-package cfn-mode
  :ensure t
  :config
  (flycheck-add-next-checker 'cfn-lint 'cfn-nag))

;; Patched version because the original from flycheck-cfn didn't work
;; as of 2021-08-18
(defun flycheck-cfn-parse-cfn-nag (output checker buffer)
  "Parse cfn-nag errors from JSON OUTPUT.

Parse cfn-nag OUTPUT for cfn-nag CHECKER on a given BUFFER"
  (seq-mapcat (lambda (violation)
             (let-alist violation
               (seq-map (lambda (linenum)
                          (flycheck-error-new-at
                           linenum
                           nil
                           (if (equal .type "WARN") 'warning 'error)
                           .message
                           :id .id
                           :checker checker
                           :filename (buffer-file-name buffer)))
                        .line_numbers)))
           (seq-filter
            'listp
            (car
             (seq-map (lambda (msg)
                        (alist-get 'violations
                                   (alist-get 'file_results msg)))
                      (car (flycheck-parse-json output)))))))

;;; Terraform ;;;

(use-package terraform-mode
  :ensure t)

;;; Clojure ;;;

(use-package flycheck-clj-kondo
  :ensure t)

(use-package clojure-mode
  :ensure t
  :config
  (require 'flycheck-clj-kondo))

(use-package cider
  :ensure t
  :config
  (setq cider-clojure-cli-global-options "-J-XX:-OmitStackTraceInFastThrow"))

(use-package html-to-hiccup
  :ensure t
  :config
  (define-key clojure-mode-map (kbd "H-h") 'html-to-hiccup-convert-region))

(use-package neil
  :ensure t
  :config
  (setq neil-prompt-for-version-p nil
        neil-inject-dep-to-project-p t))

(defun find-definition ()
  "Try to find definition of cursor via LSP otherwise fallback to cider."
  (interactive)
  (let ((cursor (point))
        (buffer (current-buffer)))
    (lsp-find-definition)
    (when (and (eq buffer (current-buffer))
               (eq cursor (point)))
      (cider-find-var))))

(define-key clojure-mode-map (kbd "M-.") #'find-definition)
(define-key cider-mode-map (kbd "M-.") #'find-definition)
(define-key clojurec-mode-map (kbd "M-.") #'find-definition)
(define-key clojurescript-mode-map (kbd "M-.") #'find-definition)

;; See also section 'LSP'

;;; Markdown ;;;

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode
  (("README\\.md\\'" . gfm-mode)
   ("\\.md\\'" . markdown-mode)
   ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;;; JavaScript ;;;

(use-package js2-mode
  :ensure t
  :mode ("\\.js\\'" . 'js2-mode))

(use-package prettier-js
  :ensure t
  :hook
  ((js2-mode . prettier-js-mode)
   (web-mode . prettier-js-mode)))

;;; Python ;;;

(use-package lsp-pyright
  :ensure t
  :hook
  (python-mode . (lambda () (require 'lsp-pyright) (lsp))))

(use-package python-black
  :ensure t
  :after python
  :hook (python-mode . python-black-on-save-mode-enable-dwim))

;;; Web ;;;

(use-package web-mode
  :ensure t)

;;; Forth ;;;
(use-package forth-mode
  :ensure t)

;;; LSP ;;;

(use-package lsp-mode
  :ensure t
  :hook
  ((clojure-mode . lsp)
   (clojurec-mode . lsp)
   (clojurescript-mode . lsp))
  :bind-keymap ("H-l" . lsp-command-map)
  :config
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-enable-indentation nil)
  (dolist (m '(clojure-mode
               clojurec-mode
               clojurescript-mode
               clojurex-mode))
     (add-to-list 'lsp-language-id-configuration `(,m . "clojure"))))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

(provide 'init.el)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(rg terraform-mode python-black lsp-pyright web-mode vertico use-package unicode-fonts smartparens rainbow-mode projectile prettier-js php-mode orderless neil marginalia lsp-ui json-mode js2-mode html-to-hiccup forth-mode flycheck-clj-kondo flycheck-cfn expand-region company code-review cfn-mode))
 '(safe-local-variable-values '((cider-shadow-cljs-default-options . "app"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
