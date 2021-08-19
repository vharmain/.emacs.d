;;; init.el --- Valtteri's personal Emacs configuration:

;;; Commentary:

;; Aiming to have everything defined in this file.  The goal is to
;; stay near to vanilla Emacs and add minimal set of extra packages
;; and configuration based on a) need and b) personal preference.

;;; Junk that needs to be installed outside Emacs

;; # Cloudformation
;; brew install ruby brew-gem cfn-lint
;; brew gem install cfn-nag
;;
;; # Clojure
;; brew install clojure/tools/clojure
;; brew install borkdude/brew/clj-kondo
;; brew install clojure-lsp/brew/clojure-lsp-native
;; 
;; # Javascript
;; brew install node npm
;; npm install -g eslint prettier
;;
;; # PHP
;; brew install php composer
;; npm i intelephense -g

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

;;; Global configs and keybindings ;;;

;; Never tabs, always spaces
(setq-default indent-tabs-mode nil)

(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)
(setq ns-right-alternate-modifier nil)

(global-set-key (kbd "C-s-n") (lambda () (interactive) (forward-line 5)))
(global-set-key (kbd "C-s-p") (lambda () (interactive) (forward-line -5)))
(global-set-key (kbd "s-n") nil)
(global-set-key (kbd "C-x C-o") 'crux-transpose-windows)

;; Linear undo & redo (emacs28)
(global-set-key (kbd "C-_") 'undo-only)
(global-set-key (kbd "M-_") 'undo-redo)

;;; Global utils ;;;

(use-package projectile
  :ensure t
  :bind-keymap ("C-c p" . projectile-command-map))

(use-package magit
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

;;; Clojure ;;;

(use-package flycheck-clj-kondo
  :ensure t)

(use-package clojure-mode
  :ensure t
  :config
  (require 'flycheck-clj-kondo))

(use-package cider
  :ensure t)

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
  :mode
  ("\\.js\\'" . 'js2-mode))

(use-package prettier-js
  :ensure t
  :hook
  ((js2-mode . prettier-js-mode)
   (web-mode . prettier-js-mode)))

;;; Python TODO ;;;

;;; Web ;;;

(use-package web-mode
  :ensure t)

;;; PHP ;;;

(use-package php-mode
  :ensure t
  :mode (("\\.phtml\\'" . php-mode)))

;; See also section 'LSP'

;;; LSP ;;;

(use-package lsp-mode
  :ensure t
  :hook
  ((clojure-mode . lsp)
   (clojurec-mode . lsp)
   (clojurescript-mode . lsp)
   (php-mode . lsp))
  :config
  (setenv "PATH" (concat
                   "/usr/local/bin" path-separator
                   (getenv "PATH")))
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
