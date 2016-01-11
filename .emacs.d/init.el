;; =============================================================
;; prelude

;; C-u 0 M-x byte-recompile-directory

(setq inhibit-startup-screen t)

(if window-system
    (progn
      (scroll-bar-mode -1)
      (tool-bar-mode -1)
      (set-face-attribute 'default nil :height 140))
  (menu-bar-mode 0))

;; =============================================================
;; package preamble

(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)

(when (boundp 'package-pinned-packages)
  (setq package-pinned-packages
        '((use-package . "melpa-stable"))))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(when (not (package-installed-p 'use-package))
  (package-install 'use-package))
(require 'use-package)
(require 'diminish)
(require 'bind-key)
;; ================================================================
;; package preamble finished.

(use-package clojure-mode
  :ensure t
  :pin melpa-stable
  :mode "\\.cljs$"
  :config
  (add-hook 'clojure-mode-hook 'yas-minor-mode)
  (add-hook 'clojure-mode-hook (lambda ()
                                 (clj-refactor-mode 1)
                                 (cljr-add-keybindings-with-prefix "C-c C-o")
                                 (linum-mode 1)
                                 (guide-key/add-local-guide-key-sequence "C-x")
                                 (guide-key/add-local-guide-key-sequence "C-c")
                                 (set-face-foreground 'font-lock-function-name-face "#808080"))))

(use-package cider
  :ensure t
  :defer t
  :pin melpa-stable
  :diminish (cider-mode . "c[]")
  :config
  (setq cider-repl-wrap-history t)
  (setq cider-repl-history-size 1000)
  (setq cider-repl-history-file "~/.emacs.d/cider-history")
  (setq cider-prompt-for-project-on-connect nil)
  (setq cider-prompt-for-symbol nil)
  (add-hook 'cider-repl-mode-hook 'subword-mode)
  (unbind-key "C-x C-r")
  (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
  (add-hook 'cider-repl-mode-hook 'cider-turn-on-eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'company-mode)
  (add-hook 'cider-mode-hook #'company-mode)
  :bind
  (("M-n"     . cider-repl-forward-input)
   ("M-p"     . cider-repl-backward-input)
   ("C-x M-e" . cider-pprint-eval-defun-at-point)
   ("C-x C-r" . cider-repl-previous-matching-input)))

(use-package clj-refactor
  :diminish "")

(use-package align-cljlet
  :bind (("C-c C-a" . align-cljlet)))

(use-package dash
  :ensure t)

;; paredit
(use-package paredit
  :diminish "«»"
  :config
  (-map
   (lambda (m)
     (add-hook m 'paredit-mode))
   '(lisp-mode-hook
     emacs-lisp-mode-hook
     scheme-mode-hook
     cider-repl-mode-hook
     clojure-mode-hook)))

(use-package markdown-mode
  :ensure t
  :pin melpa-stable
  :mode "\\.md")

(use-package puppet-mode
  :defer t
  :pin melpa-stable)

(use-package yaml-mode)
(use-package restclient
  :mode "\\.http\\'")

;; (defun repl/reset ()
;;   ""
;;   (interactive)
;;   (message (cider-interactive-eval "(dev/reset)")))

;; (defun repl/test ()
;;   ""
;;   (interactive)
;;   (message (cider-interactive-eval "(dev/run-all-my-tests)")))
;; (cider-repl-add-shortcut "reset" #'repl/reset)
;; (cider-repl-add-shortcut "test" #'repl/test)


(-map
 (lambda (m)
   (add-hook m (lambda () (toggle-truncate-lines -1))))
 '(markdown-mode-hook cider-repl-mode-hook))

(use-package magit
  :ensure t
  :pin melpa-stable
  :bind (("C-c C-g" . magit-status)
         ("C-c C-b" . magit-blame-mode))
  :config
  (setq magit-save-some-buffers 'dontask)
  (setq magit-diff-options (quote ("--ignore-space-change" "--ignore-all-space")))
  (setq magit-revert-buffers 'silent)
  (setq magit-diff-refine-hunk t))

(use-package git-gutter
  :ensure t
  :pin melpa-stable
  :diminish "GG"
  :bind (("C-x C-g" . git-gutter:toggle)))

(use-package ag
  :config
  (setq ag-highlight-search t)
  (setq ag-reuse-buffers t)
  (setq ag-arguments '("--ignore=*build/js*" "--line-number" "--smart-case" "--nogroup" "--column" "--"))
  (defun ag-search (string file-regex directory)
    "Search using ag in a given DIRECTORY (default: project root) and file type
regex FILE-REGEX for a given search STRING, with STRING defaulting to the
symbol under point.

If called with a prefix, prompts for flags to pass to ag."
    (interactive (list (read-from-minibuffer "Search string: " (ag/dwim-at-point))
                       (read-from-minibuffer "In filenames matching PCRE: " (ag/buffer-extension-regex))
                       (read-directory-name "Directory: " (ag/project-root default-directory))))
    (ag/search string directory :file-regex file-regex))
  :bind (("C-x M-f" . ag-search)))

;; eldoc
(diminish 'eldoc-mode "ED")

;; hl-sexp
(use-package hl-sexp
  :config
  (add-hook 'clojure-mode-hook 'hl-sexp-mode)
  (add-hook 'emacs-lisp-mode-hook 'hl-sexp-mode))

(use-package idle-highlight-mode
  :config
  (add-hook 'clojure-mode-hook 'idle-highlight-mode)
  (add-hook 'lisp-mode-hook 'idle-highlight-mode)
  (add-hook 'scheme-mode-hook 'idle-highlight-mode)
  (add-hook 'emacs-lisp-mode-hook 'idle-highlight-mode))

(use-package golden-ratio
  :diminish " φ"
  :config
  (golden-ratio-mode 1)
  (add-to-list 'golden-ratio-exclude-modes "ediff-mode")
  (add-to-list 'golden-ratio-exclude-modes "calendar-mode")
  (add-to-list 'golden-ratio-exclude-modes "undo-tree-visualizer"))

(use-package undo-tree
  :diminish " ⎌"
  :bind ("C-c M-z" . undo-tree-visualize)
  :config
  (global-undo-tree-mode 1))

(use-package yasnippet
  :diminish (yas-minor-mode . " Y")

  :config
  (yas-global-mode -1)
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets")
  (yas-load-directory "~/.emacs.d/snippets"))

(use-package company
  :diminish ""
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (global-set-key (kbd "M-TAB") #'company-complete))

(use-package browse-kill-ring
  :pin melpa-stable
  :config
  (browse-kill-ring-default-keybindings))

(use-package multiple-cursors
  :bind (( "C-c ."     . mc/mark-next-like-this)
         ( "C-c ,"     . mc/mark-previous-like-this)
         ( "C-c M-."   . mc/mark-all-like-this)
         ( "C-c M-SPC" . mc/edit-lines)
         ( "C-c M-,"   . mc/insert-numbers)))

(use-package ido-ubiquitous
  :config
  (ido-mode t)
  (ido-ubiquitous)
  (setq ido-enable-flex-matching t))

;; Autocomplete meta-x
(use-package smex
  :config
  (smex-initialize)
  (smex-initialize-ido)
  (setq smex-history-length 12)
  ;;TODO: bind
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  (global-set-key (kbd "C-c M-x") 'execute-extended-command))

(use-package expand-region
  :bind (("C-\\" . er/expand-region)))

(use-package yagist
  :config
  (setq yagist-encrypt-risky-config t)
  (setq yagist-github-token nil))

(use-package kaesar)

;; flyspell
(require 'flyspell)
(diminish 'flyspell-mode "FP")

;; show time
(setq display-time-24hr-format t)
(setq display-time-load-average t)
(display-time)

(setq global-mode-string
      (append global-mode-string
              '((:eval (concat "[⁋" (getenv "AM_PROFILE") "]")))))
;; jvm-mode
(use-package jvm-mode
  :pin melpa-stable
  :config
  (jvm-mode))

(winner-mode)       ;; C-c right/left
(show-paren-mode)
(global-auto-revert-mode t)
(column-number-mode t)

;; =============================================================
;; Color theme

(use-package cyberpunk-theme
  :ensure t
  :config
  (load-theme 'cyberpunk t))

;; =============================================================
;; Key bindings

;; ibuffer over list-buffers
(bind-key* "C-x C-b" 'ibuffer)

;; comments
(bind-key* "C-c ;" 'comment-or-uncomment-region)

;; better search
(bind-key* "C-s" 'isearch-forward-regexp)
(bind-key* "C-r" 'isearch-backward-regexp)
(bind-key* "C-M-s" 'isearch-forward)
(bind-key* "C-M-r" 'isearch-backward)

(global-set-key (kbd "RET") 'newline-and-indent)

;; =============================================================
;; Mode Settings

;; compojure
(define-clojure-indent
	(defroutes 'defun)
	(GET 2)
	(POST 2)
	(PUT 2)
	(DELETE 2)
	(HEAD 2)
	(ANY 2)
	(context 2))

;; =============================================================
;; Settings

(set-language-environment 'utf-8)
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-input-method nil)


(setq frame-title-format "%b")
(set-default 'truncate-lines t)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq scroll-step 1)
(setq scroll-error-top-bottom t)

;; remove trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; match parens
(setq blink-matching-paren-distance nil)
(setq-default indent-tabs-mode nil)
(setq default-tab-width 2)

(setq tab-width 2)
(setq python-indent 3)
(setq c-basic-offset 3)
(setq c-indent-level 3)
(setq c++-tab-always-indent nil)
(setq js-indent-level 3)

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))
;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; Save point position between sessions
(use-package saveplace
  :config
  (setq-default save-place t)
  (setq save-place-file (expand-file-name "places" user-emacs-directory)))

;; server
(require 'server)
(unless (server-running-p)
  (server-start))

;; =============================================================
;; Handy functions

;; XML pretty print
(defun pretty-print-xml-region (begin end)
  (interactive "r")
  (save-excursion
    (nxml-mode)
    (goto-char begin)
    (while (search-forward-regexp "\>[ \\t]*\<" nil t)
      (backward-char) (insert "\n"))
    (indent-region begin end))
  (message "Ah, much better!"))

(defun current-nrepl-server-buffer ()
  (let ((nrepl-server-buf (replace-regexp-in-string "connection" "server" (nrepl-current-connection-buffer))))
    (when nrepl-server-buf
      (get-buffer nrepl-server-buf))))

(defun clear-buffers ()
  (interactive)

  (cider-find-and-clear-repl-buffer)

  (with-current-buffer (current-nrepl-server-buffer)
    (kill-region (point-min) (point-max))))

(use-package mustache)
(use-package mustache-mode)

(bind-key* "M-~" 'ibuffer)

;; (maybe-install-and-require 'ace-jump-mode)
;; (autoload 'ace-jump-mode "ace-jump-mode" "Emacs quick move minor mode" t)
;; (autoload 'ace-jump-mode-pop-mark "ace-jump-mode" "Ace jump back" t)
;; (define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
;; (eval-after-load "ace-jump-mode"
;;   '(ace-jump-mode-enable-mark-sync))
;; (define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)

(bind-key* "C-c l" 'goto-line)

(use-package keyfreq
  :config
  (keyfreq-mode 0)
  (keyfreq-autosave-mode 0))

(use-package zencoding-mode
  :bind "C-c z")

(setq which-func-mode t)
(display-time-mode -1)

(use-package guide-key
  :config
  (guide-key-mode 1)
  (setq guide-key/popup-window-position 'bottom)
  (setq guide-key/idle-delay 2)
  :diminish " ?")

(use-package hl-todo
  :config
  (global-hl-todo-mode)
  (setq hl-todo-activate-in-modes (quote (emacs-lisp-mode clojure-mode javascript-mode)))
  ;;TODO simplify this.
  (setq hl-todo-keyword-faces (quote (("TODO" . "#cc9393")  ("TODO:" . "#cc9393")
                                      ("DONE" . "#afd8af")  ("DONE:" . "#afd8af")
                                      ("FIXME" . "#cc9393") ("FIXME:" . "#cc9393")
                                      ("XXX" . "#cc9393")   ("XXX:" . "#cc9393")))))

(-map
 (lambda (m)
   (add-hook m #'linum-mode)
   (add-hook m #'hl-todo-mode))
 '(clojure-mode-hook emacs-lisp-mode-hook javascript-mode-hook markdown-mode-hook puppet-mode-hook javascript-mode-hook))

(setq initial-scratch-message "")

(use-package avy
  :bind (("M-;" . avy-goto-char)))

(setq fill-column 80)
(setq comment-auto-fill-only-comments t)
(auto-fill-mode t)
(add-hook 'linum-before-numbering-hook
          (lambda ()
            (set-face-foreground 'linum  (face-attribute 'default :background))
            (set-face-background 'linum  (face-attribute 'font-lock-comment-face :foreground))))

(-map
 (lambda (m)
   (add-hook m (lambda () (linum-mode 1))))
 '(clojure-mode-hook emacs-lisp-mode-hook markdown-mode-hook org-mode-hook))

(setq calendar-minimum-window-height 5)
(setq vc-follow-symlinks nil)
(use-package recentf
  :config
  (recentf-mode 1)
  (setq recentf-max-menu-items 1000)
  (setq recentf-max-saved-items 1000)
  :bind (("M-`" . recentf-open-files)))

;; '((maybe-install-and-require 'tagedit)
;;   (eval-after-load "sgml-mode"
;;     '(progn
;;        (require 'tagedit)
;;        (tagedit-add-paredit-like-keybindings)
;;        (add-hook 'html-mode-hook (lambda () (tagedit-mode 1)))))
;;   (eval-after-load "mustache-mode"
;;     '(progn
;;        (require 'tagedit)
;;        (tagedit-add-paredit-like-keybindings)
;;        (add-hook 'mustache-mode-hook (lambda () (tagedit-mode 1))))))

(use-package deft
  :config
  (setq deft-auto-save-interval 60)
  (setq deft-extension "org")
  (setq deft-text-mode 'org-mode)
  (setq deft-use-filename-as-title t))

;; redefining the auto naming for Deft -- this makes it easier to
;; share amongst different machines with versioning.
(use-package uuid)
(defun deft-unused-slug ()
  (uuid-to-stringy (uuid-create)))

(add-hook 'ace-jump-mode-before-jump-hook
          (lambda ()
            (set-face-foreground 'ace-jump-face-foreground "blue")))

(use-package powerline
  :config
  (powerline-center-theme)
  (setq powerline-default-separator nil))

(defun back-window ()
  (interactive)
  (other-window -1))

(defun server-buffer ()
  (interactive)
  (switch-to-buffer-other-window (format "*nrepl-server %s*" (projectile-project-name))))

(defun repl-buffer ()
  (interactive)
  (switch-to-buffer-other-window (format "*cider-repl %s*" (projectile-project-name))))

(bind-key* "C-x 0" 'back-window)

;;NO longer defined (bind-key "C-c t"   'clojure-jump-between-tests-and-code cider-mode-map)
(bind-key "C-c C-t" 'cider-test-run-tests cider-mode-map)
(bind-key "C-c M-r" 'cider-repl-previous-matching-input)
(bind-key "C-c *" 'server-buffer)
(bind-key "C-c 8" 'repl-buffer)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (solarized-dark cyberpunk)))
 '(custom-safe-themes
   (quote
    ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(ediff-merge-split-window-function (quote split-window-horizontally))
 '(git-commit-finish-query-functions nil)
 '(org-confirm-babel-evaluate nil))

(setq bookmark-save-flag 0)

(unless (boundp 'warning-suppress-types)
  (setq warning-suppress-types nil))
(push '(undo discard-info) warning-suppress-types)

(unbind-key "C-z")

(use-package god-mode
  :diminish (god-local-mode . "[[☁️ GOD ⚡️ ]]"))

(use-package swiper
  :bind (("C-x M-s" . swiper)))

(use-package aggressive-indent
  :config
  (-map
   (lambda (m)
     (add-hook m #'aggressive-indent-mode))
   '(clojure-mode-hook emacs-lisp-mode-hook)))
;;(bind-key "C-c C-q" 'cider-quit clojure-mode-map)

(use-package mmm-mode)

(setq calendar-latitude 51.5683)
(setq calendar-longitude 0.1031)

(use-package dockerfile-mode)
(use-package projectile
  :diminish "🚀 "
  :init
  (projectile-global-mode t)

  (setq projectile-globally-ignored-directories
        (quote
         (".idea" ".eunit" ".git" ".hg" ".fslckout" ".bzr" "_darcs" ".tox" ".svn" "resources/public/js/compiled/")))
  (setq projectile-globally-ignored-files (quote ("TAGS" ".DS_Store"))))

(use-package json-mode
  :pin melpa-stable)
(use-package json-snatcher
  :pin melpa-stable)
(use-package jumblr
  :pin melpa-stable)

(use-package haskell-mode
  :config
  (add-hook 'haskell-mode-hook 'haskell-indent-mode)
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (add-hook 'haskell-mode-hook 'linum-mode))

(when (eq 'darwin system-type)
  (display-battery-mode t)
  (setq battery-mode-line-format "[b: %b%p%%]")

  (use-package color-theme)
  (use-package solarized-theme
    :config
    (load-theme 'solarized-dark))
  (use-package seethru
    :config
    (setq default-transparency 98)
    (seethru default-transparency)
    (bind-key "§" (lambda () (interactive)
                    (if (or (not (frame-parameter (selected-frame) 'alpha))
                            (= default-transparency (frame-parameter (selected-frame) 'alpha)))
                        (seethru 80)
                      (seethru default-transparency)))))

  (global-set-key (kbd "M-3") '(lambda () (interactive) (insert "#")))

  (setq cider-lein-command "/usr/local/bin/lein")
  (setq markdown-open-command "open -a /Applications/Marked.app")
  (setq ag-executable "/usr/local/bin/ag"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#002b36" :foreground "#839496" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :foundry "nil" :family "Menlo for Powerline"))))
 '(hl-todo ((t (:foreground "blue" :underline t :weight bold))))
 '(org-level-1 ((t (:inherit outline-1 :height 1.0))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.0))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.0))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.0))))
 '(org-todo ((t (:foreground "#2aa198" :weight bold :height 1.0 :box nil :underline t)))))

(use-package smartparens)

(use-package company
  :ensure t
  :pin melpa-stable
  :diminish company-mode
  :config
  (global-company-mode))

(defun live-paredit-delete-horizontal-space ()
  (interactive)
  (just-one-space -1)
  (paredit-backward-delete))

(defun live-paredit-tidy-trailing-parens ()
  (interactive)
  (save-excursion
    (while (ignore-errors (paredit-forward-up) t))
    (backward-char)
    (live-paredit-delete-horizontal-space)
    (while
        (or
         (eq (char-before) ?\))
         (eq (char-before) ?\})
         (eq (char-before) ?\]))
      (backward-char)
      (live-paredit-delete-horizontal-space))))

(use-package scala-mode2)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((clojure . t)
   (scala . t)
   (sh . t)
   ;;(pony . t)
   ))

(defun cider-repl--position-in-history (start-pos direction regexp)
  "Return the position of the history item starting at START-POS.
Search in DIRECTION for REGEXP.
Return -1 resp the length of the history if no item matches."
  ;; Loop through the history list looking for a matching line
  (let* ((step (cl-ecase direction
                 (forward -1)
                 (backward 1)))
         (history cider-repl-input-history)
         (len (length history)))
    (cl-loop for pos = (+ start-pos step) then (+ pos step)
             if (< pos 0) return -1
             if (<= len pos) return len
             if (string-match-p regexp (nth pos history)) return pos)))



(use-package echo-bell
  :config
  (echo-bell-mode t)
  (setq echo-bell-background "dark red")
  (setq echo-bell-string "D'oh!"))

(use-package s)

(defun filter-repl-history (&optional arg)
  (interactive)
  (-filter
   (lambda (s)
     (s-matches? (if arg
                     arg
                   (buffer-substring cider-repl-input-start-mark (point))) s))
   cider-repl-input-history))

(defun company-repl-history
    (command &optional arg)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-repl-history))
    (prefix (and (eq major-mode 'cider-repl-mode)
                 (buffer-substring cider-repl-input-start-mark (point))))

    ;;    (or  "")
    (candidates
     (filter-repl-history arg))))

(add-to-list 'company-backends 'company-repl-history)
