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
;; package

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defun maybe-install-and-require (p)
  (when (not (package-installed-p p))
    (package-install p))
  (require p))

(maybe-install-and-require 'diminish)
(maybe-install-and-require 'bind-key)
;; =============================================================
;; Major modes

;; Clojure
(maybe-install-and-require 'clojure-mode)
(maybe-install-and-require 'clojure-test-mode)
(setq auto-mode-alist (cons '("\\.cljs$" . clojure-mode) auto-mode-alist))

;; Tuareg / OCaml
(setq save-abbrevs nil)
(diminish 'abbrev-mode)
(maybe-install-and-require 'tuareg)
(add-hook 'tuareg-mode-hook 'tuareg-imenu-set-imenu)
(setq auto-mode-alist
      (append '(("\\.ml[ily]?$" . tuareg-mode)
                ("\\.topml$" . tuareg-mode))
              auto-mode-alist))

;; markdown
(maybe-install-and-require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; Puppet
(maybe-install-and-require 'puppet-mode)

;; Yaml
(maybe-install-and-require 'yaml-mode)

;; Restclient
(maybe-install-and-require 'restclient)
(add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode))

;; =============================================================
;; Minor modes

;; Cider
(maybe-install-and-require 'cider)
(maybe-install-and-require 'cider-tracing)
(diminish 'cider-mode " Cdr")
(setq cider-repl-wrap-history t)
(setq cider-repl-history-size 1000)
(setq cider-repl-history-file "~/.emacs.d/cider-history")
(add-hook 'cider-repl-mode-hook 'subword-mode)
(add-hook 'cider-repl-mode-hook (lambda () (toggle-truncate-lines -1)))
;; Eval to buffer
(bind-key "C-x M-e" 'cider-pprint-eval-defun-at-point cider-mode-map)
(unbind-key "C-x C-r")
(bind-key "C-x C-r" 'cider-repl-previous-matching-input cider-mode-map)



;; clj-refactor
(maybe-install-and-require 'clj-refactor)
(diminish 'clj-refactor-mode)


;; align-cljlet
(maybe-install-and-require 'align-cljlet)
(bind-key* "C-c C-a" 'align-cljlet)

;; slamhound
(maybe-install-and-require 'slamhound)

;; paredit
(maybe-install-and-require 'paredit)
(diminish 'paredit-mode " Pe")
(add-hook 'lisp-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'scheme-mode-hook 'paredit-mode)
(add-hook 'cider-repl-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'paredit-mode)

;; utop / OCaml
;; (maybe-install-and-require 'utop)
;; (autoload 'utop-setup-ocaml-buffer "utop" "Toplevel for OCaml" t)
;; (add-hook 'tuareg-mode-hook 'utop-setup-ocaml-buffer)

;; merlin / OCaml
;; (maybe-install-and-require 'merlin)
;; (diminish 'merlin-mode "MRL")
;; (add-hook 'tuareg-mode-hook 'merlin-mode)
;; (setq merlin-use-auto-complete-mode t)
;; (setq merlin-error-after-save nil)

;; Magit
(maybe-install-and-require 'magit)
(bind-key* "C-c C-g" 'magit-status)
(bind-key* "C-c C-b" 'magit-blame-mode)
(setq magit-save-some-buffers 'dontask)
(diminish 'magit-auto-revert-mode)

;; git gutter
(maybe-install-and-require 'git-gutter)
(diminish 'git-gutter-mode "GG")
;;(global-git-gutter-mode t)
(bind-key* "C-x C-g" 'git-gutter:toggle)

;; silver searcher
(maybe-install-and-require 'ag)
(setq ag-highlight-search t)
(setq ag-reuse-buffers t)

(defun ag-search (string file-regex directory)
  "Search using ag in a given DIRECTORY (default: project root) and file type
regex FILE-REGEX for a given search STRING, with STRING defaulting to the
symbol under point.

If called with a prefix, prompts for flags to pass to ag."
  (interactive (list (read-from-minibuffer "Search string: " (ag/dwim-at-point))
                     (read-from-minibuffer "In filenames matching PCRE: " (ag/buffer-extension-regex))
                     (read-directory-name "Directory: " (ag/project-root default-directory))))
  (ag/search string directory :file-regex file-regex))
(bind-key* "C-x M-f" 'ag-search)

;; eldoc
(diminish 'eldoc-mode "ED")
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(add-hook 'cider-repl-mode-hook 'cider-turn-on-eldoc-mode)

;; hl-sexp
(maybe-install-and-require 'hl-sexp)
(add-hook 'clojure-mode-hook 'hl-sexp-mode)
(add-hook 'lisp-mode-hook 'hl-sexp-mode)
(add-hook 'scheme-mode-hook 'hl-sexp-mode)
(add-hook 'emacs-lisp-mode-hook 'hl-sexp-mode)

;; idle-highlight-mode
(maybe-install-and-require 'idle-highlight-mode)
(add-hook 'clojure-mode-hook 'idle-highlight-mode)
(add-hook 'lisp-mode-hook 'idle-highlight-mode)
(add-hook 'scheme-mode-hook 'idle-highlight-mode)
(add-hook 'emacs-lisp-mode-hook 'idle-highlight-mode)

;; Golden Ratio
(maybe-install-and-require 'golden-ratio)
(diminish 'golden-ratio-mode " φ")
(golden-ratio-mode 1)
(add-to-list 'golden-ratio-exclude-modes "ediff-mode")
(add-to-list 'golden-ratio-exclude-modes "calendar-mode")
(add-to-list 'golden-ratio-exclude-modes "undo-tree-visualizer")

;; undo-tree
(maybe-install-and-require 'undo-tree)
(diminish 'undo-tree-mode " ⎌")
(global-undo-tree-mode)
(global-set-key (kbd "C-c M-z") 'undo-tree-visualize)

;; yasnippet
(maybe-install-and-require 'yasnippet)
(diminish 'yas-minor-mode " Y")
(maybe-install-and-require 'clojure-snippets)
(yas-global-mode 1)
(add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets")
(yas-load-directory "~/.emacs.d/snippets")

;; company mode
(maybe-install-and-require 'company)
(diminish 'company-mode)
(add-hook 'after-init-hook 'global-company-mode)

;; browse-kill-ring
(maybe-install-and-require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)

;; multiple cursors
(maybe-install-and-require 'multiple-cursors)
(bind-key* "C-c ."     'mc/mark-next-like-this)
(bind-key* "C-c ,"     'mc/mark-previous-like-this)
(bind-key* "C-c M-."   'mc/mark-all-like-this)
(bind-key* "C-c M-SPC" 'mc/edit-lines)
(bind-key* "C-c M-,"   'mc/insert-numbers)

;; IDO
(maybe-install-and-require 'ido-ubiquitous)
(ido-mode t)
(ido-ubiquitous)
(setq ido-enable-flex-matching t)

;; Autocomplete meta-x
(maybe-install-and-require 'smex)
(smex-initialize)
(smex-initialize-ido)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(setq smex-history-length 12)
(global-set-key (kbd "C-c M-x") 'execute-extended-command)


;; expand region
(maybe-install-and-require 'expand-region)
(bind-key* "C-\\" 'er/expand-region)

;; yagist
(maybe-install-and-require 'yagist)
(maybe-install-and-require 'kaesar)
(setq yagist-encrypt-risky-config t)
(setq yagist-github-token nil)

;; flyspell
(require 'flyspell)
(diminish 'flyspell-mode "FP")

;; show time
(setq display-time-24hr-format t)
(setq display-time-load-average t)
(display-time)

;; jvm-mode
(maybe-install-and-require 'jvm-mode)
(jvm-mode)

(winner-mode)       ;; C-c right/left
(show-paren-mode)
(global-auto-revert-mode t)
(column-number-mode t)

;; =============================================================
;; Color theme

(maybe-install-and-require 'cyberpunk-theme)
(load-theme 'cyberpunk t)

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
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name "places" user-emacs-directory))

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

(maybe-install-and-require 'mustache)
(maybe-install-and-require 'mustache-mode)

(bind-key* "M-~" 'ibuffer)

(maybe-install-and-require 'ace-jump-mode)
(autoload 'ace-jump-mode "ace-jump-mode" "Emacs quick move minor mode" t)
(autoload 'ace-jump-mode-pop-mark "ace-jump-mode" "Ace jump back" t)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)
(bind-key* "C-c l" 'goto-line)

(maybe-install-and-require 'keyfreq)
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)

(maybe-install-and-require 'zencoding-mode)
(bind-key* "C-c z" 'zencoding-mode)

(setq which-func-mode t)
(display-time-mode -1)
;; (display-battery-mode t)
;; (setq battery-mode-line-format "[b: %b%p%%]")

(maybe-install-and-require 'guide-key)
(guide-key-mode 1)
(diminish 'guide-key-mode " ?")
(setq guide-key/popup-window-position 'bottom)
(setq guide-key/idle-delay 2)

(add-hook 'clojure-mode-hook (lambda ()
                               (clj-refactor-mode 1)
                               (cljr-add-keybindings-with-prefix "C-c C-o")
                               (linum-mode 1)
                               (guide-key/add-local-guide-key-sequence "C-x")
                               (guide-key/add-local-guide-key-sequence "C-c")
                               (set-face-foreground 'font-lock-function-name-face "#808080")))

(bind-key* "C-c *" (lambda () (interactive) (switch-to-buffer-other-window "*scratch*")))

;; Open a rest client to interact with a server.
(bind-key* "C-c C-r" (lambda ()
                       (interactive)
                       (with-current-buffer "*REST*"
                         (restclient-mode)
                         (goto-char (point-max))
                         (insert "\n################\nGET http://"))
                       (switch-to-buffer-other-window "*REST*")
                       (goto-char (point-max))))

(setq initial-scratch-message "
	rf 'cljr-rename-file
	ru 'cljr-replace-use
	au 'cljr-add-use-to-ns
	ar 'cljr-add-require-to-ns
	ai 'cljr-add-import-to-ns
	sn 'cljr-sort-ns
	rr 'cljr-remove-unused-requires
	sr 'cljr-stop-referring
	th 'cljr-thread
	uw 'cljr-unwind
	ua 'cljr-unwind-all
	il 'cljr-introduce-let
	el 'cljr-expand-let
	ml 'cljr-move-to-let
	mf 'cljr-move-form
	tf 'cljr-thread-first-all
	tl 'cljr-thread-last-all
	cp 'cljr-cycle-privacy
	cc 'cljr-cycle-coll
	cs 'cljr-cycle-stringlike
	ci 'cljr-cycle-if
	ad 'cljr-add-declaration
	dk 'cljr-destructure-keys
	pc 'cljr-project-clean
	")

(setq fill-column 80)
(setq comment-auto-fill-only-comments t)
(auto-fill-mode t)
(add-hook 'linum-before-numbering-hook
          (lambda ()
            (set-face-foreground 'linum  (face-attribute 'default :background))
            (set-face-background 'linum  (face-attribute 'font-lock-comment-face :foreground))))

;; toggle the default colours on linum mode.

(setq calendar-minimum-window-height 5)
(setq vc-follow-symlinks nil)
(maybe-install-and-require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 1000)
(bind-key* "M-`" 'recentf-open-files)
(bind-key* "C-x M-`" 'tmm-menubar)

(eval-after-load 'clojure-mode
  '(font-lock-add-keywords
    'clojure-mode `(("(\\(partial\\)[[:space:]]"
                     (0 (progn (compose-region (match-beginning 1)
                                               (match-end 1) "Ƥ")
                               nil))))))

(eval-after-load 'clojure-mode
  '(font-lock-add-keywords
    'clojure-mode `(("(\\(comp\\)[[:space:]]"
                     (0 (progn (compose-region (match-beginning 1)
                                               (match-end 1) "∘")
                               nil))))))

(eval-after-load 'clojure-mode
  '(font-lock-add-keywords
    'clojure-mode `(("(\\(fn\\)[\[[:space:]]"
                     (0 (progn (compose-region (match-beginning 1)
                                               (match-end 1) "λ")
                               nil))))))

(eval-after-load 'clojure-mode
  '(font-lock-add-keywords
    'clojure-mode `(("\\(#\\)("
                     (0 (progn (compose-region (match-beginning 1)
                                               (match-end 1) "ƒ")
                               nil))))))

(eval-after-load 'clojure-mode
  '(font-lock-add-keywords
    'clojure-mode `(("\\(#\\){"
                     (0 (progn (compose-region (match-beginning 1)
                                               (match-end 1) "∈")
                               nil))))))

'((maybe-install-and-require 'tagedit)
  (eval-after-load "sgml-mode"
    '(progn
       (require 'tagedit)
       (tagedit-add-paredit-like-keybindings)
       (add-hook 'html-mode-hook (lambda () (tagedit-mode 1)))))
  (eval-after-load "mustache-mode"
    '(progn
       (require 'tagedit)
       (tagedit-add-paredit-like-keybindings)
       (add-hook 'mustache-mode-hook (lambda () (tagedit-mode 1))))))


(maybe-install-and-require 'deft)
(setq deft-auto-save-interval 60)
(setq deft-extension "org")
(setq deft-text-mode 'org-mode)
(setq deft-use-filename-as-title t)

;; redefining the auto naming for Deft -- this makes it easier to
;; share amongst different machines with versioning.
(maybe-install-and-require 'uuid)
(defun deft-unused-slug ()
  (uuid-to-stringy (uuid-create)))

(add-hook 'ace-jump-mode-before-jump-hook
          (lambda ()
            (set-face-foreground 'ace-jump-face-foreground "blue")))

(maybe-install-and-require 'powerline)
(powerline-center-theme)
(setq global-mode-string
      (append global-mode-string
              '((:eval (concat "[⁋" (getenv "AM_PROFILE") "]")))))

(defun back-window ()
    (interactive)
      (other-window -1))

(bind-key* "C-x 0" 'back-window)

(bind-key "C-c t"   'clojure-jump-between-tests-and-code cider-mode-map)
(bind-key "C-c C-t" 'cider-test-run-tests cider-mode-map)

;; toggle-truncate-lines
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.

 '(magit-diff-options (quote ("--ignore-space-change" "--ignore-all-space")))
 '(powerline-show-vc nil))

(setq bookmark-save-flag 0)

(unless (boundp 'warning-suppress-types)
  (setq warning-suppress-types nil))
(push '(undo discard-info) warning-suppress-types)


;; http://www.emacswiki.org/emacs/AlarmBell#toc11
(defun mode-line-visible-bell ()
  "A friendlier visual bell effect."
  (invert-face 'mode-line)
  (run-with-timer 0.3 nil 'invert-face 'mode-line))
;;todo: for the C-<- and C-->  do a bell first.

(setq visible-bell nil
      ring-bell-function 'mode-line-visible-bell)
