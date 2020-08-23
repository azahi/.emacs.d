;;; init.el --- Emacs initialization file -*- lexical-binding: t; -*-

;;; Commentary:
;;; TODO Split this disgusting mess into different files.
;;; TODO Pull hooks from `emacs-startup-hook', make it as "lazy" as possible.

;;; Code:
;;;

;; Prevent garbage collection from running before invoking `ghcm-mode'.
(setq gc-cons-threshold most-positive-fixnum)

;;
;;; External libraries.
;;

(require 'cl)
(require 'subr-x)

;;
;;; Library.
;;

(defun e-enlist (exp)
  (declare (pure t) (side-effect-free t))
  (if (listp exp) exp (list exp)))

(defun e-path-p (&rest segments)
  (let ((dir (pop segments)))
    (unless segments
      (setq dir (expand-file-name dir)))
    (while segments
      (setq dir (expand-file-name (car segments) dir)
            segments (cdr segments)))
    dir))

(defun e-glob (&rest segments)
  (let* (case-fold-search
         (dir (apply #'e-path-p segments)))
    (if (string-match-p "[[*?]" dir)
        (file-expand-wildcards dir t)
      (if (file-exists-p dir)
          dir))))

(defun e-path (&rest segments)
  (if segments
      (apply #'e-path-p segments)
    (file!)))

;;
;;; Variables.
;;

;; Ensure running from file's directory.
(let (file-name-handler-alist)
  (setq user-emacs-directory (file-name-directory load-file-name)))

;; Directories.
(defconst e-emacs-dir
  (eval-when-compile (file-truename user-emacs-directory)))
(defconst e-local-dir (concat e-emacs-dir ".local/"))
(defconst e-etc-dir (concat e-local-dir "etc/"))
(defconst e-cache-dir (concat e-local-dir "cache/"))
(defconst e-autoload-file (concat e-local-dir "autoloads.el"))
(defconst e-env-file (concat e-local-dir "env"))

(defvar-local e-inhibit-indent-detection nil)

;; Unicode.
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8
      selection-conding-system 'utf-8)

;; Disable warnings from legacy advice system.
(setq ad-redefinition-action 'accept)

;; Make apropos omnipotent.
(setq apropos-do-all t)

;; A second, case-insensitive pass over `auto-mode-alist' is time wasted.
(setq auto-mode-case-fold nil)

;; Less noise at startup.
(setq inhibit-default-init t
      inhibit-startup-echo-area-message user-login-name
      inhibit-startup-message t
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil)

;; Unless in a daemon session, disable messages about startup.
(unless (daemonp)
  (advice-add #'display-startup-echo-area-message :override #'ignore))

;; Scale update tick.
(setq idle-update-delay 1.0)

;; Makes tramp a lot faster.
(setq tramp-default-method "ssh")

;; Security.
(setq gnutls-verify-error t
      gnutls-algorithm-priority
      (when (boundp 'libgnutls-version)
        (concat "SECURE128:+SECURE192:-VERS-ALL"
                (if (and (not (version< emacs-version "26.3"))
                         (>= libgnutls-version 30605))
                    ":+VERS-TLS1.3")
                ":+VERS-TLS1.2"))
      ;; https://www.keylength.com/en/4/
      gnutls-min-prime-bits 3072
      tls-checktrust gnutls-verify-error
      ;; https://redd.it/8sykl1
      tls-program '("openssl s_client -connect %h:%p -CAfile %t -nbio -no_ssl3 -no_tls1 -no_tls1_1 -ign_eof"
                    "gnutls-cli -p %p --dh-bits=3072 --ocsp --x509cafile=%t --strict-tofu --priority='SECURE192:+SECURE128:-VERS-ALL:+VERS-TLS1.2:+VERS-TLS1.3' %h"
                    "gnutls-cli -p %p %h"))

;; Stop storing `authinfo' in plain-text format.
(setq auth-sources (list (concat e-etc-dir "authinfo.gpg")
                         "~/.authinfo.gpg"))

;; No littering in my swamp.
(setq abbrev-file-name                      (concat e-local-dir "abbrev.el")
      async-byte-compile-log-file           (concat e-etc-dir "async-bytecomp.log")
      bookmark-default-file                 (concat e-etc-dir "bookmarks")
      custom-file                           (concat e-local-dir "config.el")
      custom-theme-directory                (concat e-local-dir "themes/")
      desktop-base-file-name                "autosave"
      desktop-base-lock-name                "autosave-lock"
      desktop-dirname                       (concat e-etc-dir "desktop")
      pcache-directory                      (concat e-cache-dir "pcache/")
      request-storage-directory             (concat e-cache-dir "request")
      shared-game-score-directory           (concat e-etc-dir "shared-game-score/")
      tramp-auto-save-directory             (concat e-cache-dir "tramp-auto-save/")
      tramp-backup-directory-alist          backup-directory-alist
      tramp-persistency-file-name           (concat e-cache-dir "tramp-persistency.el")
      url-cache-directory                   (concat e-cache-dir "url/")
      url-configuration-directory           (concat e-etc-dir "url/"))

;; Stop sessions from littering the user directory.
(advice-add 'emacs-session-filename :override
  (lambda (&rest _)
    (concat e-cache-dir "emacs-session." sessoin-id)))

;; When enabling disabled commands write them to `config.el' instead.
(advice-add #'en/disable-command :around
  (lambda (orig-fn &rest args)
    (let ((use-init-file custom-file))
      (apply orig-fn args))))

;;
;;; Optimization.
;;

;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)

;; Defer `tty-run-terminal-initialization'.
(unless (daemonp)
  (advice-add #'tty-run-terminal-initialization :override #'ignore)
  (add-hook 'window-setup-hook
    (lambda (&rest _)
      (advice-remove #'tty-run-terminal-initialization #'ignore)
      (tty-run-terminal-initialization (selected-frame) nil t))))

;;
;;; Package.
;;

(setq package-enable-at-startup nil
      package-user-dir (concat e-local-dir "elpa/")
      package-gnupghome-dir (expand-file-name "gpg" package-user-dir)
      package-archives
      (let ((proto (if gnutls-verify-error "https" "http")))
        `(("gnu"   . ,(concat proto "://elpa.gnu.org/packages/"))
          ("melpa" . ,(concat proto "://melpa.org/packages/"))
          ("org"   . ,(concat proto "://orgmode.org/elpa/")))))

;; Prevent `init.el' to be modified by `package.el'.
(advice-add #'package--ensure-init-file :override #'ignore)

;;
;;; Straight.
;;

(setq straight-base-dir e-local-dir
      straight-repository-branch "develop")

;; Ensure `straight.el'.
(defun e-ensure-straight (&rest _)
  (let ((repo-dir (expand-file-name "straight/repos/straight.el" straight-base-dir ))
        (repo-url (concat "http" (if gnutls-verify-error "s") "://github.com/raxod502/straight.el"))
        (branch straight-repository-branch)
        (call (lambda (command &rest args)
                (with-temp-buffer
                  (cons (or (apply #'call-process command nil t nil (remq nil args))
                            -1)
                        (string-trim (buffer-string)))))))
    (unless (file-directory-p repo-dir)
      (funcall call "git" "clone" repo-url repo-dir
               "--origin" "origin"
               "--branch" straight-repository-branch
               "--single-branch"
               "--no-tags"))
    (require 'straight (concat repo-dir "/straight.el"))
    (with-temp-buffer
      (insert-file-contents (expand-file-name "bootstrap.el" repo-dir))
      (eval-region (search-forward "(require 'straight)")
                   (point-max)))))
(e-ensure-straight)

;;
;;; Use-package.
;;

(straight-use-package 'use-package)

(autoload 'use-package "use-package-core" nil nil t)

(setq use-package-compute-statistics nil
      use-package-verbose nil
      use-package-minimum-reported-time 0.1
      use-package-expand-minimally t)

;; Activate straight.el integration.
(setq straight-use-package-by-default t)

;;
;;; GC.
;;

(use-package gcmh
  :init
  (setq gcmh-idle-delay 5
        gcmh-high-cons-threshold (* 16 1024 1024)
        gcmh-verbose nil)
  :config (gcmh-mode 1))

;;
;;; Editor.
;;

;; Resolve symlinks when opening files.
(setq find-file-visit-truename t
      vc-follow-symlinks t)

;; Disable the warning "X and Y are the same file".
(setq find-file-suppress-same-file-warnings t)

;; Create missing directories when we open a file that doesn't exist under a
;; directory tree that may not exist.
(add-hook 'find-file-not-found-functions
  (lambda (&rest _)
    (unless (file-remote-p buffer-file-name)
      (let ((parent-directory (file-name-directory buffer-file-name)))
        (and (not (file-directory-p parent-directory))
             (y-or-n-p (format "Directory `%s' does not exist! Create it?"
                               parent-directory))
             (progn (make-directory parent-directory 'parents)
                    t))))))

;; Autosaves.
(setq auto-save-default nil
      create-lockfiles nil
      make-backup-files nil
      auto-save-list-file-prefix (concat e-cache-dir "autosave/")
      auto-save-file-name-transforms `((".*" ,auto-save-list-file-prefix t))
      backup-directory-alist `((".*" . ,(concat e-cache-dir "backup/"))))

(with-eval-after-load 'tramp
  (add-to-list 'backup-directory-alist (cons tramp-file-name-regexp nil)))

(add-hook 'after-save-hook
  (lambda (&rest _)
    (when (eq major-mode 'fundamental-mode)
      (let ((buffer (or (buffer-base-buffer) (current-buffer))))
        (and (buffer-file-name buffer)
             (eq buffer (window-buffer (selected-window)))
             (set-auto-mode))))))

;;; Formatting.

;; Prefer spaces over tabs.
(setq-default indent-tabs-mode nil
              tab-width 4)

;; Make `tabify' and `untabify' only affect indentation.
(setq tabify-regexp "^\t* [ \t]+")

;; Reject modernity. Embrace tradition.
(setq-default fill-column 80)

;; Continue wrapped words at whitespace, rather than in the middle of a word.
(setq-default word-wrap t)
;; ...but don't do any wrapping by default.
(setq-default truncate-lines t)

;; This looks really bad.
(setq sentence-end-double-space nil)

;; POSIX.
(setq require-final-newline t)

;; Default to soft line-wrapping in text modes.
(add-hook 'text-mode-hook #'visual-line-mode)

;;; Clipboard

;; Cull duplicates in the kill ring to reduce bloat.
(setq kill-do-not-save-duplicates t)

;; Allow UTF or composed text from the clipboard.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;;; Built-in packages.

(use-package autorevert
  :config
  (setq auto-revert-verbose t
        auto-revert-use-notify nil
        auto-revert-stop-on-user-input nil
        revert-without-query (list ".")))

(use-package recentf
  :hook (emacs-startup . recentf-mode)
  :commands recentf-open-files
  :config
  (defun e-recent-file-truename (file)
    (if (or (file-remote-p file nil t)
            (not (file-remote-p file)))
        (file-truename file)
      file))
  (setq recentf-filename-handlers '(substring-no-properties
                                     e-recent-file-truename
                                     abbreviate-file-name)
        recentf-save-file (concat e-cache-dir "recentf")
        recentf-auto-cleanup 'never
        recentf-max-menu-items 0
        recentf-max-saved-items 200)

  (add-hook 'dired-mode-hook
    (lambda (&rest _)
      (recentf-add-file default-directory)))

  (add-hook 'kill-emacs-hook #'recentf-cleanup))

(use-package savehist
  :hook (emacs-startup . savehist-mode)
  :init
  (setq savehist-file (concat e-cache-dir "savehist"))
  :config
  (setq savehist-save-minibuffer-history t
        savehist-autosave-interval nil
        savehist-additional-variables '(kill-ring
                                        mark-ring
                                        global-mark-ring
                                        search-ring
                                        regexp-search-ring))
  (add-hook 'savehist-save-hook
    (lambda (&rest _)
      (setq kill-ring (cl-loop for item in kill-ring
                               if (stringp item)
                               collect (substring-no-properties item)
                               else if item collect it)))))

(use-package saveplace
  :hook (pre-command . save-place-mode)
  :init
  (setq save-place-file (concat e-cache-dir "saveplace")
        save-place-limit 100)
  :config
  (advice-add #'save-place-find-file-hook :after-while
    (lambda (&rest _)
      (if buffer-file-name (ignore-errors (recentr)))))

  (save-place-mode))

(use-package server
  :when (display-graphic-p)
  :defer 1
  :init
  (when-let (name (getenv "EMACS_SERVER_NAME"))
    (setq server-name name))
  :config
  (unless (server-running-p)
    (server-start)))

(defvar-local e-electric-indent-words '())
(with-eval-after-load 'electric
  (setq-default electric-indent-chars '(?\n ?\^?))

  (add-hook 'electric-indent-functions
    (lambda (_c)
      (when (and (eolp) e-electric-indent-words)
        (save-excursion
          (backward-word)
          (looking-at-p (concat "\\<" (regexp-opt e-electric-indent-words))))))))

;;; External packages.

(use-package auto-minor-mode)

(use-package better-jumper
  :hook (emacs-startup . better-jumper-mode)
  :hook (better-jumper-post-jump . recenter)
  :commands (e-set-jump e-set-jump-maybe)
  :config
  (defun e-set-jump (orig-fn &rest args)
    (better-jumper-set-jump (if (markerp (car args)) (car args)))
    (let ((evil--jumps-jumping t)
          (better-jumper--jumping t))
      (apply orig-fn args)))
  (advice-add #'kill-current-buffer :around #'e-set-jump)
  (advice-add #'imenu :around #'e-set-jump-a)

  (defun e-set-jump-maybe-a (orig-fn &rest args)
    (let ((origin (point-marker))
          (result
           (let* ((evil--jumps-jumping t)
                  (better-jumper--jumping t))
             (apply orig-fn args))))
      (unless result
        (with-current-buffer (marker-buffer origin)
          (better-jumper-set-jump
           (if (markerp (car args))
               (car args)
             origin))))
      result)))

(use-package dtrt-indent
  :hook ((change-major-mode-after-body read-only-mode) . e-detect-indentation)
  :config
  (defun e-detect-indentation ()
    (unless (or (not after-init-time)
                e-inhibit-indent-detection
                (eq major-mode 'fundamental-mode)
                (member (substring (buffer-name) 0 1) '(" " "*")))
      (let ((inhibit-message nil))
        (dtrt-indent-mode +1))))

  ;; Always keep tab-width up-to-date.
  (push '(t tab-width) dtrt-indent-hook-generic-mapping-list))

(use-package editorconfig
  :hook (emacs-startup . editorconfig-mode)
  :config
  (when (require 'ws-butler nil t)
    (setq editorconfig-trim-whitespaces-mode 'ws-butler-mode))

  (defvar e-editorconfig-mode-alist
    '((emacs-lisp-mode . "el")
      (js2-mode        . "js")
      (perl-mode       . "pl")
      (php-mode        . "php")
      (python-mode     . "py")
      (ruby-mode       . "rb")
      (sh-mode         . "sh")))
  (advice-add #'editorconfig-call-editorconfig-exec :around
    (lambda (orig-fn)
      (let ((buffer-file-name
            (if (and (not (bound-and-true-p org-src-mode))
                      (file-name-extension buffer-file-name))
                buffer-file-name
              (format "%s%s" (buffer-file-name (buffer-base-buffer))
                      (if-let (ext (alist-get major-mode e-editorconfig-mode-alist))
                          (concat "." ext)
                        "")))))
        (funcall orig-fn))))

  (add-hook 'editorconfig-after-apply-functions
    (lambda (props)
      (when (or (gethash 'indent_style props)
                (gethash 'indent_size props))
        (setq e-inhibit-indent-detection 'editorconfig)))))

(use-package helpful
  :commands helpful--read-symbol
  :init
  (global-set-key [remap describe-function] #'helpful-callable)
  (global-set-key [remap describe-command]  #'helpful-command)
  (global-set-key [remap describe-variable] #'helpful-variable)
  (global-set-key [remap describe-key]      #'helpful-key)
  (global-set-key [remap describe-symbol]   #'helpful-symbol)

  (with-eval-after-load 'apropos
    (dolist (fun-bt '(apropos-function apropos-macro apropos-command))
      (button-type-put
       fun-bt 'action
       (lambda (button)
         (helpful-callable (button-get button 'apropos-symbol)))))
    (dolist (var-bt '(apropos-variable apropos-user-option))
      (button-type-put
       var-bt 'action
       (lambda (button)
         (helpful-variable (button-get button 'apropos-symbol)))))))

(use-package imenu
  :hook (recenter . imenu-after-jump-hook))

(use-package smartparens
  :hook (prog-mode . smartparens-mode)
  :commands (sp-pair sp-local-pair sp-with-modes sp-point-in-comment sp-point-in-string)
  :config
  ;; Recognize both `slime-mrepl-mode' and `sly-mrepl-mode'.
  (add-to-list 'sp-lisp-modes 'sly-mrepl-mode)
  ;; Load default smartparens rules for various languages.
  (require 'smartparens-config)
  ;; Ditch overlays in favour of show-parens.
  (setq sp-highlight-pair-overlay nil
        sp-highlight-wrap-overlay nil
        sp-highlight-wrap-tag-overlay nil)
  (with-eval-after-load 'evil
    ;; ...unless we are evil.
    (setq sp-show-pair-from-inside t
          sp-cancel-autoskip-on-backward-movement nil))

  ;; Performance.
  (setq sp-max-prefix-length 25
        sp-max-pair-length 4)

  ;; https://github.com/Fuco1/smartparens/issues/783
  (setq sp-escape-quotes-after-insert nil)

  ;; Be quiet.
  (dolist (key '(:unmatched-expression :no-matching-tag))
    (setf (alist-get key sp-message-alist) nil))

  (add-hook 'minibuffer-setup-hook
    (lambda (&rest _)
      (and (memq this-command '(eval-expression pp-eval-expression evil-ex))
           smartparens-global-mode
           (smartparens-mode))))

  ;; Disable in minibuffer.
  (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
  (sp-local-pair 'minibuffer-inactive-mode "`" nil :actions nil)

  ;; Breakage in evil-mode's replace state.
  (defvar e-buffer-smartparens-mode nil)
  (add-hook 'evil-replace-state-exit-hook
    (lambda (&rest _)
      (when e-buffer-smartparens-mode
        (turn-on-smartparens-mode)
        (kill-local-variable 'e-buffer-smartparens-mode))))
  (add-hook 'evil-replace-state-entry-hook
    (lambda (&rest _)
      (when smartparens-mode
        (setq-local e-buffer-smartparens-mode t)
        (turn-off-smartparens-mode)))))

(use-package ws-butler
  :hook (prog-mode . ws-butler-global-mode))

;;
;;; UX.
;;

;; Just quit.
(setq confirm-kill-emacs 'yes-or-no-p)

;; Don't prompt for confirmation on creating a new file.
(setq confirm-nonexistent-file-or-buffer nil)

(setq uniquify-buffer-name-style 'forward
      ring-bell-function #'ignore
      visible-bell nil)

;;; Cursor.

;; Bit distracting.
(blink-cursor-mode -1)

;; Don't blink the paren matching the one at point.
(setq blink-matching-paren nil)

;; Don't stretch the cursor to fit wide characters.
(setq x-stretch-cursor nil)

;;; Fringe.

;; Reduce the clutter in the fringes.
(setq indicate-buffer-boundaries nil
      indicate-empty-lines nil)

;; Frames.

(setq frame-title-format '("%b – Emacs")
      icon-title-format frame-title-format)

;; "I am once again asking you to stop showing toolbars."
(unless (assq 'menu-bar-lines default-frame-alist)
  (add-to-list 'default-frame-alist '(menu-bar-lines . 0))
  (add-to-list 'default-frame-alist '(tool-bar-lines . 0))
  (add-to-list 'default-frame-alist '(vertical-scroll-bars)))
(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)

;; The native border "consumes" a pixel of the fringe on righter-most splits,
;; `window-divider' does not.
(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)

;; Avoid GUI.
(setq use-dialog-box nil)
;; Don't display floating tooltips; display their contents in the echo-area,
;; because native tooltips are ugly.
(when (bound-and-true-p tooltip-mode)
  (tooltip-mode -1))

;; Favor vertical splits over horizontal ones.
(setq split-width-threshold 160
      split-height-threshold nil)

;;; Minibuffer.

(setq enable-recursive-minibuffers t)

;; Show current key-sequence in minibuffer.
(setq echo-keystrokes 0.02)

;; Expand the minibuffer to fit multi-line text displayed in the echo-area.
(setq resize-mini-windows 'grow-only
      ;; Don't let the minibuffer grow beyond this size.
      max-mini-window-height 0.15)

;; I think whoever came out with this default doesn't value their time.
(advice-add #'yes-or-no-p :override #'y-or-n-p)

;; Try really hard to keep the cursor from getting stuck in the read-only prompt
;; portion of the minibuffer.
(setq minibuffer-prompt-properties '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;;; Line numbers

;; Explicitly define a width to reduce computation.
(setq-default display-line-numbers-width 3)

;; Show absolute line numbers for narrowed regions.
(setq-default display-line-numbers-widen t)

;; Enable line numbers in most text-editing modes.
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)
(add-hook 'conf-mode-hook #'display-line-numbers-mode)

;;; Built-in packages.

(use-package ansi-color
  :init (setq ansi-color-for-comint-mode t))

;; TODO Move to `use-package' somehow.
(with-eval-after-load 'comint
  (setq comint-prompt-read-only t))

(use-package compile
  :config
  (setq compilation-always-kill t
        compilation-ask-about-save nil
        compilation-scroll-output 'first-error)
  ;; Handle ANSI codes in compilation buffer.
  (add-hook 'compilation-filter-hook
    (lambda (&rest _)
      (with-silent-modifications
        (ansi-color-apply-on-region compilation-filter-start (point))))))

(use-package ediff
  :config
  (setq ediff-diff-options "-w"
        ediff-split-window-function #'split-window-horizontally
        ediff-window-setup-function #'ediff-setup-windows-plain)

  (defvar e-ediff-saved-wconf nil)
  ;; Restore window config after quitting ediff.
  (add-hook 'ediff-before-setup-hook
    (lambda (&rest _)
      (setq e-ediff-saved-wconf (current-window-configuration))))
  (add-hook 'ediff-quit-hook :append
    (lambda (&rest _)
      (when (window-configuration-p e-ediff-saved-wconf)
        (set-window-configuration e-ediff-saved-wconf)))))

(use-package hl-line
  :disabled ; FIXME Temporary disabled.
  :hook ((prog-mode text-mode conf-mode special-mode) . hl-line-mode)
  :config
  ;; Not having to render the hl-line overlay in multiple buffers offers a tiny
  ;; performance boost.
  (setq hl-line-sticky-flag nil
        global-hl-line-sticky-flag nil)

  ;; Temporarily disable `hl-line' when selection is active.
  (defvar e-hl-line-mode nil)
  (add-hook 'evil-visual-state-entry-hook
    (lambda (&rest _)
      (when hl-line-mode
        (setq-local e-hl-line-mode t)
        (hl-line-mode -1))))
  (add-hook 'evil-visual-state-exit-hook
    (lambda (&rest _)
      (when e-hl-line-mode
        (hl-line-mode +1)))))

(use-package winner
  :hook (emacs-startup . winner-mode))

(use-package paren
  :hook (emacs-startup . show-paren-mode)
  :config
  (setq show-paren-delay 0.1
        show-paren-highlight-openparen t
        show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t))

(use-package whitespace
  :config
  (setq whitespace-line-column nil
        whitespace-style
        '(face indentation tabs tab-mark spaces space-mark newline newline-mark
          trailing lines-tail)
        whitespace-display-mappings
        '((tab-mark ?\t [?› ?\t])
          (newline-mark ?\n [?¬ ?\n])
          (space-mark ?\  [?·] [?.])))

  (add-function :around whitespace-enable-predicate
    (lambda (orig-fn &rest _)
      (unless (frame-parameter nil 'parent-frame)
        (funcall orig-fn)))))

;;; External packages.

(use-package hide-mode-line
  :hook (Man-mode . hide-mode-line-mode)
  :hook (completion-list-mode . hide-mode-line-mode))

(use-package highlight-numbers
  :hook ((prog-mode conf-mode) . highlight-numbers-mode)
  :config (setq highlight-numbers-generic-regexp "\\_<[[:digit:]]+\\(?:\\.[0-9]*\\)?\\_>"))

(use-package rainbow-delimiters
  :init (setq rainbow-delimiters-max-face-count 3))

(use-package restart-emacs)

;;
;;; Keybnidings.
;;

(defvar e-leader-key "SPC")
(defvar e-leader-alt-key "M-SPC")
(defvar e-localleader-key "SPC m")
(defvar e-localleader-alt-key "M-SPC m")

(defvar e-leader-map (make-sparse-keymap))

;;; Universal ESC.

(defvar e-escape-hook nil)
(defun e-escape ()
  (interactive)
        ;; Quit the minibuffer if open.
  (cond ((minibuffer-window-active-p (minibuffer-window))
         (abort-recursive-edit))
        ;; Run all escape hooks. If any returns non-nil, then stop there.
        ((run-hook-with-args-until-success 'e-escape-hook))
        ;; Don't abort macros.
        ((or defining-kbd-macro executing-kbd-macro) nil)
        ;; Back to the default.
        ((keyboard-quit))))
(global-set-key [remap keyboard-quit] #'e-escape)

(use-package general
  :init
  ;; Convenience aliases.
  (defalias 'define-key! #'general-def)
  (defalias 'undefine-key! #'general-unbind)
  :config
  (defmacro define-leader-key! (&rest args)
    `(general-define-key
      :states nil
      :wk-full-keys nil
      :keymaps 'e-leader-map
      ,@args))

  (defmacro define-localleader-key! (&rest args)
    `(general-define-key
      :states '(normal visual motion emacs insert)
      :major-modes t
      :prefix e-localleader-key
      :non-normal-prefix e-localleader-alt-key
      ,@args)))

(use-package which-key
  :hook (emacs-startup . which-key-mode)
  :init
  (setq which-key-sort-order #'which-key-prefix-then-key-order
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 6
        which-key-side-window-slot -10)
  :config
  (which-key-add-key-based-replacements e-leader-key "<leader>")
  (which-key-add-key-based-replacements e-localleader-key "<localleader>"))

;;
;;; Projectile.
;;

(defvar e-projectile-cache-limit 10000)
(defvar e-projectile-cache-blacklist '("~" "/tmp" "/"))
(defvar e-projectile-cache-purge-non-projects nil)
(defvar e-projectile-fd-binary (cl-find-if #'executable-find (list "fdfind" "fd")))

(use-package projectile
  :commands (projectile-project-root
             projectile-project-name
             projectile-project-p
             projectile-locate-dominating-file)
  :init
  (setq projectile-cache-file (concat e-cache-dir "projectile.cache")
        projectile-auto-discover nil
        projectile-enable-caching t
        projectile-kill-buffers-filter 'kill-only-files
        projectile-known-projects-file (concat e-cache-dir "projectile.projects")
        projectile-ignored-projects '("~/" "/tmp" "/"))

  (global-set-key [remap evil-jump-to-tag] #'projectile-find-tag)
  (global-set-key [remap find-tag]         #'projectile-find-tag)

  :config
  (projectile-mode +1)

  ;; Return project root
  (defun e-project-root (&optional dir)
    (let ((projectile-project-root (unless dir projectile-project-root))
          projectile-require-project-root)
      (projectile-project-root dir)))

  ;; Auto-discovery on `projectile-mode' is slow.
  (add-hook 'projectile-relevant-known-projects #'projectile-cleanup-known-projects)
  (add-hook 'projectile-relevant-known-projects #'projectile-discover-projects-in-search-path)

  ;; Reduce the number of project root marker files/directories projectile
  ;; searches for when resolving the project root.
  (setq projectile-project-root-files-bottom-up
        (append '(".projectile"
                  ".git")
                (when (executable-find "hg")
                  '(".hg"))
                (when (executable-find "bzr")
                  '(".bzr")))
        projectile-project-root-files '()
        projectile-project-root-files-top-down-recurring '("Makefile"))

  (push (abbreviate-file-name e-local-dir) projectile-globally-ignored-directories)

  ;; Treat current directory in dired as a "file in a project" and track it.
  (add-hook 'dired-before-readin-hook #'projectile-track-known-projects-find-file-hook)

  ;; Accidentally indexing big directories like $HOME or / will massively bloat
  ;; projectile's cache (into the hundreds of MBs). This purges those entries
  ;; when exiting Emacs to prevent slowdowns/freezing when cache files are
  ;; loaded or written to.
  (add-hook 'kill-emacs-hook
    (lambda (&rest _)
      (when (and (bound-and-true-p projectile-projects-cache)
                 projectile-enable-caching)
        (projectile-cleanup-known-projects)
        (cl-loop with blacklist = (mapcar #'file-truename e-projectile-cache-blacklist)
                 for proot in (hash-table-keys projectile-projects-cache)
                 if (or (not (stringp proot))
                        (>= (length (gethash proot projectile-projects-cache))
                            e-projectile-cache-limit)
                        (member (substring proot 0 -1) blacklist)
                        (and e-projectile-cache-purge-non-projects
                             (not (e-project-p proot))))
                 do (remhash proot projectile-projects-cache)
                 and do (remhash proot projectile-projects-cache-time)
                 and do (remhash proot projectile-project-type-cache))
        (projectile-serialize-cache))))

  ;; Disable bottom-up root searching to prevent issues with root resolution on
  ;; HOME directory.
  (let ((default-directory "~"))
    (when (cl-find-if #'projectile-file-exists-p
                      projectile-project-root-files-bottom-up)
      (setq projectile-project-root-files
            (append projectile-project-root-files-bottom-up
                    projectile-project-root-files)
            projectile-project-root-files-bottom-up nil)))

  ;; Only use `fd' or `ripgrep'.
  (advice-add #'projectile-get-ext-command :override
    (lambda (vcs &rest _)
      (if (functionp projectile-generic-command)
          (funcall projectile-generic-command vcs)
        projectile-generic-command)))
  (setq projectile-git-submodule-command nil
        projectile-indexing-method 'hybrid
        projectile-generic-command
        (lambda (&rest _)
          (let ((find-exe-fn #'executable-find))
            (cond
             ((when-let
                  (bin (if (ignore-errors (file-remote-p default-directory nil t))
                           (cl-find-if find-exe-fn (list "fdfind" "fd"))
                         e-projectile-fd-binary))
                (concat (format "%s . -0 -H -E .git --color=never --type file --type symlink --follow"
                                bin))))
             ((funcall find-exe-fn "rg")
              (concat "rg -0 --files --follow --color=never --hidden"
                      (cl-loop for dir in projectile-globally-ignored-directories
                               concat " --glob "
                               concat (shell-quote-argument (concat "!" dir)))
                      (if IS-WINDOWS " --path-separator /")))
             ("find . -type f -print0")))))

  ;; Ignore "unknown project" errors.
  (advice-add #'projectile-default-generic-command :around
    (lambda (orig-fn &rest args))
      (ignore-errors (apply orig-fn args))))

;;
;;; Dired.
;;

(with-eval-after-load 'dired
  (setq dired-auto-revert-buffer t
        dired-dwim-target t
        dired-hide-details-hide-symlink-targets nil
        ;; Always copy/delete recursively.
        dired-recursive-copies  'always
        dired-recursive-deletes 'top
        image-dired-dir (concat e-cache-dir "image-dired/")
        image-dired-db-file (concat image-dired-dir "db.el")
        image-dired-gallery-dir (concat image-dired-dir "gallery/")
        image-dired-temp-image-file (concat image-dired-dir "temp-image")
        image-dired-temp-rotate-image-file (concat image-dired-dir "temp-rotate-image")
        image-dired-thumb-size 150)

  (let ((args (list "-ahl" "-v" "--group-directories-first")))
    (setq dired-listing-switches (string-join args " "))

    (add-hook 'dired-mode-hook
      (lambda (&rest _)
        (when (file-remote-p default-directory)
          (setq-local dired-actual-switches (car args))))))

  ;; Don't complain about this command being disabled.
  (put 'dired-find-alternate-file 'disabled nil))

(use-package dired-rsync
  :general (dired-mode-map "C-c C-r" #'dired-rsync))

(use-package diredfl
  :hook (dired-mode . diredfl-mode))

(use-package diff-hl
  :hook (dired-mode . diff-hl-dired-mode-unless-remote)
  :hook (magit-post-refresh . diff-hl-magit-post-refresh)
  :config
  ;; Use margin instead of fringe.
  (diff-hl-margin-mode))

(use-package fd-dired
  :when e-projectile-fd-binary
  :defer t
  :init
  (global-set-key [remap find-dired] #'fd-dired))

;;
;;; VC.
;;

(with-eval-after-load 'vc-annotate
  (define-key vc-annotate-mode-map [remap quit-window] #'kill-current-buffer))

(use-package git-timemachine
  :init
  (setq git-timemachine-show-minibuffer-details t)
  :config
  (advice-add #'git-timemachine--show-minibuffer-details :override
    (lambda (revision)
      (let* ((date-relative (nth 3 revision))
            (date-full (nth 4 revision))
            (author (if git-timemachine-show-author (concat (nth 6 revision) ": ") ""))
            (sha-or-subject (if (eq git-timemachine-minibuffer-detail 'commit) (car revision) (nth 5 revision))))
        (setq header-line-format
              (format "%s%s [%s (%s)]"
                      (propertize author 'face 'git-timemachine-minibuffer-author-face)
                      (propertize sha-or-subject 'face 'git-timemachine-minibuffer-detail-face)
                      date-full date-relative)))))

  (with-eval-after-load 'evil
    (add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps)))

(use-package git-commit
  :hook (emacs-startup . global-git-commit-mode)
  :config
  ;; https://chris.beams.io/posts/git-commit/
  (setq git-commit-summary-max-length 50
        git-commit-style-convention-checks '(overlong-summary-line non-empty-second-line))
  (add-hook 'git-commit-mode-hook
    (lambda (&rest _)
      (setq qfill-column 72)))

  (add-hook 'git-commit-setup-hook
    (lambda (&rest _)
      (when (and (bound-and-true-p evil-mode)
                 (bobp) (eolp))
        (evil-insert-state)))))

(use-package browse-at-remote
  :init (setq browse-at-remote-add-line-number-if-no-region-selected nil))

(use-package gitconfig-mode)

(use-package gitignore-mode)

;;
;;; Undo.
;;

(use-package undo-fu
  :hook (emacs-startup . undo-fu-mode)
  :init
  (with-eval-after-load 'undo-tree
    (global-undo-tree-mode -1))
  :config
  (setq undo-limit 400000
        undo-strong-limit 3000000
        undo-outer-limit 3000000)

  (define-minor-mode undo-fu-mode
    :keymap (let ((map (make-sparse-keymap)))
              (define-key map [remap undo] #'undo-fu-only-undo)
              (define-key map [remap redo] #'undo-fu-only-redo)
              (define-key map (kbd "C-_")     #'undo-fu-only-undo)
              (define-key map (kbd "M-_")     #'undo-fu-only-redo)
              (define-key map (kbd "C-M-_")   #'undo-fu-only-redo-all)
              (define-key map (kbd "C-x r u") #'undo-fu-session-save)
              (define-key map (kbd "C-x r U") #'undo-fu-session-recover)
              map)
    :init-value nil
    :global t))

(use-package undo-fu-session
  :hook (undo-fu-mode . global-undo-fu-session-mode)
  :preface
  (setq undo-fu-session-directory (concat e-cache-dir "undo-fu-session/")
        undo-fu-session-incompatible-files '("\\.gpg$" "/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))

  (with-eval-after-load 'undo-fu-session
    (when (executable-find "zstd")
      (advice-add #'undo-fu-session--make-file-name :filter-return
        (lambda (filename)
          (if undo-fu-session-compression
              (concat (file-name-sans-extension filename) ".zst")
            filename))))))
;;
;;; IBuffer.
;;

(with-eval-after-load 'ibuffer
  (setq ibuffer-show-empty-filter-groups nil
        ibuffer-filter-group-name-face '(:inherit (success bold)))

  ;; Redefine size column to display human readable size.
  (define-ibuffer-column size
    (:name "Size"
     :inline t
     :header-mouse-map ibuffer-size-header-map)
    (file-size-human-readable (buffer-size)))

  (advice-add #'ibuffer-find-file :override
    (lambda (_file &optional _wildcards)
      (interactive)
      (counsel-find-file
        (let ((buf (ibuffer-current-buffer)))
          (if (buffer-live-p buf)
              (with-current-buffer buf
                default-directory)
            default-directory))))))

(use-package ibuffer-projectile
  :hook (ibuffer . ibuffer-projectile-set-filter-groups))

(use-package ibuffer-vc)

;;
;;; EViL.
;;

(use-package evil
  :hook (emacs-startup . evil-mode)
  :demand t
  :preface
  (setq evil-ex-interactive-search-highlight 'selected-window
        evil-ex-search-vim-style-regexp t
        evil-ex-visual-char-range t
        evil-kbd-macro-suppress-motion-error t
        evil-mode-line-format 'nil
        evil-symbol-word-search t
        evil-want-C-g-bindings t
        evil-want-C-i-jump (or (daemonp) (display-graphic-p))
        evil-want-C-u-delete t
        evil-want-C-u-scroll t
        evil-want-C-w-delete t
        evil-want-C-w-scroll t
        evil-want-Y-yank-to-eol t
        evil-want-abbrev-expand-on-insert-exit nil
        evil-want-visual-char-semi-exclusive t
        evil-visual-state-cursor 'box
        evil-default-cursor      'box
        evil-normal-state-cursor 'box
        evil-emacs-state-cursor  'box
        evil-insert-state-cursor 'box)
  ;; For `evil-collection`.
  (setq evil-want-integration t
        evil-want-keybinding nil)
  :config
  (evil-select-search-module 'evil-search-module 'evil-search)

  (put 'evil-define-key* 'lisp-indent-function 'defun)

  ;; https://bitbucket.org/lyro/evil/issue/336/osx-visual-state-copies-the-region-on
  ;; http://stackoverflow.com/questions/15873346/elisp-rename-macro
  (advice-add #'evil-visual-update-x-selection :override #'ignore)

  ;; Start help-with-tutorial in Emacs state.
  (advice-add #'help-with-tutorial :after
    (lambda (&rest _)
      (evil-emacs-state +1))))

(use-package evil-collection
  :after evil
  :init (setq evil-collection-company-use-tng t)
  :config (evil-collection-init))

(use-package evil-args)

(use-package evil-easymotion
  :commands (evilem-create evilem-default-keybindings)
  :config
  ;; Use `evil-search' backend, instead of `isearch'.
  (evilem-make-motion evilem-motion-search-next #'evil-ex-search-next
                      :bind ((evil-ex-search-highlight-all nil)))
  (evilem-make-motion evilem-motion-search-previous #'evil-ex-search-previous
                      :bind ((evil-ex-search-highlight-all nil)))
  (evilem-make-motion evilem-motion-search-word-forward #'evil-ex-search-word-forward
                      :bind ((evil-ex-search-highlight-all nil)))
  (evilem-make-motion evilem-motion-search-word-backward #'evil-ex-search-word-backward
                      :bind ((evil-ex-search-highlight-all nil))))

(use-package evil-embrace
  :commands (embrace-add-pair embrace-add-pair-regexp)
  :hook (LaTeX-mode . embrace-LaTeX-mode-hook)
  :hook (org-mode . embrace-org-mode-hook)
  :hook (emacs-lisp-mode . embrace-emacs-lisp-mode-hook)
  :init
  (with-eval-after-load 'evil-surround
    (evil-embrace-enable-evil-surround-integration))
  :config
  (setq evil-embrace-show-help-p nil))

(use-package evil-escape
  :straight (:host github :repo "hlissner/evil-escape")
  :commands evil-escape
  :init
  (setq evil-escape-excluded-states '(normal visual multiedit emacs motion)
        evil-escape-excluded-major-modes '(neotree-mode treemacs-mode vterm-mode)
        evil-escape-key-sequence "jk"
        evil-escape-delay 0.15)
  (evil-define-key* '(insert replace visual operator) 'global "\C-g" #'evil-escape)
  :config
  ;; No `evil-escape' in minibuffer.
  (add-hook 'evil-escape-inhibit-functions
    (lambda (&rest _)
      (and (minibufferp)
           (or (not (bound-and-true-p evil-collection-setup-minibuffer))
               (evil-normal-state-p)))))
  ;; In case `evil-mc' needs `evil-escape'.
  (evil-escape-mode +1))

(use-package evil-exchange
  :commands evil-exchange
  :config
  (add-hook 'e-escape-hook
    (lambda (&rest _)
      (when evil-exchange--overlays
        (evil-exchange-cancel)
        t))))

(use-package evil-quick-diff
  :straight (:host github :repo "rgrinberg/evil-quick-diff")
  :commands (evil-quick-diff evil-quick-diff-cancel))

(use-package evil-indent-plus)

(use-package evil-lion)

(use-package evil-nerd-commenter
  :commands (evilnc-comment-operator
             evilnc-inner-comment
             evilnc-outer-commenter))

(use-package evil-snipe
  :commands (evil-snipe-mode
             evil-snipe-override-mode
             evil-snipe-local-mode
             evil-snipe-override-local-mode)
  :init
  (setq evil-snipe-smart-case t
        evil-snipe-scope 'line
        evil-snipe-repeat-scope 'visible
        evil-snipe-char-fold t)
  :config
  (push 'Info-mode evil-snipe-disabled-modes)
  (push 'calc-mode evil-snipe-disabled-modes)
  (push 'treemacs-mode evil-snipe-disabled-modes)

  (evil-snipe-mode +1)
  (evil-snipe-override-mode +1))

(use-package evil-surround
  :commands (global-evil-surround-mode
             evil-surround-edit
             evil-Surround-edit
             evil-surround-region)
  :config (global-evil-surround-mode 1))

(use-package evil-traces
  :after evil-ex
  :config
  (evil-traces-mode))

(use-package evil-visualstar
  :commands (evil-visualstar/begin-search
             evil-visualstar/begin-search-forward
             evil-visualstar/begin-search-backward)
  :init
  (evil-define-key* 'visual 'global
    "*" #'evil-visualstar/begin-search-forward
    "#" #'evil-visualstar/begin-search-backward))

;;
;;; Ivy.
;;

(use-package counsel
  :defer t
  :init
  (define-key!
    [remap apropos]                  #'counsel-apropos
    [remap bookmark-jump]            #'counsel-bookmark
    [remap describe-bindings]        #'counsel-descbinds
    [remap describe-face]            #'counsel-faces
    [remap describe-function]        #'counsel-describe-function
    [remap describe-variable]        #'counsel-describe-variable
    [remap evil-ex-registers]        #'counsel-evil-registers
    [remap evil-show-marks]          #'counsel-mark-ring
    [remap execute-extended-command] #'counsel-M-x
    [remap find-file]                #'counsel-find-file
    [remap find-library]             #'counsel-find-library
    [remap imenu]                    #'counsel-imenu
    [remap info-lookup-symbol]       #'counsel-info-lookup-symbol
    [remap load-theme]               #'counsel-load-theme
    [remap locate]                   #'counsel-locate
    [remap org-goto]                 #'counsel-org-goto
    [remap org-set-tags-command]     #'counsel-org-tag
    [remap recentf-open-files]       #'counsel-recentf
    [remap set-variable]             #'counsel-set-variable
    [remap swiper]                   #'counsel-grep-or-swiper
    [remap unicode-chars-list-chars] #'counsel-unicode-char
    [remap yank-pop]                 #'counsel-yank-pop)
  :config
  ;; Don't use ^ as initial input. Set this here because `counsel' defines more
  ;; of its own, on top of the defaults.
  (setq ivy-initial-inputs-alist nil)

  ;; Integrate with `helpful'.
  (setq counsel-describe-function-function #'helpful-callable
        counsel-describe-variable-function #'helpful-variable)

  (add-hook 'counsel-grep-post-action-hook #'better-jumper-set-jump)
  (add-hook 'counsel-grep-post-action-hook #'recenter)

  ;; Make `counsel-compile' projectile-aware.
  (add-to-list 'counsel-compile-root-functions #'projectile-project-root)
  (with-eval-after-load 'savehist
    (add-to-list 'savehist-additional-variables 'counsel-compile-history))

  ;; Sort it by appearance in page.
  (add-to-list 'ivy-sort-functions-alist '(counsel-imenu))

  ;; `counsel-find-file'.
  (dolist (fn '(counsel-rg counsel-find-file))
    (ivy-add-actions
     fn '(("p" (lambda (path) (with-ivy-window (insert (file-relative-name path default-directory))))
           "insert relative path")
          ("P" (lambda (path) (with-ivy-window (insert path)))
           "insert absolute path")
          ("l" (lambda (path) (with-ivy-window (insert (format "[[./%s]]" (file-relative-name path default-directory)))))
           "insert relative org-link")
          ("L" (lambda (path) (with-ivy-window (insert (format "[[%s]]" path))))
           "Insert absolute org-link"))))

  (ivy-add-actions 'counsel-file-jump (plist-get ivy--actions-list 'counsel-find-file))

  (advice-add #'counsel--find-return-list :override
    (lambda (args &rest _)
      (cl-destructuring-bind (find-program . args)
          (cond ((when-let (fd (executable-find (or e-projectile-fd-binary "fd")))
                  (append (list fd
                                "--color=never" "-E" ".git"
                                "--type" "file" "--type" "symlink" "--follow")
                          (if IS-WINDOWS '("--path-separator=/")))))
                ((executable-find "rg")
                (append (list "rg" "--files" "--follow" "--color=never" "--hidden" "--no-messages")
                        (cl-loop for dir in projectile-globally-ignored-directories
                                  collect "--glob"
                                  collect (concat "!" dir))))
                ((cons find-program args)))
        (counsel--call
        (cons find-program args)
        (lambda (&rest _)
          (goto-char (point-min))
          (let (files)
            (while (< (point) (point-max))
              (push (buffer-substring (line-beginning-position) (line-end-position))
                    files)
              (forward-line 1))
            (nreverse files))))))))

(use-package counsel-projectile
  :defer t
  :init
  (define-key!
    [remap projectile-find-dir]         #'counsel-projectile-find-dir
    [remap projectile-switch-to-buffer] #'counsel-projectile-switch-to-buffer
    [remap projectile-grep]             #'counsel-projectile-grep
    [remap projectile-ag]               #'counsel-projectile-ag
    [remap projectile-switch-project]   #'counsel-projectile-switch-project)
  :config
  ;; No highlighting of visited files.
  (ivy-set-display-transformer #'counsel-projectile-find-file nil)

  (setq counsel-projectile-sort-files t))

(use-package ivy
  :hook (emacs-startup . ivy-mode)
  :init
  (let ((standard-search-fn #'e-ivy-prescient-non-fuzzy)
        (alt-search-fn      #'ivy--regex-fuzzy))
    (setq ivy-re-builders-alist
          `((counsel-rg     . ,standard-search-fn)
            (swiper         . ,standard-search-fn)
            (swiper-isearch . ,standard-search-fn)
            (t . ,alt-search-fn))
          ivy-more-chars-alist
          '((counsel-rg . 1)
            (counsel-search . 2)
            (t . 3))))
  :config
  ;; Relaxed search.
  (setq ivy-sort-max-size 7500)

  ;; Load `counsel' early.
  (require 'counsel nil t)

  (setq ivy-height 17
        ivy-wrap t
        ivy-fixed-height-minibuffer t
        projectile-completion-system 'ivy
        ivy-magic-slash-non-match-action nil
        ivy-use-virtual-buffers nil
        ivy-virtual-abbreviate 'full
        ivy-on-del-error-function #'ignore
        ivy-use-selectable-prompt t)

  ;; Disable in `evil-ex'.
  (advice-add #'evil-ex :around
    (lambda (orig-fn &rest args)
      (let ((completion-in-region-function #'completion--in-region))
        (apply orig-fn args))))

  (define-key! ivy-minibuffer-map
    "C-o" #'ivy-dispatching-done
    "M-o" #'hydra-ivy/body))

(use-package ivy-rich
  :after ivy
  :config
  (setq ivy-rich-parse-remote-buffer nil)

  (ivy-rich-mode +1))

(use-package ivy-hydra)

(use-package ivy-prescient
  :hook (ivy-mode . ivy-prescient-mode)
  :hook (ivy-prescient-mode . prescient-persist-mode)
  :init
  (setq prescient-filter-method '(literal regexp initialism fuzzy))
  :config
  (setq ivy-prescient-sort-commands
        '(:not counsel-ag
               counsel-buffer-or-recentf
               counsel-git-grep
               counsel-grep
               counsel-imenu
               counsel-recentf
               counsel-rg
               counsel-yank-pop
               ivy-switch-buffer
               swiper
               swiper-isearch)
        ivy-prescient-retain-classic-highlighting t)

  (defun e-ivy-prescient-non-fuzzy (str)
    (let ((prescient-filter-method '(literal regexp)))
      (ivy-prescient-re-builder str)))

  ;; NOTE prescient config duplicated with `company'.
  (setq prescient-save-file (concat e-cache-dir "prescient-save.el")))

(use-package flx
  :defer t
  :init (setq ivy-flx-limit 10000))

(use-package wgrep
  :commands wgrep-change-to-wgrep-mode
  :config (setq wgrep-auto-save-buffer t))

(use-package swiper
  :init (setq swiper-action-recenter t))

(use-package amx
  :init (setq amx-save-file (concat e-cache-dir "amx-items")))

;;
;;; Company.
;;

(use-package company
  :hook (emacs-startup . global-company-mode)
  :commands (company-complete-common company-manual-begin company-grab-line)
  :init
  (setq company-idle-delay 0.25
        company-minimum-prefix-length 2
        company-tooltip-limit 14
        company-tooltip-align-annotations t
        company-require-match 'never
        company-global-modes '(not erc-mode message-mode help-mode gud-mode)
        company-frontends '(company-pseudo-tooltip-frontend company-echo-metadata-frontend)
        company-backends '(company-capf)
        company-dabbrev-other-buffers nil
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil)

  :config
  (add-hook 'company-mode-hook #'evil-normalize-keymaps)

  (add-hook 'evil-normal-state-entry-hook
    (lambda (&rest _)
      (when company-candidates
        (company-abort))))

  (advice-add #'company-begin-backend :before
    (lambda (&rest _)
      (company-abort)))

  (defvar e-company-backend-alist
    '((text-mode company-dabbrev company-yasnippet company-ispell)
      (prog-mode company-capf company-yasnippet)
      (conf-mode company-capf company-dabbrev-code company-yasnippet)))
  (defun set-company-backend! (modes &rest backends)
    (declare (indent defun))
    (dolist (mode (e-enlist modes))
      (if (null (car backends))
          (setq e-company-backend-alist
                (delq (assq mode e-company-backend-alist)
                      e-company-backend-alist))
        (setf (alist-get mode e-company-backend-alist)
              backends)))))

;;
;;; Ido.
;;

(use-package ido
  :hook (emacs-startup . ido-mode)
  :hook (ido-mode . ido-everywhere)
  :hook (ido-mode . ido-ubiquitous-mode)
  :preface
  ;; Define the hook manually.
  (advice-add #'ido-mode :after
    (lambda (&rest _)
      (run-hooks 'ido-mode-hook)))
  :init
  (setq ido-save-directory-list-file (concat e-cache-dir "ido.last"))
  :config
  (push "\\`.DS_Store$" ido-ignore-files)
  (push "Icon\\?$" ido-ignore-files)
  (setq ido-ignore-buffers '(" output\\*$"
                             "\\` "
                             "^TAGS$"
                             "^\*Ido"
                             "^\\*.*Completions\\*$"
                             "^\\*Buffer"
                             "^\\*ESS\\*"
                             "^\\*Ediff"
                             "^\\*Messages\\*"
                             "^\\*[Hh]elp"
                             "^\\*cvs-"
                             "^\\*tramp"
                             "_region_")
        ido-auto-merge-work-directories-length -1
        ido-confirm-unique-completion t
        ido-case-fold t
        ido-create-new-buffer 'always
        ido-enable-flex-matching t))


(use-package ido-completing-read-plus+
  :straight (:host github :repo "DarwinAwardWinner/ido-completing-read-plus")
  :hook (ido-mode . ido-ubiquitous-mode))

(use-package ido-vertical-mode
  :hook ido-mode
  :config (setq ido-vertical-show-count t))

(use-package ido-sort-mtime
  :hook (ido-mode . ido-sort-mtime-mode))

(use-package crm-custom
  :hook (ido-mode . crm-custom-mode))

(use-package flx-ido
  :hook (ido-mode . flx-ido-mode))

;;
;;; Flyspell.
;;

(defvar ispell-dictionary "en_US")

(use-package ispell
  :config
  (pcase (cond ((executable-find "aspell")   'aspell)
               ((executable-find "hunspell") 'hunspell))
    (`aspell
     (setq ispell-program-name "aspell"
           ispell-extra-args '("--sug-mode=ultra" "--run-together" "--dont-tex-check-comments"))

     (add-hook 'text-mode-hook
       (lambda (&rest _)
         (setq-local ispell-extra-args (remove "--run-together" ispell-extra-args))))

     (defun e-spell-init-ispell-extra-args-a (orig-fun &rest args)
       :around '(ispell-word flyspell-auto-correct-word)
       (let ((ispell-extra-args (remove "--run-together" ispell-extra-args)))
         (ispell-kill-ispell t)
         (apply orig-fun args)
         (ispell-kill-ispell t))))

    (`hunspell
     (setq ispell-program-name "hunspell"))))

(use-package flyspell
  :defer t
  :preface
  :init
  (add-hook #'flyspell-mode '(org-mode-hook
                              markdown-mode-hook
                              TeX-mode-hook
                              rst-mode-hook
                              mu4e-compose-mode-hook
                              message-mode-hook
                              git-commit-mode-hook))
  (add-hook #'flyspell-prog-mode '(yaml-mode-hook
                                   conf-mode-hook
                                   prog-mode-hook))
  :config
  (setq flyspell-issue-welcome-flag nil
        flyspell-issue-message-flag nil))

(use-package flyspell-correct
  :commands flyspell-correct-previous
  :general ([remap ispell-word] #'flyspell-correct-at-point)
  :config (require 'flyspell-correct-ivy nil t))

(use-package flyspell-correct-ivy
  :after flyspell-correct)

(use-package flyspell-lazy
  :after flyspell
  :config
  (setq flyspell-lazy-idle-seconds 1
        flyspell-lazy-window-idle-seconds 3)
  (flyspell-lazy-mode +1))

(use-package langtool
  :when (executable-find "langaugetool")
  :commands (langtool-check
             langtool-check-done
             langtool-show-message-at-point
             langtool-correct-buffer)
  :init (setq langtool-default-language "en-US")
  :config (setq langtool-bin "/usr/bin/languagetool"))

(use-package writegood-mode
  :hook (org-mode markdown-mode rst-mode asciidoc-mode latex-mode))

;;
;;; Flycheck.
;;

(use-package flycheck
  :commands (flycheck-list-errors flycheck-buffer)
  :hook (emacs-startup . global-flycheck-mode)
  :config
  (setq flycheck-emacs-lisp-load-path 'inherit)

  ;; Check only when saving or opening files
  (setq flycheck-check-syntax-automatically '(save mode-enabled idle-buffer-switch))

  ;; For the above functionality, check syntax in a buffer that you switched to
  ;; only briefly.
  (setq flycheck-buffer-switch-check-intermediate-buffers t)

  ;; Display errors a little quicker.
  (setq flycheck-display-errors-delay 0.25)

  ;; ESC buffer.
  (add-hook 'e-escape-hook :append
    (lambda (&rest _)
      (when flycheck-mode
        (ignore-errors (flycheck-buffer))
        nil))))

(use-package flycheck-popup-tip
  :commands (flycheck-popup-tip-show-popup flycheck-popup-tip-delete-popup)
  :hook (flycheck-mode . e-flycheck-init-popups)
  :config
  (defun e-flycheck-init-popups (&rest _)
    (unless (and (bound-and-true-p lsp-ui-mode)
                 lsp-ui-sideline-enable)
        (flycheck-popup-tip-mode +1)))

  (with-eval-after-load 'evil
    ;; Don't display popups while in insert or replace mode, as it can affect
    ;; the cursor's position or cause disruptive input delays.
    (add-hook #'flycheck-popup-tip-delete-popup 'evil-insert-state-entry-hook)
    (add-hook #'flycheck-popup-tip-delete-popup 'evil-replace-state-entry-hook)
    (advice-add #'flycheck-popup-tip-show-popup :before-while
      (lambda (&rest _)
        (if evil-local-mode
            (eq evil-state 'normal)
          (not (bound-and-true-p company-backend)))))))

;;
;;; Magit.
;;

(use-package magit
  :commands magit-file-delete
  :init
  ;; Must be set early to prevent ~/.emacs.d/transient from being created
  (setq transient-levels-file  (concat e-etc-dir "transient/levels")
        transient-values-file  (concat e-etc-dir "transient/values")
        transient-history-file (concat e-etc-dir "transient/history"))
  :config
  (setq transient-default-level 5
        magit-diff-refine-hunk t
        magit-save-repository-buffers nil
        magit-revision-insert-related-refs nil)
  (add-hook 'magit-process-mode-hook #'goto-address-mode)

  ;; Center the target file.
  (advice-add #'magit-status-here :after #'recenter)

  (unless (file-exists-p "~/.git-credential-cache/")
    (setq magit-credential-cache-daemon-socket
          (e-glob (or (getenv "XDG_CACHE_HOME")
                         "~/.cache/")
                     "git/credential/socket")))

  ;; Prevent scrolling when manipulating `magit-status' hunks.
  (defvar e-magit-pos nil)
  (add-hook 'magit-pre-refresh-hook
    (lambda (&rest _)
      (setq-local e-magit-pos (list (current-buffer) (point) (window-start)))))
  (add-hook 'magit-post-refresh-hook
    (lambda (&rest _)
      (when (and e-magit-pos (eq (current-buffer) (car e-magit-pos)))
        (goto-char (cadr e-magit-pos))
        (set-window-start nil (caddr e-magit-pos) t)
        (kill-local-variable 'e-magit-pos))))

  ;; Add additional switches that seem common enough.
  (transient-append-suffix 'magit-fetch "-p"
    '("-t" "Fetch all tags" ("-t" "--tags")))
  (transient-append-suffix 'magit-pull "-r"
    '("-a" "Autostash" "--autostash"))

  ;; Close transient with ESC.
  (define-key transient-map [escape] #'transient-quit-one)

  ;; Optimization.
  (add-hook 'magit-status-mode-hook
    (lambda (&rest _)
      (when-let (path (executable-find magit-git-executable))
        (setq-local magit-git-executable path)))))

(use-package forge
  :commands (forge-create-pullreq forge-create-issue)
  :preface
  (setq forge-database-file (concat e-etc-dir "forge/forge-database.sqlite"))
  :config
  (advice-add #'forge-get-repository :before-while
    (lambda (&rest _)
      (file-executable-p emacsql-sqlite-executable)))

  (advice-add #'forge-dispatch :before
    (lambda (&rest _)
      (unless (file-executable-p emacsql-sqlite-executable)
        (emacsql-sqlite-compile 2)
        (if (not (file-executable-p emacsql-sqlite-executable))
            (message (concat "Failed to build emacsql; forge may not work correctly.\n"
                            "See *Compile-Log* buffer for details"))
          (setq forge--sqlite-available-p t)
          (magit-add-section-hook 'magit-status-sections-hook 'forge-insert-pullreqs nil t)
          (magit-add-section-hook 'magit-status-sections-hook 'forge-insert-issues   nil t)
          (with-eval-after-load 'forge-topic
            (dolist (hook forge-bug-reference-hooks)
              (add-hook hook #'forge-bug-reference-setup))))))))

(use-package github-review
  :after magit
  :config
  (defun e-magit-start-github-review (arg)
    (interactive "P")
    (call-interactively
      (if (or arg (not (featurep 'forge)))
          #'github-review-start
        #'github-review-forge-pr-at-point)))
  (transient-append-suffix 'magit-merge "i"
    '("y" "Review pull request" e-magit-start-github-review))
  (with-eval-after-load 'forge
    (transient-append-suffix 'forge-dispatch "c u"
      '("c r" "Review pull request" e-magit-start-github-review))))

(use-package magit-todos
  :after magit
  :config
  (setq magit-todos-keyword-suffix "\\(?:([^)]+)\\)?:?")
  (define-key magit-todos-section-map "j" nil))

(use-package magit-gitflow
  :hook (magit-mode . turn-on-magit-gitflow))

(use-package evil-magit
  :after magit
  :init
  (setq evil-magit-state 'normal
        evil-magit-use-z-for-folds t)
  :config
  (undefine-key! magit-mode-map
    ;; Replaced by z1, z2, z3, etc
    "M-1" "M-2" "M-3" "M-4"
    "1" "2" "3" "4"
    "0") ; moved to g=
  (evil-define-key* 'normal magit-status-mode-map [escape] nil)
  (evil-define-key* '(normal visual) magit-mode-map
    "%"  #'magit-gitflow-popup
    "zt" #'evil-scroll-line-to-top
    "zz" #'evil-scroll-line-to-center
    "zb" #'evil-scroll-line-to-bottom
    "g=" #'magit-diff-default-context
    "gi" #'forge-jump-to-issues
    "gm" #'forge-jump-to-pullreqs)
  (define-key! 'normal
    (magit-status-mode-map
     magit-stash-mode-map
     magit-revision-mode-map
     magit-diff-mode-map)
    [tab] #'magit-section-toggle)
  (with-eval-after-load 'git-rebase
    (dolist (key '(("M-k" . "gk") ("M-j" . "gj")))
      (when-let (desc (assoc (car key) evil-magit-rebase-commands-w-descriptions))
        (setcar desc (cdr key))))
    (evil-define-key* evil-magit-state git-rebase-mode-map
      "gj" #'git-rebase-move-line-down
      "gk" #'git-rebase-move-line-up))
  (transient-replace-suffix 'magit-dispatch 'magit-worktree
    '("%" "Gitflow" magit-gitflow-popup))
  (transient-append-suffix 'magit-dispatch '(0 -1 -1)
    '("*" "Worktree" magit-worktree)))

;;
;;; LSP.
;;

(use-package lsp-mode
  :commands lsp-install-server
  :init
  (setq lsp-session-file (concat e-etc-dir "lsp-session"))

  ;; For `lsp-clients'.
  (setq lsp-server-install-dir (concat e-etc-dir "lsp/"))

  ;; Disable text modificatoin.
  (setq lsp-enable-indentation nil
        lsp-enable-on-type-formatting nil)

  ;; Disable features that have great potential to be slow.
  (setq lsp-enable-file-watchers nil
        lsp-enable-folding nil
        lsp-enable-text-document-color nil)

  ;; Don't auto-kill LSP server when all buffers are closed.
  (setq lsp-keep-workspace-alive nil)

  :config
  (when lsp-auto-configure
    (mapc
      (lambda (package)
        (require package nil t))
      (cl-remove-if #'featurep lsp-client-packages)))

  (add-hook 'lsp-completion-mode-hook
    (lambda (&rest _)
      (when lsp-completion-mode
        (set (make-local-variable 'company-backends)
             (cons 'company-capf
                   (remove 'company-capf
                           (remq 'company-capf company-backends)))))))

  ;; Defer LSP server shutdown.
  (defvar e-lsp-defer-shutdown 5)
  (defvar e-lsp-deferred-shutdown-timer nil)
  (advice-add #'lsp--shutdown-workspace :around
    (lambda (orig-fn &optional restart)
      (if (or lsp-keep-workspace-alive
              restart
              (null e-lsp-defer-shutdown)
              (= e-lsp-defer-shutdown 0))
          (funcall orig-fn restart)
        (when (timerp e-lsp-deferred-shutdown-timer)
          (cancel-timer e-lsp-deferred-shutdown-timer))
        (setq e-lsp-deferred-shutdown-timer
              (run-at-time
              (if (numberp e-lsp-defer-shutdown) e-lsp-defer-shutdown 3)
              nil (lambda (workspace)
                    (let ((lsp--cur-workspace workspace))
                      (unless (lsp--workspace-buffers lsp--cur-workspace)
                        (funcall orig-fn))))
              lsp--cur-workspace))))))

(use-package lsp-ui
  :defer t
  :config
  (setq lsp-ui-doc-max-height 8
        lsp-ui-doc-max-width 35
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-sideline-show-hover nil))

(use-package lsp-ivy
  :commands (lsp-ivy-workspace-symbol lsp-ivy-global-workspace-symbol))

;;
;;; C/C++
;;

(use-package cc-mode
  :mode ("\\.h\\'" . e-cc-guess-mode)
  :hook (c-mode-common . rainbow-delimiters-mode)
  :hook ((c-mode-local-vars c++-mode-local-vars) . e-cc-init-ffap-integration)
  :hook ((c-mode-local-vars c++-mode-local-vars) . lsp)
  :config
  ;; Guess `cc-mode` minor (either C or C++) for the current header file.
  (defun e-cc-guess-mode (&rest _)
    (let ((base (file-name-sans-extension (buffer-file-name (buffer-base-buffer)))))
      (cond ((or (file-exists-p (concat base ".cpp"))
                 (file-exists-p (concat base ".cxx"))
                 (file-exists-p (concat base ".cc")))
             (c++-mode))
            ((file-exists-p (concat base ".c"))
             (c-mode))
            ((functionp 'c-mode)
             (funcall 'c-mode))
            ((c-mode)))))

  ;; Resolve include paths and integrate them in `ffap'.
  (defun e-cc-init-ffap-integration (&rest _)
    (when-let (project-root (and (featurep 'lsp)
                                 (or (lsp-workspace-root)
                                     (e-project-root))))
      (require 'ffap)
      (make-local-variable 'ffap-c-path)
      (make-local-variable 'ffap-c++-path)
      (cl-loop for dir in (or (cdr (assoc project-root nil))
                              (cl-loop with path = (or buffer-file-name default-directory)
                                      for dir in (list "inc" "include" "includes")
                                      if (file-name-absolute-p dir)
                                      collect dir
                                      else if (projectile-locate-dominating-file path dir)
                                      collect (expand-file-name dir it)))
              do (add-to-list (pcase major-mode
                                (`c-mode 'ffap-c-path)
                                (`c++-mode 'ffap-c++-path))
                              (expand-file-name dir project-root)))))

  (with-eval-after-load 'ffap
    (add-to-list 'ffap-alist '(c-mode . ffap-c-mode)))

  (setq c-basic-offset tab-width
        c-backspace-function #'delete-backward-char))

(use-package modern-cpp-font-lock
  :hook (c++-mode . modern-c++-font-lock-mode))

(use-package ccls
  :after lsp
  :init
  (with-eval-after-load 'projectile
    (add-to-list 'projectile-globally-ignored-directories ".ccls-cache")
    (add-to-list 'projectile-project-root-files-bottom-up ".ccls-root")
    (add-to-list 'projectile-project-root-files-top-down-recurring "compile_commands.json")))

(use-package demangle-mode
  :hook llvm-mode)

(use-package glsl-mode)

(use-package company-glsl
  :after glsl-mode
  :config (set-company-backend! 'glsl-mode 'company-glsl))

;;
;;; CMake.
;;

(use-package cmake-mode
  :straight (:host github :repo "emacsmirror/cmake-mode" :files (:defaults "*")))

(use-package company-cmake
  :straight (:host github :repo "purcell/company-cmake")
  :after cmake-mode
  :config (set-company-backend! 'cmake-mode 'company-cmake))

;;
;;; Lisp.
;;

(use-package lispy
  :hook ((lisp-mode
          emacs-lisp-mode
          scheme-mode) . lispy-mode)
  :config
  (setq lispy-close-quotes-at-end-p t)
  (add-hook 'lispy-mode-hook #'turn-off-smartparens-mode))


(use-package lispyville
  :hook (lispy-mode . lispyville-mode)
  :init
  (setq lispyville-key-theme
        '((operators normal)
          c-w
          (prettify insert)
          (atom-movement normal visual)
          slurp/barf-lispy
          additional
          additional-insert))
  :config
  (lispyville-set-key-theme))

(use-package macrostep)

(use-package highlight-quoted
  :hook ((lisp-mode
          emacs-lisp-mode
          scheme-mode) . highlight-quoted-mode))

(use-package parinfer
  :hook ((emacs-lisp-mode
          scheme-mode
          lisp-mode) . parinfer-mode)
  :init
  (setq parinfer-extensions
        '(defaults
          pretty-parens
          smart-tab
          smart-yank))
  (push 'evil parinfer-extensions))

;;; Common Lisp.

(defvar inferior-lisp-program "sbcl")

(with-eval-after-load 'lisp-mode
  (add-hook 'lisp-mode-hook 'rainbow-delimiters-mode))

(use-package sly
  :hook (lisp-mode-local-vars . sly-editing-mode)
  :init
  :config
  (setq sly-mrepl-history-file-name (concat e-cache-dir "sly-mrepl-history")
        sly-kill-without-query-p t
        sly-net-coding-system 'utf-8-unix
        sly-complete-symbol-function 'sly-flex-completions)

  (add-hook 'sly-mode-hook
    (lambda (&rest _)
      (cond ((sly-connected-p))
            ((executable-find inferior-lisp-program)
             (let ((sly-auto-start 'always))
               (sly-auto-start)
               (add-hook 'kill-buffer-hook
                  (lambda (&rest _)
                    (unless (cl-loop for buf in (delq (current-buffer) (buffer-list))
                                    if (and (buffer-local-value 'sly-mode buf)
                                            (get-buffer-window buf))
                                    return t)
                      (dolist (conn (sly--purge-connections))
                        (sly-quit-lisp-internal conn 'sly-quit-sentinel t))
                      (let (kill-buffer-hook kill-buffer-query-functions)
                        (mapc #'kill-buffer
                              (cl-loop for buf in (delq (current-buffer) (buffer-list))
                                      if (buffer-local-value 'sly-mode buf)
                                      collect buf)))))
                  nil t)))
            ((message "WARNING: Couldn't find `inferior-lisp-program' (%s)"
                      inferior-lisp-program)))))

  (add-hook 'sly-mode-hook #'evil-normalize-keymaps))

(use-package sly-macrostep
  :after macrostep)

(use-package sly-repl-ansi-color
  :defer t
  :init
  (add-to-list 'sly-contribs 'sly-repl-ansi-color))

;;; Emacs Lisp.

(with-eval-after-load 'elisp-mode
  (add-hook 'emacs-lisp-mode-hook #'outline-minor-mode)
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)

  ;; Recenter window after following definition.
  (advice-add #'elisp-def :after #'recenter)

  ;; Display variable value next to documentation in `eldoc'.
  (advice-add #'elisp-get-var-docstring :around
    (lambda (orig-fn sym)
      (when-let (ret (funcall orig-fn sym))
        (concat ret " "
                (let* ((truncated " [...]")
                      (print-escape-newlines t)
                      (str (symbol-value sym))
                      (str (prin1-to-string str))
                      (limit (- (frame-width) (length ret) (length truncated) 1)))
                  (format (format "%%0.%ds%%s" limit)
                          (propertize str 'face 'warning)
                          (if (< (length str) limit) "" truncated))))))))

(use-package ielm
  :defer t
  :config
  ;; Adapted from http://www.modernemacs.com/post/comint-highlighting/ to add
  ;; syntax highlighting to `ielm' REPLs.
  (add-hook 'ielm-mode-hook
    (lambda (&rest _)
      (font-lock-add-keywords
      nil (cl-loop for (matcher . match-highlights)
                    in (append lisp-el-font-lock-keywords-2 lisp-cl-font-lock-keywords-2)
                    collect
                    `((lambda (limit)
                        (and ,(if (symbolp matcher)
                                  `(,matcher limit)
                                `(re-search-forward ,matcher limit t))
                            (> (match-beginning 0) (car comint-last-prompt))
                            (let ((state (sp--syntax-ppss)))
                              (not (or (nth 3 state)
                                        (nth 4 state))))))
                      ,@match-highlights))))))

(use-package elisp-def)

;;; Scheme.

(use-package scheme
  :hook (scheme-mode . rainbow-delimiters-mode))

(use-package geiser
  :defer t
  :init
  (setq geiser-active-implementations '(chicken)
        geiser-smart-tab-p t))

;;
;;; Pass.
;;

(use-package auth-source-pass
  :straight (:host github :repo "DamienCassou/auth-password-store")
  :config (auth-source-pass-enable))

(use-package pass
  :config
  (define-key! pass-mode-map
    "j"    #'pass-next-entry
    "k"    #'pass-prev-entry
    "d"    #'pass-kill
    "\C-j" #'pass-next-directory
    "\C-k" #'pass-prev-directory))

(use-package password-store
  :init (setq password-store-password-length 20)
  :config
  (advice-add #'auth-source-pass--read-entry :override
    (lambda (entry)
      (with-temp-buffer
        (insert-file-contents
        (expand-file-name (format "%s.gpg" entry) (password-store-dir)))
        (buffer-substring-no-properties (point-min) (point-max))))))

(use-package ivy-pass)

(provide 'init)
;;; init.el ends here
