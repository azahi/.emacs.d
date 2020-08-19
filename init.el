;;; init.el -*- lexical-binding: t; -*-

;; Prevent garbage collection from running before invoking `ghcm-mode'.
(setq gc-cons-threshold most-positive-fixnum)

;;
;;; External libraries.
;;

(require 'cl)
(require 'subr-x)

;;
;;; Variables.
;;

;; Ensure running from file's directory.
(let (file-name-handler-alist)
  (setq user-emacs-directory (file-name-directory load-file-name)))

(defconst e-emacs-dir
  (eval-when-compile (file-truename user-emacs-directory)))
(defconst e-local-dir (concat e-emacs-dir ".local/"))
(defconst e-etc-dir (concat e-local-dir "etc/"))
(defconst e-cache-dir (concat e-local-dir "cache/"))
(defconst e-autoload-file (concat e-local-dir "autoloads.el"))
(defconst e-env-file (concat e-local-dir "env"))

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

;; Prevent `init.el' modification by `package.el'.
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
  :hook (focus-in . e-auto-revert-buffers)
  :hook (after-save . e-auto-revert-buffers)
  :config
  (setq auto-revert-verbose t
        auto-revert-use-notify nil
        auto-revert-stop-on-user-input nil
        revert-without-query (list "."))

  (defun e-auto-revert-buffer ()
    (unless (or auto-revert-mode (active-minibuffer-window))
      (let ((auto-revert-mode t))
        (auto-revert-handler))))

  (defun e-auto-revert-buffers ()
    (dolist (buf (e-visible-buffers))
      (with-current-buffer buf
        (e-auto-revert-buffer)))))

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
  :hook ((change-major-mode-after-body read-only-mode) . e-detect-indentation-h)
  :config
  (defun e-detect-indentation-h ()
    (unless (or (not after-init-time)
                (eq major-mode 'fundamental-mode)
                (member (substring (buffer-name) 0 1) '(" " "*")))
      (let ((inhibit-message nil))
        (dtrt-indent-mode +1))))

  ;; Always keep tab-width up-to-date.
  (push '(t tab-width) dtrt-indent-hook-generic-mapping-list))

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

;; FIXME A wierd st bug that leaves "artifacts" of line numbers once you
;; pop a new line before some text. Meanwhile I'll disable linum becuase it
;; creeps me out.

;; Enable line numbers in most text-editing modes.
;(add-hook 'prog-mode-hook #'display-line-numbers-mode)
;(add-hook 'text-mode-hook #'display-line-numbers-mode)
;(add-hook 'conf-mode-hook #'display-line-numbers-mode)

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
  (cond ((minibuffer-window-active-p (minibuffer-window))
         ;; Quit the minibuffer if open.
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
      (evil-emacs-state +1)))

  ;; Shorter messages.
  (unless noninteractive
    (setq save-silently t)
    (add-hook 'after-save-hook
      (lambda (&rest _)
        (message "\"%s\" %dL, %dC written"
                 (if buffer-file-name
                     (file-relative-name (file-truename buffer-file-name) (e-project-root))
                   (buffer-name))
                 (count-lines (point-min) (point-max))
                 (buffer-size))))))

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
      (company-abort))))
