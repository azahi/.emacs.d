;;; init.el -*- lexical-binding: t; -*-

;; Prevent GC from running before `ghcm-mode'.
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

;; Noop `file-name-handler-alist' to increase I/O performace.
(unless noninteractive
  (defvar e-initial-file-name-handler-alist file-name-handler-alist)

  (setq file-name-handler-alist nil)
  (defun e-reset-file-handler-alist-h ()
    (dolist (handler file-name-handler-alist)
      (add-to-list 'e-initial-file-name-handler-alist handler))
    (setq file-name-handler-alist e-initial-file-name-handler-alist))
  (add-hook 'emacs-startup-hook #'e-reset-file-handler-alist-h))

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

;; I don't understand why GNU always makes these kinds of poor descisions
;; regarding pretty much everything including *security*. I will never in my
;; life recommend anyone using anything by GNU. GNU, despite the fact that it
;; changed the world of computing, is a mistake. I only use Emacs because I like
;; `org-mode', in-place lisp evaluation, macros and a couple of unified features
;; I can use in all major-modes, other than that Emacs is a pain to make usable
;; and maintain. If you consider picking up a _power-user_ editor, just go for
;; NeoVim, don't play around with this shit, it doesn't worth your time. I
;; supposed to write a meaningful comment here regarding SSL in Emacs but it
;; turned to be a rant. I just can't take it anymore. I want someone to make a
;; proper org-mode plugin for Vim with shit like CalDAV and pandoc importers and
;; etc.
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

;; Littering.
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
(defun e-use-cache-dir (session-id)
  (concat e-cache-dir "emacs-session." session-id))
(advice-add 'emacs-session-filename :override #'e-use-cache-dir)

;; When enabling disabled commands write them to config.el instead.
(defun e-save-enabled-commands-to-custom (orig-fn &rest args)
  (let ((user-init-file custom-file))
    (apply orig-fn args)))
(advice-add #'en/disable-command :around #'e-save-enabled-commands-to-custom)

;;
;;; Optimization.
;;

;; Disable bidirectional text rendering.
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right
              bidi-inhibit-bpa t)

;; Reduce rendering/line scan work for Emacs by not rendering cursors or regions
;; in non-focused windows.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; Rrapid scrolling over unfontified regions.
(setq fast-but-imprecise-scrolling t)

;; Resizing the Emacs frame.
(setq frame-inhibit-implied-resize t)

;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)

;; Font compacting can be terribly expensive.
(setq inhibit-compacting-font-caches t)

;; Defer `tty-run-terminal-initialization'.
(unless (daemonp)
  (advice-add #'tty-run-terminal-initialization :override #'ignore)
  (add-hook 'window-setup-hook
    (lambda ()
      (advice-remove #'tty-run-terminal-initialization #'ignore)
      (tty-run-terminal-initialization (selected-frame) nil t))))

(defvar e-inhibit-local-var-hooks nil)
(defun e-run-local-var-hooks-h ()
  (unless e-inhibit-local-var-hooks
    (set (make-local-variable 'e-inhibit-local-var-hooks) t)
    (run-hook-wrapped (intern-soft (format "%s-local-vars-hook" major-mode))
                      #'e-try-run-hook)))

(defun e-run-local-var-hooks-maybe-h ()
  (unless enable-local-variables
    (e-run-local-var-hooks-h)))

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

;; Prevent init.el modification.
(advice-add #'package--ensure-init-file :override #'ignore)

;;
;;; Straight.
;;

(setq straight-base-dir e-local-dir
      straight-repository-branch "develop")

(with-eval-after-load 'straight
  (add-to-list 'straight-built-in-pseudo-packages 'let-alist))

;; This is a hacky hack I stole from doom-emacs.
;; TODO Replace with pure elisp.
(defun e-call-process (command &rest args)
  (with-temp-buffer
    (cons (or (apply #'call-process command nil t nil (remq nil args))
              -1)
          (string-trim (buffer-string)))))

;; Ensure straight.el
(defun e-ensure-straight ()
  (let ((repo-dir (expand-file-name "straight/repos/straight.el" straight-base-dir ))
        (repo-url (concat "http" (if gnutls-verify-error "s")
                          "://github.com/raxod502/straight.el"))
        (branch straight-repository-branch)
        (call #'e-call-process))
    (unless (file-directory-p repo-dir)
      (funcall call "git" "clone" "--origin" "origin" repo-url repo-dir
               "--branch" straight-repository-branch
               "--single-branch" "--no-tags"))
    (require 'straight (concat repo-dir "/straight.el"))
    (with-temp-buffer
      (insert-file-contents (expand-file-name "bootstrap.el" repo-dir))
      (eval-region (search-forward "(require 'straight)")
                   (point-max)))))
(e-ensure-straight)

;;
;;; Use-package.
;;

(with-eval-after-load 'straight
  (straight-use-package 'use-package))

(autoload 'use-package "use-package-core" nil nil t)

  (setq use-package-compute-statistics nil
        use-package-verbose nil
        use-package-minimum-reported-time 0.1
        use-package-expand-minimally t)

(with-eval-after-load 'use-package-core
  ;; Activate straight.el integration.
  (setq straight-use-package-by-default t))

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
  (lambda ()
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

(add-hook 'after-save-hook
  (lambda ()
    (when (eq major-mode 'fundamental-mode)
      (let ((buffer (or (buffer-base-buffer) (current-buffer))))
        (and (buffer-file-name buffer)
             (eq buffer (window-buffer (selected-window)))
             (set-auto-mode))))))

(eval-after-load 'tramp
  (add-to-list 'backup-directory-alist (cons tramp-file-name-regexp nil)))

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
  :hook (focus-in . e-auto-revert-buffers-h)
  :hook (after-save . e-auto-revert-buffers-h)
  ;:hook (e-switch-buffer . e-auto-revert-buffer-h)
  ;:hook (e-switch-window . e-auto-revert-buffer-h)
  :config
  (setq auto-revert-verbose t
        auto-revert-use-notify nil
        auto-revert-stop-on-user-input nil
        revert-without-query (list "."))

  (defun e-auto-revert-buffer-h ()
    "Auto revert current buffer, if necessary."
    (unless (or auto-revert-mode (active-minibuffer-window))
      (let ((auto-revert-mode t))
        (auto-revert-handler))))

  (defun e-auto-revert-buffers-h ()
    "Auto revert stale buffers in visible windows, if necessary."
    (dolist (buf (e-visible-buffers))
      (with-current-buffer buf
        (e-auto-revert-buffer-h)))))

(use-package recentf
  :hook (e-first-file . recentf-mode)
  :commands recentf-open-files
  :config
  (defun e-recent-file-truename (file)
    (if (or (file-remote-p file nil t)
            (not (file-remote-p file)))
        (file-truename file)
      file))
  (setq recentf-filename-handlers
        '(substring-no-properties
          e-recent-file-truename
          abbreviate-file-name)
        recentf-save-file (concat e-cache-dir "recentf")
        recentf-auto-cleanup 'never
        recentf-max-menu-items 0
        recentf-max-saved-items 200)

  (add-hook 'e-switch-window-hook
    (lambda ()
      (when buffer-file-name
        (recentf-add-file buffer-file-name))
      nil))

  (add-hook 'dired-mode-hook
    (lambda ()
      (recentf-add-file default-directory)))

  (add-hook 'kill-emacs-hook #'recentf-cleanup))

(use-package savehist
  :hook (e-first-input . savehist-mode)
  :init
  (setq savehist-file (concat e-cache-dir "savehist"))
  :config
  (setq savehist-save-minibuffer-history t
        savehist-autosave-interval nil
        savehist-additional-variables
        '(kill-ring
          mark-ring
          global-mark-ring
          search-ring
          regexp-search-ring))
  (add-hook 'savehist-save-hook
    (lambda ()
      (setq kill-ring (cl-loop for item in kill-ring
                               if (stringp item)
                               collect (substring-no-properties item)
                               else if item collect it)))))

(use-package saveplace
  :hook (e-first-file . save-place-mode)
  :init
  (setq save-place-file (concat e-cache-dir "saveplace")
        save-place-limit 100)
  :config
  (defun e-recenter-on-load-saveplace-a (&rest _)
    (if buffer-file-name (ignore-errors (recenter))))
  (advice-add #'save-place-find-file-hook :after-while #'e-recenter-on-load-saveplace-a)

  (defun e-inhibit-saveplace-in-long-files-a (orig-fn &rest args)
    (unless e-large-file-p
      (apply orig-fn args)))
  (advice-add #'save-place-to-alist :around #'e-inhibit-saveplace-in-long-files-a))

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
  :hook (e-first-input . better-jumper-mode)
  :hook (better-jumper-post-jump . recenter)
  :commands e-set-jump-a e-set-jump-maybe-a e-set-jump-h
  :init
  (global-set-key [remap evil-jump-forward]  #'better-jumper-jump-forward)
  (global-set-key [remap evil-jump-backward] #'better-jumper-jump-backward)
  (global-set-key [remap xref-pop-marker-stack] #'better-jumper-jump-backward)
  :config
  (defun e-set-jump-a (orig-fn &rest args)
    (better-jumper-set-jump (if (markerp (car args)) (car args)))
    (let ((evil--jumps-jumping t)
          (better-jumper--jumping t))
      (apply orig-fn args)))

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
      result))

  (defun e-set-jump-h ()
    (better-jumper-set-jump)
    nil)

  (advice-add #'kill-current-buffer :around #'e-set-jump-a)

  (advice-add #'imenu :around #'e-set-jump-a))

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
  :hook (e-first-buffer . smartparens-global-mode)
  :commands sp-pair sp-local-pair sp-with-modes sp-point-in-comment sp-point-in-string
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

  ;; https://github.com/Fuco1/smartparens/issues/783.
  (setq sp-escape-quotes-after-insert nil)

  ;; Quiet.
  (dolist (key '(:unmatched-expression :no-matching-tag))
    (setf (alist-get key sp-message-alist) nil))

  (add-hook 'minibuffer-setup-hook
    (lambda ()
      (and (memq this-command '(eval-expression pp-eval-expression evil-ex))
           smartparens-global-mode
           (smartparens-mode))))

  ;; Disable in minibuffer.
  (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
  (sp-local-pair 'minibuffer-inactive-mode "`" nil :actions nil)

  ;; Breakage in evil-mode's replace state.
  (defvar e-buffer-smartparens-mode nil)
  (add-hook 'evil-replace-state-exit-hook
    (lambda ()
      (when e-buffer-smartparens-mode
        (turn-on-smartparens-mode)
        (kill-local-variable 'e-buffer-smartparens-mode))))
  (add-hook 'evil-replace-state-entry-hook
    (lambda ()
      (when smartparens-mode
        (setq-local e-buffer-smartparens-mode t)
        (turn-off-smartparens-mode)))))

(use-package ws-butler
  :hook (e-first-buffer . ws-butler-global-mode))

;;
;;; UX.
;;

;;; Hooks.

(defvar e-init-ui-hook nil)
(defvar e-load-theme-hook nil)
(defvar e-switch-buffer-hook nil)
(defvar e-switch-window-hook nil)
(defvar e-switch-frame-hook nil)

(defvar e-inhibit-switch-buffer-hooks nil)
(defvar e-inhibit-switch-window-hooks nil)
(defvar e-inhibit-switch-frame-hooks nil)

;;;

;; Just quit.
(setq confirm-kill-emacs 'yes-or-no-p)

;; Don't prompt for confirmation on creating a new file.
(setq confirm-nonexistent-file-or-buffer nil)

(setq uniquify-buffer-name-style 'forward
      ring-bell-function #'ignore
      visible-bell nil)

;;; Scrolling.

(setq hscroll-margin 2
      hscroll-step 1
      scroll-conservatively 101
      scroll-margin 0
      scroll-preserve-screen-position t
      auto-window-vscroll nil)

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
(add-hook 'e-init-ui-hook #'window-divider-mode)

;; Avoid GUI.
(setq use-dialog-box nil)
;; Don't display floating tooltips; display their contents in the echo-area,
;; because native tooltips are ugly.
(when (bound-and-true-p tooltip-mode)
  (tooltip-mode -1))
(setq x-gtk-use-system-tooltips nil)

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

(use-package compile
  :config
  (setq compilation-always-kill t
        compilation-ask-about-save nil
        compilation-scroll-output 'first-error)
  ;; Handle ansi codes in compilation buffer.
  (add-hook 'compilation-filter-hook
    (lambda ()
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
    (lambda ()
      (setq e-ediff-saved-wconf (current-window-configuration))))
  (add-hook 'ediff-quit-hook :append
    (lambda ()
      (when (window-configuration-p e-ediff-saved-wconf)
        (set-window-configuration e-ediff-saved-wconf)))))

(use-package hl-line
  :disabled
  :hook ((prog-mode text-mode conf-mode special-mode) . hl-line-mode)
  :config
  ;; Not having to render the hl-line overlay in multiple buffers offers a tiny
  ;; performance boost.
  (setq hl-line-sticky-flag nil
        global-hl-line-sticky-flag nil)

  ;; Temporarily disable `hl-line' when selection is active.
  (defvar e-hl-line-mode nil)
  (add-hook 'evil-visual-state-entry-hook
    (lambda ()
      (when hl-line-mode
        (setq-local e-hl-line-mode t)
        (hl-line-mode -1))))
  (add-hook 'evil-visual-state-exit-hook
    (lambda ()
      (when e-hl-line-mode
        (hl-line-mode +1)))))

(use-package winner
  :preface (defvar winner-dont-bind-my-keys t)
  :hook (doom-first-buffer . winner-mode))

(use-package paren
  :hook (e-first-buffer . show-paren-mode)
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

  (defun doom-disable-whitespace-mode-in-childframes-a (orig-fn)
    (unless (frame-parameter nil 'parent-frame)
      (funcall orig-fn)))
  (add-function :around whitespace-enable-predicate #'e-disable-whitespace-mode-in-childframes-a))

;;; External packages.

(use-package hide-mode-line
  :hook (Man-mode-hook . hide-mode-line-mode)
  :hook (completion-list-mode-hook . hide-mode-line-mode))

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
  (defalias 'undefine-key! #'general-unbind))

(use-package which-key
  :hook (e-first-input . which-key-mode)
  :init
  (setq which-key-sort-order #'which-key-prefix-then-key-order
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 6
        which-key-side-window-slot -10)
  :config
  (set-face-attribute 'which-key-local-map-description-face nil :weight 'bold)
  (which-key-setup-side-window-bottom)

  (which-key-add-key-based-replacements e-leader-key "<leader>")
  (which-key-add-key-based-replacements e-localleader-key "<localleader>"))

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
    ,@args))

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
    (lambda ()
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

  ;; Only use `fd' or `ripgrep'.
  (advice-add #'projectile-get-ext-command :override
    (lambda (vcs)
      (if (functionp projectile-generic-command)
          (funcall projectile-generic-command vcs)
        projectile-generic-command)))
  (setq projectile-git-submodule-command nil
        projectile-indexing-method 'hybrid
        projectile-generic-command
        (lambda ()
          (let ((find-exe-fn #'executable-find))
            (cond
             ((when-let
                  (bin (if (ignore-errors (file-remote-p default-directory nil t))
                           (cl-find-if find-exe-fn (list "fdfind" "fd"))
                         e-projectile-fd-binary))
                (concat (format "%s . -0 -H -E .git --color=never --type file --type symlink --follow"
                                bin)
                        (if IS-WINDOWS " --path-separator=/"))))
             ((funcall find-exe-fn "rg")
              (concat "rg -0 --files --follow --color=never --hidden"
                      (cl-loop for dir in projectile-globally-ignored-directories
                               concat " --glob "
                               concat (shell-quote-argument (concat "!" dir)))
                      (if IS-WINDOWS " --path-separator /")))
             ("find . -type f -print0")))))

  (advice-add #'projectile-default-generic-command :around
    (lambda (orig-fn &rest args))
      (ignore-errors (apply orig-fn args))))

;;
;;; Finalize.
;;

(add-hook 'hack-local-variables-hook #'e-run-local-var-hooks-h)
(add-hook 'after-change-major-mode-hook #'e-run-local-var-hooks-maybe-h)

(defvar e-first-input-hook nil)
(add-hook 'e-first-input-hook #'gcmh-mode)
(add-hook 'e-first-input-hook #'pre-command-hook)

(defvar e-first-file-hook nil)
(add-hook 'e-first-file-hook #'after-find-file)
(add-hook 'e-first-file-hook #'dired-initial-position-hook)

(defvar e-first-buffer-hook nil)
(add-hook 'e-first-buffer-hook #'after-find-file)
(add-hook 'e-first-buffer-hook #'e-switch-buffer-hook)
