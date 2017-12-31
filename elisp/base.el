;;; package --- summary
;;; basic extensions

;;; Commentary:

;;; Maybe add ledger

;;; Code:



;(require 'iso-transl)
;; Tell emacs where is your personal elisp lib dir

;;(load-library "url-handlers")

(require 'package)
(package-initialize)

;;;;;;;;;;;;;;;; ELPA Sources ;;;;;;;;;;;;;;;;



(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
             ("marmalade" . "http://marmalade-repo.org/packages/")
             ("melpa" . "http://melpa.org/packages/")
             ("org" . "http://orgmode.org/elpa/")))



;;;set PATH variable
;; (setenv "PATH" (concat (getenv "PATH") ":/usr/texbin"))
;; (setq exec-path (append exec-path '("/usr/texbin")))

(when (not package-archive-contents)
  (package-refresh-contents))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; Bootstrap `use-package';;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'diminish)                ;; if you use :diminish
(require 'bind-key)                ;; if you use any :bind variant


;;; Install every package that is used but not already installed
;; (setq use-package-always-ensure t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Auto-update packages every 7 days
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package auto-package-update
  :config
  (auto-package-update-maybe))




(defconst private-dir  (expand-file-name "private" user-emacs-directory))
(defconst temp-dir (format "%s/cache" private-dir)
  "Hostname-based elisp temp directories.")

;; Core settings
;; UTF-8 please
(set-charset-priority 'unicode)
(setq locale-coding-system   'utf-8)   ; pretty
(set-terminal-coding-system  'utf-8)   ; pretty
(set-keyboard-coding-system  'utf-8)   ; pretty
(set-selection-coding-system 'utf-8)   ; please
(prefer-coding-system        'utf-8)   ; with sugar on top
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

;; Emacs customizations
(setq confirm-kill-emacs                  nil
      confirm-nonexistent-file-or-buffer  t
      save-interprogram-paste-before-kill t
      mouse-yank-at-point                 t
      require-final-newline               t
      visible-bell                        nil
      ring-bell-function                  'ignore
      custom-file                         "~/.emacs.d/.custom.el"
      ;; http://ergoemacs.org/emacs/emacs_stop_cursor_enter_prompt.html
      minibuffer-prompt-properties
      '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)

      ;; Disable non selected window highlight
      cursor-in-non-selected-windows     nil
      highlight-nonselected-windows      nil
      ;; PATH
      exec-path                          (append exec-path '("/usr/local/bin/"))
      indent-tabs-mode                   nil
      tab-width                          4
      inhibit-startup-message            t
      fringes-outside-margins            t
      select-enable-clipboard            t
      delete-selection-mode              1
      use-package-always-ensure          t)

;; Bookmarks
(setq
 ;; persistent bookmarks
 bookmark-save-flag                      t
 bookmark-default-file              (concat temp-dir "/bookmarks"))


;; Backups enabled, use nil to disable
(setq
 history-length                     1000
 backup-inhibited                   nil
 make-backup-files                  t
 auto-save-default                  t
 auto-save-interval                 1000
 auto-save-list-file-name           (concat temp-dir "/autosave")
 make-backup-files                  t
 create-lockfiles                   nil
 backup-directory-alist            `((".*" . ,(concat temp-dir "/backup/")))
 auto-save-file-name-transforms    `((".*" ,(concat temp-dir "/auto-save-list/") t)))

(fset 'yes-or-no-p 'y-or-n-p)
(global-auto-revert-mode t)

;; Disable toolbar & menubar
(menu-bar-mode 1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode 1))
(when (  fboundp 'scroll-bar-mode)
  (scroll-bar-mode 1))

(show-paren-mode 1)

;; Delete trailing whitespace before save
;; The whitespace-cleanup-mode does this now
;;(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Save session
(desktop-save-mode 1)

;; Garbage collection
(add-hook 'focus-out-hook #'garbage-collect)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; mark line where cursor is
(global-hl-line-mode t)


(provide 'base)
;;; base ends here
