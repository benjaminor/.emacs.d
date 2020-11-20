;; Package -- base-customization
;;; Commentary:
;;; this is for my functions, my keybindings and my aliases

;;;;; Key bindings ;;;;;;

;;; Code:

;;(global-set-key "\C-x/" 'point-to-register)
(global-set-key "\C-xj" 'jump-to-register)
;;(global-set-key "\C-xc" 'compile)


;;;;Open certain directories easy
(defun my/open-directory (DIRECTORY)
  "Open interactive find-files in DIRECTORY."
  (interactive)
  (let ((default-directory DIRECTORY))
    (call-interactively 'helm-find-files)))

(global-set-key "\C-xä" '(lambda () (interactive)(my/open-directory my-tex-files-directory)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom splitting functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun vsplit-last-buffer ()
  "Split the buffer vertically and open last buffer."
  (interactive)
  (split-window-vertically)
  (other-window 1 nil)
  (switch-to-next-buffer)
  )
(defun hsplit-last-buffer ()
  "Split the buffer horizontally and open last buffer."
  (interactive)
  (split-window-horizontally)
  (other-window 1 nil)
  (switch-to-next-buffer)
  )

(global-set-key (kbd "C-x 2") 'vsplit-last-buffer)
(global-set-key (kbd "C-x 3") 'hsplit-last-buffer)

;; Reload buffer with F5
(global-set-key (kbd "<f5>") 'revert-buffer)

;; from: https://with-emacs.com/posts/tips/quit-current-context/
(defun keyboard-quit-context+ ()
  "Quit current context.
    This function is a combination of `keyboard-quit' and
    `keyboard-escape-quit' with some parts omitted and some custom
    behavior added."
  (interactive)
  (cond ((region-active-p)
         ;; Avoid adding the region to the window selection.
         (setq saved-region-selection nil)
         (let (select-active-regions)
           (deactivate-mark)))
        ((eq last-command 'mode-exited) nil)
        (current-prefix-arg
         nil)
        (defining-kbd-macro
          (message
           (substitute-command-keys
            "Quit is ignored during macro defintion, use \\[kmacro-end-macro] if you want to stop macro definition"))
          (cancel-kbd-macro-events))
        ((active-minibuffer-window)
         (when (get-buffer-window "*Completions*")
           ;; hide completions first so point stays in active window when
           ;; outside the minibuffer
           (minibuffer-hide-completions))
         (abort-recursive-edit))
        (t
         (keyboard-quit))))
(global-set-key [remap keyboard-quit] #'keyboard-quit-context+)


;; Aliases
;;
(defalias 'sh 'shell)
(defalias 'indr 'indent-region)


(provide 'base-customization)
;;; base-customization.el ends here
