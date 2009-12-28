(setq load-path (cons "~/.emacs.d" load-path))

;; Use M-x byte-compile-file to precompile .el files (ex: linum)

;; RST files support:
(require 'rst)
(setq auto-mode-alist
      (append '(("\\.txt$" . rst-mode)
                ("\\.rst$" . rst-mode)
                ("\\.rest$" . rst-mode)) auto-mode-alist))


;; Displaying of line number on the left:
;; (see also 'longlines')
(require 'linum)
(add-hook 'find-file-hook (lambda () (linum-mode 1))) 

;; Moves the cursor across "physical lines":
(require 'physical-line)
(add-hook 'find-file-hooks 'physical-line-mode-without-exception)

;; Automatic indentation while typing:
;(add-hook 'find-file-hooks '(lambda ()
;      (local-set-key (kbd "RET") 'newline-and-indent)))

(add-hook 'find-file-hooks '(lambda ()
      (local-set-key (kbd "RET") 'reindent-then-newline-and-indent)))

;(setq indent-line-function 'indent-relative-maybe)
	  
	  
;; Indenting buffers as a whole:
(defun iwb ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  )


(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)
(setq uniquify-after-kill-buffer-p 1)
(setq uniquify-ignore-buffers-re "^\\*")

(require 'whitespace)
(global-whitespace-mode 1)
(setq-default show-trailing-whitespace nil)
(setq whitespace-style '(space tabs lines-tail trailing empty indentation space-before-tab space-after-tab))
(setq whitespace-line-column 80)


(global-set-key [f6]  'goto-line)
(global-set-key [f2]  'save-buffer)
(global-set-key [f9]         'query-replace)
(global-set-key [(shift f9)] 'query-replace-regexp)
(global-set-key [f10] 'undo)
(global-set-key [f11] 'iwb)
(global-set-key [f12] 'kill-buffer)


(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)
(setq font-lock-maximum-size nil)
(transient-mark-mode t)
(setq compilation-window-height 10)
;;(standard-display-european 1)

(setq ispell-dictionary "francais")	;
(setq ispell-program-name "aspell")
(add-hook 'text-mode-hook 'flyspell-mode)

(show-paren-mode t)
(setq show-paren-style 'parenthesis)

(setq initial-scratch-message "")

(setq inhibit-startup-message t)
(setq-default transient-mark-mode t)

(defalias 'yes-or-no-p 'y-or-n-p)

(global-hl-line-mode 1)

(setq make-backup-files nil)

(setq initial-major-mode 'text-mode)
(setq default-major-mode 'text-mode)

;; ========== Place Backup Files in Specific Directory ==========

;; Save all backup file in this directory.
(setq backup-directory-alist (quote ((".*" . "~/.emacs_backups/"))))

;; ========== Enable Line and Column Numbering ==========

;; Show line-number in the mode line
;;(line-number-mode 1)

;; Show column-number in the mode line
(column-number-mode 1)

(setq-default fill-column 80)

;; Set cursor color
(set-cursor-color "white")

;; Set mouse color
(set-mouse-color "white")

;; Set foreground and background
(set-foreground-color "white")
;;(set-background-color "darkblue")
(set-background-color "black")

;;; Set highlighting colors for isearch and drag
;;(set-face-foreground 'highlight "white")


;; Color for the cursor line:
(set-face-background 'highlight "gray19")
;;(set-face-background 'highlight "black")

(set-face-foreground 'region "cyan")
(set-face-background 'region "blue")

;(set-face-foreground 'secondary-selection "skyblue")
;(set-face-background 'secondary-selection "darkblue")
(set-face-foreground 'secondary-selection "red")
(set-face-background 'secondary-selection "green")

;;Turn off a blinking cursor! 
(if (fboundp 'blink-cursor-mode)
    (blink-cursor-mode -1))


(setq line-move-visual nil)

(setq default-tab-width 4)

(setq scroll-step 1)
(show-paren-mode 1)
(delete-selection-mode 1)
(transient-mark-mode 1)
(savehist-mode 1)

(setq tool-bar-mode nil)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(transient-mark-mode t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
