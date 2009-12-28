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
 
;; Indenting buffers as a whole:
(defun iwb ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  )

(global-set-key [f12] 'iwb)

(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)
(setq font-lock-maximum-size nil)
(transient-mark-mode t)
(setq compilation-window-height 10)
;;(standard-display-european 1)

(setq ispell-dictionary "francais")	;

(show-paren-mode t)
(setq default-major-mode 'text-mode)
(setq inhibit-startup-message t)
(setq-default transient-mark-mode t)

(global-hl-line-mode 1)

;; ========== Place Backup Files in Specific Directory ==========

;; Enable backup files.
(setq make-backup-files t)

;; Save all backup file in this directory.
(setq backup-directory-alist (quote ((".*" . "~/.emacs_backups/"))))

;; ========== Enable Line and Column Numbering ==========

;; Show line-number in the mode line
;;(line-number-mode 1)

;; Show column-number in the mode line
(column-number-mode 1)

;; Set cursor color
(set-cursor-color "white")

;; Set mouse color
(set-mouse-color "white")

;; Set foreground and background
(set-foreground-color "white")
;;(set-background-color "darkblue")
(set-background-color "black")

;;; Set highlighting colors for isearch and drag
(set-face-foreground 'highlight "white")


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

