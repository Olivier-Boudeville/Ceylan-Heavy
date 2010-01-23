(setq load-path (cons "~/.emacs.d" load-path))

;; Use M-x byte-compile-file to precompile .el files (ex: linum)



;; RST files support section.

(require 'rst)
(setq auto-mode-alist
      (append '(("\\.txt$"  . rst-mode)
				("\\.rst$"  . rst-mode)
				("\\.rest$" . rst-mode)) auto-mode-alist))

;; Automatically update the table of contents everytime you adjust a
;; section title:
(add-hook 'rst-adjust-hook 'rst-toc-update)


;; Corresponds to conventions in demo-for-css-testing.rst:
;; (not correctly applied apparently, though)
(setq rst-preferred-decorations '( (?= over-and-under 0)
                                   (?- over-and-under 0)
                                   (?= simple 0)
                                   (?- simple 0)
                                   (?. simple 0)
                                   (?_ simple 0)
                                   (?* simple 0)
                                   (?: simple 0)
                                   (?+ simple 0) ))



;; Displaying of line number on the left:
;; (see also 'longlines')
(require 'linum)
(add-hook 'find-file-hook (lambda () (linum-mode 1)))


;; Moves the cursor across "physical lines":
;; (finally deactivated, as the 'go to end of line' key was leading to the
;; cursor going downward...)
;;(require 'physical-line)
;;(add-hook 'find-file-hooks 'physical-line-mode-without-exception)



;; Automatic indentation while typing:

;; Does not work correctly with inner bullet lists:
;;(setq indent-line-function 'indent-relative-maybe)

;; Just indents by default at the same level when Enter is hit:
;;(add-hook 'find-file-hooks '(lambda ()
;;      (local-set-key (kbd "RET") 'newline-and-indent)))


;; Useful for most programming modes, but disrupts sub-bullet lists in
;; text (ex: RST) modes (puts them all at the same level):
;; (not defined as a lambda in order to be able to remove it)
(defun set-advanced-ret-behaviour ()
  ;;(message "############ Setting advanced RET behaviour ###########")
  ;;(local-set-key (kbd "RET") 'reindent-then-newline-and-indent)
  (global-set-key (kbd "RET") 'reindent-then-newline-and-indent)
)

(add-hook 'find-file-hooks 'set-advanced-ret-behaviour)



(defun fix-ret-behaviour-for-rst-mode ()
  ;;(message "############## Fixing RET behaviour for RST mode ###########")
  
  (remove-hook 'find-file-hooks 'set-advanced-ret-behaviour)
  (global-set-key (kbd "RET") 'newline-and-indent)
)

(add-hook 'rst-mode-hook 'fix-ret-behaviour-for-rst-mode)



;; Indenting buffers as a whole:
(defun indent-whole-buffer ()
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


;; Key section:

;;(global-set-key [f1]              'kill-buffer)
;;(global-set-key [XF86New]         'kill-buffer)

(global-set-key [f2]                'save-buffer)
(global-set-key [XF86Reply]         'save-buffer)

;;(global-set-key [f3]              'kill-buffer)
;;(global-set-key [XF86MailForward] 'kill-buffer)

;;(global-set-key [f4]              'kill-buffer)
;;(global-set-key [XF86Send]        'kill-buffer)

(global-set-key [f5]                'undo)
(global-set-key [XF86New]           'undo)

;;(global-set-key [f6]              'goto-line)
;;(global-set-key [XF86New]         'goto-line)

;;(global-set-key [f7]              'kill-buffer)
;;(global-set-key [print]           'kill-buffer)

;;(global-set-key [f8]              'kill-buffer)
;;(global-set-key [XF86Save]        'kill-buffer)

(global-set-key [f9]			    'query-replace)
(global-set-key [XF86New]		    'query-replace)

(global-set-key [(shift f9)]	    'query-replace-regexp)
(global-set-key [(shift XF86New)]   'query-replace-regexp)

(global-set-key [f10]			    'whitespace-cleanup)
(global-set-key [XF86Documents]     'whitespace-cleanup)

(global-set-key [f11]			    'indent-whole-buffer)
(global-set-key [XF86New]		    'indent-whole-buffer)

;;(global-set-key [f12]             'kill-buffer)
;;(global-set-key [XF86New]         'kill-buffer)


(global-set-key "\C-Z" 'undo)

;;(global-set-key "TAB" 'reindent-then-newline-and-indent)



(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)
(setq font-lock-maximum-size nil)
(transient-mark-mode t)
(setq compilation-window-height 10)
;;(standard-display-european 1)


(setq ispell-dictionary "english")
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


;; Save all backup file in this directory:
(setq backup-directory-alist (quote ((".*" . "~/.emacs_backups/"))))

;
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

;;(set-face-foreground 'secondary-selection "skyblue")
;;(set-face-background 'secondary-selection "darkblue")
(set-face-foreground 'secondary-selection "red")
(set-face-background 'secondary-selection "green")

;; Turns off a blinking cursor:
(if (fboundp 'blink-cursor-mode)
    (blink-cursor-mode -1))

;;(setq frame-background-mode 'dark)
'(frame-background-mode (quote dark))


(setq line-move-visual nil)

(setq default-tab-width 4)

(setq scroll-step 1)
(show-paren-mode 1)
(delete-selection-mode 1)
(transient-mark-mode 1)
(savehist-mode 1)

(setq frame-title-format '("%b" (buffer-file-name ": %f")))

(setq tool-bar-mode nil)

(server-start)

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
 ;; Determines the rendering of titles in RST mode:
  '(rst-level-1-face ((t (:background "#00f" :foreground "#fff"))))
  '(rst-level-2-face ((t (:background "#00a" :foreground "#ddd"))))
  '(rst-level-3-face ((t (:background "#003" :foreground "#bbb"))))
  '(rst-level-4-face ((t (:background "#000" :foreground "#999"))))
  '(rst-level-5-face ((t (:background "#010" :foreground "#666"))))
  '(rst-level-6-face ((t (:background "#020" :foreground "#555"))))
 )

