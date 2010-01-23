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

(require 'erlang-start)


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

;; We want to see whether we go past column 80:
(require 'highlight-80+)
(add-hook 'find-file-hook 'highlight-80+-mode)

;; 85 width allows to display correctly even files with 9999 lines,
;; knowing that the leftmost column for line numbers uses some place:
(add-to-list 'default-frame-alist (cons 'width 85))


;; Key section:


(defun default-fone ()
  (message "Default for F1")
)

(defun default-f2 ()
  (message "Default for F2")
)

(defun default-f3 ()
  (message "Default for F3")
)

(defun default-f4 ()
  (message "Default for F4")
)

(defun default-f5 ()
  (message "Default for F5")
)

(defun default-f6 ()
  (message "Default for F6")
)

(defun default-f7 ()
  (message "Default for F7")
)

(defun default-f8 ()
  (message "Default for F8")
)

(defun default-f9 ()
  (message "Default for F9")
)

(defun default-shift-f9 ()
  (message "Default for Shift-F9")
)

(defun default-f10 ()
  (message "Default for F10")
)

(defun default-f11 ()
  (message "Default for F11")
)

(defun default-f12 ()
  (message "Default for F12")
)


;; Actual mapping:

(defun show-assigned-keys ()
 "Shows the current key bindings"
 (interactive)
 (message "F1        -> save-buffer" )
 (message "F2        -> query-replace" )
 (message "F3        -> query-replace-regexp" )
 (message "F4        -> indent-whole-buffer" )
 (message "F5        -> undo" )
 (message "F6        -> repeat-complex-command" )
 (message "F7        -> goto-line" )
 (message "F8        -> whitespace-cleanup" )
 (message "F9        -> (intercepted by Ubuntu)" )
 (message "Shift-F9  -> (currently not bound)" )
 (message "F10       -> (currently not bound)" )
 (message "F11       -> (does nothing)" )
 (message "F12       -> (does nothing)" )
  
)


;; Curiously hitting F1 triggers default-f12:
(global-set-key [f1]			  'default-f1)
(global-set-key [XF86New]		  'default-f1)

;; Usable and behaves like expected:
(global-set-key [f2]              'query-replace)
(global-set-key [XF86Reply]       'query-replace)

;; Usable and behaves like expected:
(global-set-key [f3]			  'query-replace-regexp)
(global-set-key [XF86MailForward] 'query-replace-regexp)

;; Usable and behaves like expected:
(global-set-key [f4]			  'indent-whole-buffer)
(global-set-key [XF86Send]  	  'indent-whole-buffer)

;; Curiously bound to Undo:
(global-set-key [f5]              'default-f5)
(global-set-key [XF86New]         'default-f5)

;; Curiously bound to repeat-complex-command:
(global-set-key [f6]			  'default-f6)
(global-set-key [XF86New]		  'default-f6)


;; Usable and behaves like expected:
(global-set-key [f7]			  'goto-line)
(global-set-key [print] 		  'goto-line)


;; Usable and behaves like expected:
(global-set-key [f8]              'whitespace-cleanup)
(global-set-key [XF86Save]        'whitespace-cleanup)


;; Intercepted by Ubuntu:
(global-set-key [f9]			  'default-f9)
(global-set-key [XF86New]		  'default-f9)


;; Usable and behaves like expected:
(global-set-key [(shift f9)]	    'default-shift-f9)
(global-set-key [(shift XF86New)]   'default-shift-f9)
(global-set-key [XF86Explorer]      'default-shift-f9)


;; Usable and behaves like expected:
(global-set-key [f10]			    'default-f10)
(global-set-key [XF86Documents]     'default-f10)


;; Not triggered on my keyboard:
(global-set-key [f11]			    'default-f11)
(global-set-key [XF86New]		    'default-f11)


;; Not triggered when hitting F12, but triggered when hitting F1 on my keyboard:
(global-set-key [f12]               'save-buffer)
(global-set-key [XF86New]           'save-buffer)


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

(add-to-list 'default-frame-alist (cons 'width 85))

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

