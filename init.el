;; Guide to install all packages
;; Step 1 - M-x -> package-install use-package
;; Step 2 - M-x -> package-install swiper
;; Step 3 - Reboot emacs
;; Step 4 - M-x -> package-refresh-contents
;; Step 5 - M-x -> package-install-selected-packages

;; Enable Melpa
(package-initialize)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

;; Turn window into a dedicated window
(defun turn-window-dedicated ()
  (interactive)
   (let  (window (get-buffer-window (current-buffer)))
         (set-window-dedicated-p window t)))
(global-set-key (kbd "C-c t") 'turn-window-dedicated)

;; Buffer sidebar
(defun ibuffer-light-sidebar ()
  (interactive)
  (let (( buffer  
          (save-window-excursion
            (ibuffer nil "*side-ibuffer*")
                   (ibuffer-auto-mode 1)
                   (setq-local ibuffer-formats (append ibuffer-formats '((mark "" name))))
                   (setq-local ibuffer-current-format (1- (length ibuffer-formats)))
                   (ibuffer-redisplay t)
                   (setq-local ibuffer-display-summary nil)
            (current-buffer))))
    (pop-to-buffer buffer
                   '(display-buffer-in-side-window
                    (side . left) (slot . -1))))
                    (turn-window-dedicated))
(global-set-key (kbd "<C-f2>") 'ibuffer-light-sidebar)

;; Shift selection support in org mode
(setq org-replace-disputed-keys t)

;; Dired sidebar
(defun dired-light-sidebar ()
  (interactive)
  (let (( buffer
          (save-window-excursion
            (dired-at-point ".")
            (current-buffer))))
    (pop-to-buffer buffer
                   '(display-buffer-in-side-window
                     (side . left) (slot . -2))))
                    (turn-window-dedicated))
(global-set-key (kbd "<C-f1>") 'dired-light-sidebar)

;; Open file manager dired
(defun dired-at-point-dot ()
  (interactive)
  
  ;; Put your commands below
  (dired-at-point ".")
)
(global-set-key (kbd "C-o") 'dired-at-point-dot)

;; Use package Flycheck
(use-package flycheck
  :ensure t
  :config
  (add-hook 'c-mode-hook 'flycheck-mode)

  ;; Display error popup only if error list isn't open
  (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)
  (global-flycheck-mode t)) 

;; Dired directory first
;; (setq dired-listing-switches "-laGh1v --group-directories-first")

;; Hide dired details by default
(add-hook 'dired-mode-hook
      (lambda ()
        (dired-hide-details-mode)
        (dired-sort-toggle-or-edit)))

;; Dired subtree
(eval-after-load "dired" '(progn
  (define-key dired-mode-map (kbd "TAB") 'dired-subtree-cycle)
  (define-key dired-mode-map (kbd "c") 'dired-ranger-copy)
  (define-key dired-mode-map (kbd "x") 'dired-ranger-move)
  (define-key dired-mode-map (kbd "v") 'dired-ranger-paste)
  (define-key dired-mode-map (kbd "M-f") 'dired-do-find-regexp)
  (define-key dired-mode-map (kbd "M-h") 'dired-do-find-regexp-and-replace)
  ))

;; Disable Dired subtree colors.
(setq dired-subtree-use-backgrounds nil)

;; Toggle Dired editing mode
(eval-after-load "dired" '(progn
  (define-key dired-mode-map (kbd "C-h") 'dired-toggle-read-only) ))

;; Show different symbols characters for tabs and spaces
(global-whitespace-mode)
(setq whitespace-style '(trailing tabs newline tab-mark))

;; Auto complete with company mode
(use-package company
  :ensure t
  :config
  (setq company-idle-delay nil)
  (setq company-dabbrev-downcase nil)
  (setq company-minimum-prefix-length 1)
  (global-company-mode t)
  )

(global-set-key (kbd "<C-SPC>") 'company-complete-common)

;;(use-package flycheck-clang-analyzer
;;  :ensure t
;;  :config
;;  (with-eval-after-load 'flycheck
;;    (require 'flycheck-clang-analyzer)
;;     (flycheck-clang-analyzer-setup)))

(with-eval-after-load 'company
  (add-hook 'c++-mode-hook 'company-mode)
  (add-hook 'c-mode-hook 'company-mode))

(use-package company-c-headers
  :ensure t)

(use-package company-irony
  :ensure t
  :config
  (setq company-backends '((company-c-headers
                            company-dabbrev-code
                            company-irony))))

(use-package irony
  :ensure t
  :config
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

;; Electric indent
;; (electric-indent-mode t)
;; Stop indenting previous line electric indent
(setq-default electric-indent-inhibit t)
;; Use spaces instead of tabs for indentation
(setq-default indent-tabs-mode nil)
;; Change indent style
(setq c-default-style "bsd")
;;(setq backward-delete-char-untabify-method 'hungry)
(c-set-offset 'arglist-intro          '+)
(c-set-offset 'arglist-close           0)
(c-set-offset 'statement-cont          0)
(c-set-offset 'topmost-intro          '0)
(c-set-offset 'topmost-intro-cont     '0)
(c-set-offset 'class-open             '0)
(c-set-offset 'block-close            '0)
(c-set-offset 'substatement-open      '0)
(c-set-offset 'func-decl-cont         '0)
(c-set-offset 'statement-case-open    '0)
(setq-default tab-width                4)
(setq-default c-basic-offset           4)
(c-set-offset 'brace-list-intro       '4)
(c-set-offset 'statement-block-intro  '4)
(c-set-offset 'defun-block-intro      '4)
(c-set-offset 'case-label             '4)
;; You can find more C offsets using M-x "c-set-offset"

;; Enable xterm mouse mode
(xterm-mouse-mode t)

;; Goto line
(global-set-key (kbd "C-l") 'goto-line)

;; Enable electric pair mode, automatically insert an extra ( or ) or { or }
;; (electric-pair-mode 1)

;; Enable { highlighting by default
(show-paren-mode 1)

;; Shrink/Enlarge buffers
;; (global-set-key (kbd "<C-s-M-up>") 'shrink-window)
;; (global-set-key (kbd "<C-s-M-down>") 'enlarge-window)
;; (global-set-key (kbd "<C-s-M-left>") 'shrink-window-horizontally)
;; (global-set-key (kbd "<C-s-M-right>") 'enlarge-window-horizontally)

;; Resize windows and move windows (Ace-window)
(bind-key* "C-e" 'resize-window)
(bind-key* "M-e" 'ace-window)
(setq aw-dispatch-always 1)
(setq aw-background nil)

(setq aw-keys '(?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
(defvar aw-dispatch-alist
  '((?x aw-delete-window "Delete Window")
    (?m aw-swap-window "Swap Windows")
    (?M aw-move-window "Move Window")
    (?c aw-copy-window "Copy Window")
    (?j aw-switch-buffer-in-window "Select Buffer")
    (?n aw-flip-window)
    (?u aw-switch-buffer-other-window "Switch Buffer Other Window")
    (?c aw-split-window-fair "Split Fair Window")
    (?v aw-split-window-vert "Split Vert Window")
    (?b aw-split-window-horz "Split Horz Window")
    (?o delete-other-windows "Delete Other Windows")
    (?? aw-show-dispatch-help))
  "List of actions for `aw-dispatch-default'.")

;;(global-set-key (kbd "<M-up>") 'windmove-up)
;;(global-set-key (kbd "<M-down>") 'windmove-down)
;;(global-set-key (kbd "<M-left>") 'windmove-left)
;;(global-set-key (kbd "<M-right>") 'windmove-right)

;; GPG binary
;;(setq epg-gpg-program "/usr/bin/gpg2")
(setf epa-pinentry-mode 'loopback)

;; Disable window dialogs 
(setq use-dialog-box nil)

;; Change yes-or-no questions into y-or-n questions
(defalias 'yes-or-no-p 'y-or-n-p)

;; Run Emacs server
(load "server")
(unless (server-running-p) (server-start))

;; Change default startup buffer message
(setq initial-scratch-message "Your Emacs server is running!")

;; Change default startup echo message
(defun display-startup-echo-area-message ()
  (message "Hello! :D"))

;; Run shell
(setq eshell-banner-message "")
(setq explicit-shell-file-name "/bin/bash")
(setq eshell-scroll-to-bottom-on-output "all")

(defun Toggle-terminal ()
  (interactive)
  (let (( buffer
          (save-window-excursion
            (ibuffer nil "*side-ibuffer*")
            (setq-local buffer-stale-function
            (lambda (&rest ignore) t))
               (eshell)         
            (current-buffer))))
    (pop-to-buffer buffer
                   '(display-buffer-in-side-window
                     (side . bottom)))))
(global-set-key (kbd "C-<f3>") 'Toggle-terminal)

;; List Flycheck errors
(global-set-key (kbd "C-<f4>") 'flycheck-list-errors)

;; Run latest terminal command
(defun Run-last-command ()
  (interactive)
  ;; Put your commands below
   (switch-to-buffer "*eshell*")
   (eshell-previous-input 1)
   (eshell-send-input)
   (previous-buffer)
)
(global-set-key (kbd "<f2>") 'Run-last-command)

;; Toggle inline images in org-mode
(global-set-key (kbd "C-M-u") 'org-toggle-inline-images)
(setq org-startup-with-inline-images t)

;; Select all using CTRL + A
(global-set-key (kbd "C-a") 'mark-whole-buffer)

;; Cua-mode CTRL+V CTRL+C, and redo
(cua-mode t)
(global-set-key (kbd "M-SPC") 'set-mark-command)
(global-set-key (kbd "C-M-SPC") 'cua-set-rectangle-mark)

(setq cua-keep-region-after-copy t)

;(delete-selection-mode 1)
;(setq delete-selection-mode nil)

;; Disable autosave
;; (setq auto-save-default nil)

;; autosave original file instead of copy
;; (setq auto-save-visited-file-name t)

;; Disable backup
;; (setq backup-inhibited t)

;; Disable Toolbar
(tool-bar-mode -1)

;; Menu bar mode
(menu-bar-mode -1)

;; Visual line mode
;; (global-visual-line-mode 1)

;; Ido mode
;; (ido-mode 1)

;; Ivy mode
(ivy-mode 1)

;; Search text, search regexp, find text, find regexp
(global-set-key (kbd "C-f") 'swiper)

;;(progn
;; set arrow keys in isearch. left/right is backward/forward, up/down is history. press Return to exit
;;  (define-key isearch-mode-map (kbd "<up>") 'isearch-ring-retreat )
;;  (define-key isearch-mode-map (kbd "<down>") 'isearch-ring-advance )
;;  (define-key isearch-mode-map (kbd "<left>") 'isearch-repeat-backward)
;;  (define-key isearch-mode-map (kbd "<right>") 'isearch-repeat-forward)
;;  (define-key isearch-mode-map (kbd "<return>") 'isearch-repeat-forward)
;;  (define-key isearch-mode-map (kbd "DEL") 'isearch-del-char)

;;  (define-key minibuffer-local-isearch-map (kbd "<left>") 'isearch-reverse-exit-minibuffer)
;;  (define-key minibuffer-local-isearch-map (kbd "<right>") 'isearch-forward-exit-minibuffer))

;; Replace regexp
(global-set-key (kbd "C-h") 'query-replace-regexp)

;; St Terminal compatibility Search, selection and CUA using shift, etc...
;; (keyboard-translate ?\C-h ?\C-?)
(global-set-key (kbd "C-@") 'company-complete-common)
(global-set-key (kbd "C-M-@") (kbd "C-M-SPC"))
(define-key input-decode-map "\^[[1;5P" (kbd "C-<f1>"))
(define-key input-decode-map "\^[[1;5Q" (kbd "C-<f2>"))
(define-key input-decode-map "\^[[1;5R" (kbd "C-<f3>"))
(define-key input-decode-map "\^[[1;5S" (kbd "C-<f4>"))
(define-key input-decode-map "\e[1;2A" [S-up])
(define-key input-decode-map "\e[1;2B" [S-down])
(define-key input-decode-map "\e[1;3D" [M-left])
(define-key input-decode-map "\e[1;3C" [M-right])
(define-key input-decode-map "\e[1;5D" [C-left])
(define-key input-decode-map "\e[1;5C" [C-right])
(define-key input-decode-map "\M-[1;6D" [C-S-left])
(define-key input-decode-map "\M-[1;6C" [C-S-right])
(define-key input-decode-map "\M-[1;5A" [C-up])
(define-key input-decode-map "\M-[1;5B" [C-down])
(define-key input-decode-map "\M-[1;6A" [S-C-up])
(define-key input-decode-map "\M-[1;6B" [S-C-down])

;; Suspend frame
(global-set-key (kbd "s-s") 'suspend-frame)

;; Kill buffer
(defun volatile-kill-buffer ()
   "Kill current buffer unconditionally."
   (interactive)
   (let ((buffer-modified-p nil))
     (kill-buffer (current-buffer))))
(global-set-key (kbd "M-w") 'volatile-kill-buffer)     ;; Unconditionally kill unmodified buffers.

;; Close window
(global-set-key (kbd "C-M-w") 'delete-window)

;; Close buffer and window
(global-set-key (kbd "C-w") 'kill-buffer-and-window)

;; Quit Emacs
(global-set-key (kbd "C-q") 'save-buffers-kill-emacs)

;; New buffer/switch buffer
(global-set-key (kbd "M-q") 'switch-to-buffer)

;; Save buffer
(global-set-key (kbd "C-s") 'save-buffer)

;; Search for files by name, find files
;; (global-set-key (kbd "C-M-f") 'locate)

;; Search for a specific string in multiple files
(global-set-key (kbd "C-M-f") 'find-grep)

;; Open link in org mode
(global-set-key (kbd "S-SPC") 'org-open-at-point)

;; Hide emphasis markers
(setq org-hide-emphasis-markers t)

;; Disable the splash screen (t/0)
(setq inhibit-splash-screen t)

;; Change text scale, zoom in zoom out
(global-set-key (kbd "<C-mouse-4>") 'text-scale-increase)
(global-set-key (kbd "<C-mouse-5>") 'text-scale-decrease)
(global-set-key (kbd "C-+") 'text-scale-decrease)
(global-set-key (kbd "C--") 'text-scale-increase)
(global-set-key (kbd "C-0") (kbd "C-x C-0"))


;; Enable transient mark mode
(transient-mark-mode 1)

;; Custom set variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cua-keep-region-after-copy t)
 '(custom-enabled-themes (quote (wombat)))
 '(package-selected-packages
   (quote
    (swiper flycheck use-package dired-ranger ace-window resize-window dired-subtree))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(region ((t (:background "#0066ff" :foreground "#f6f3e8"))))

 '(company-tooltip ((t (:background "#eeeeee" :foreground "#252525"))))
 '(company-tooltip-annotation ((t (:foreground "#252525"))))
 '(company-tooltip-selection ((t (:background "#0066ff" :foreground"#ffffff"))))
 '(company-tooltip-common-selection ((t (:background "#0066ff"))))
 '(company-scrollbar-bg ((t (:background "#686868" :inherit company-tooltip))))
 '(company-scrollbar-fg ((t (:background "#808080"))))
 '(company-echo ((t nil)))
'(company-tooltip-common ((t (:inherit nil :foreground "#252525")))))
