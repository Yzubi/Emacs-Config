;; Enable Melpa
(package-initialize)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

;; CTRL + Enter = enable/disable square selection

;; Do you want to use company-mode or auto-complete?

;; Start company-mode
;; (setq company-dabbrev-downcase nil)
;; (global-company-mode)

;; Enable { highlighting by default
(show-paren-mode 1)

;; Start auto-complete
;; Install ini-mode for ini files
(require 'auto-complete)
(require 'auto-complete-config)
; (setq ac-modes '(c++-mode c-mode sql-mode ini-mode))
(global-auto-complete-mode t)
(ac-config-default)
;; Bind key to autocomplete
(global-set-key (kbd "C-SPC") 'auto-complete)
;; Disable realtime auto-completion listing 
(setq ac-auto-start nil)

;; Shrink/Enlarge buffers
(global-set-key (kbd "<C-s-M-up>") 'shrink-window)
(global-set-key (kbd "<C-s-M-down>") 'enlarge-window)
(global-set-key (kbd "<C-s-M-left>") 'shrink-window-horizontally)
(global-set-key (kbd "<C-s-M-right>") 'enlarge-window-horizontally)
(global-set-key (kbd "<M-s-up>") 'windmove-up)
(global-set-key (kbd "<M-s-down>") 'windmove-down)
(global-set-key (kbd "<M-s-left>") 'windmove-left)
(global-set-key (kbd "<M-s-right>") 'windmove-right)

;; GPG binary
(setq epg-gpg-program "/usr/bin/gpg2")
(setf epa-pinentry-mode 'loopback)

;; Disable window dialogs 
(setq use-dialog-box nil)

;; Y or N exit
(fset 'yes-or-no-p 'y-or-n-p)

;; Run Emacs server
;; (server-start)

;; Run shell
(setq eshell-banner-message "")
(setq explicit-shell-file-name "/bin/bash")

(defun Start-Terminal-Custom ()
  (interactive)
  
  ;; Put your commands below
  (split-window-vertically)
  (windmove-down)
  ;; Use "term explicit-shell-file-name" or "eshell"
  (eshell)
  (fit-window-to-buffer)
  (windmove-up)
)

(global-set-key (kbd "C-M-g") 'Start-Terminal-Custom)

;; Run latest terminal command
(global-set-key (kbd "<f2>") (kbd "<s-M-down><up><return><M-s-up>"))

;; Toggle inline images in org-mode
(global-set-key (kbd "C-M-u") 'org-toggle-inline-images)
(setq org-startup-with-inline-images t)

;; Select all using CTRL + A
(global-set-key (kbd "C-a") 'mark-whole-buffer)

;; Cua-mode CTRL+V CTRL+C
(cua-mode t)

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
(ido-mode 1)

;; Search
(global-set-key (kbd "C-f") 'isearch-forward)
(define-key isearch-mode-map (kbd "C-f") 'isearch-repeat-forward)
(global-unset-key ["C-s"])

;; Suspend frame
(global-set-key (kbd "s-s") 'suspend-frame)

;; Kill buffer
(defun volatile-kill-buffer ()
   "Kill current buffer unconditionally."
   (interactive)
   (let ((buffer-modified-p nil))
     (kill-buffer (current-buffer))))
(global-set-key (kbd "C-w") 'volatile-kill-buffer)     ;; Unconditionally kill unmodified buffers.

;; New buffer/switch buffer
(global-set-key (kbd "M-q") 'switch-to-buffer)

;; Save buffer
(global-set-key (kbd "C-s") 'save-buffer)

;; Search files
(global-set-key (kbd "C-l") 'locate)

;; Open link in org mode
(global-set-key (kbd "S-SPC") 'org-open-at-point)

;; Hide emphasis markers
(setq org-hide-emphasis-markers t)

;; Disable the splash screen (t/0)
(setq inhibit-splash-screen t)

;; Enable transient mark mode
(transient-mark-mode 1)

;; Shift number
(global-set-key (kbd "M-+") 'shift-number-up)
(global-set-key (kbd "M-_") 'shift-number-down)

;; Custom set variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (shift-number ini-mode auto-complete))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
