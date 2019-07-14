;; Enable Melpa
(package-initialize)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

;; Shrink/Enlarge buffers
(global-set-key (kbd "<C-s-M-up>") 'shrink-window)
(global-set-key (kbd "<C-s-M-down>") 'enlarge-window)
(global-set-key (kbd "<C-s-M-left>") 'shrink-window-horizontally)
(global-set-key (kbd "<C-s-M-right>") 'enlarge-window-horizontally)
(global-set-key (kbd "<M-s-up>") 'windmove-up)
(global-set-key (kbd "<M-s-down>") 'windmove-down)
(global-set-key (kbd "<M-s-left>") 'windmove-left)
(global-set-key (kbd "<M-s-right>") 'windmove-right)

;; Disable window dialogs 
(setq use-dialog-box nil)

;; Y or N exit
(fset 'yes-or-no-p 'y-or-n-p)

;; Run Emacs server
(server-start)

;; Run emacs shell
(setq eshell-banner-message "")

(defun Start-Terminal-Custom ()
  (interactive)
  
  ;; Put your commands below
  (split-window-vertically)
  (windmove-down)
  (eshell)
  (fit-window-to-buffer)
  (windmove-up)
)

(global-set-key (kbd "C-M-g") 'Start-Terminal-Custom)

;; Toggle inline images in org-mode
(global-set-key (kbd "C-M-u") 'org-toggle-inline-images)
(setq org-startup-with-inline-images t)

;; Select all using CTRL + A
(global-set-key (kbd "C-a") 'mark-whole-buffer)

;; Cua-mode CTRL+V CTRL+C
(cua-mode t)

;;  Disable autosave
;; (setq auto-save-default nil)

;; Disable Toolbar
(tool-bar-mode -1)

;; Menu bar mode
(menu-bar-mode -1)

;; Visual line mode
(global-visual-line-mode 1)

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

;; Custom set variables
(custom-set-variables
 '(package-selected-packages
   (quote
    ())))
(custom-set-faces)
