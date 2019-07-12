;; Enable Melpa
(package-initialize)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

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
(custom-set-faces
 )
