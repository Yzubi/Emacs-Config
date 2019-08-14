;; Enable Melpa
(package-initialize)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

;; Autocomplete Hippie expand
(defun my-hippie-expand-completions (&optional hippie-expand-function)
  "Return the full list of possible completions generated by `hippie-expand'.
The optional argument can be generated with `make-hippie-expand-function'."
  (require 'cl)
  (let ((this-command 'my-hippie-expand-completions)
        (last-command last-command)
        (buffer-modified (buffer-modified-p))
        (hippie-expand-function (or hippie-expand-function 'hippie-expand)))
    (flet ((ding)) ; avoid the (ding) when hippie-expand exhausts its options.
      (while (progn
               (funcall hippie-expand-function nil)
               (setq last-command 'my-hippie-expand-completions)
               (not (equal he-num -1)))))
    ;; Evaluating the completions modifies the buffer, however we will finish
    ;; up in the same state that we began.
    (set-buffer-modified-p buffer-modified)
    ;; Provide the options in the order in which they are normally generated.
    (delete he-search-string (reverse he-tried-table))))

(defmacro my-ido-hippie-expand-with (hippie-expand-function)
  "Generate an interactively-callable function that offers ido-based completion
using the specified hippie-expand function."
  `(call-interactively
    (lambda (&optional selection)
      (interactive
       (let ((options (my-hippie-expand-completions ,hippie-expand-function)))
         (if options
             (list (ido-completing-read "Completions: " options)))))
      (if selection
          (he-substitute-string selection t)
        (message "No expansion found")))))

(defun my-ido-hippie-expand ()
  "Ido-based completion for the word at point."
  (interactive)
  (my-ido-hippie-expand-with 'hippie-expand))

(global-set-key (kbd "<C-SPC>") 'my-ido-hippie-expand)

;; Stop indenting previous line electric indent
(setq-default electric-indent-inhibit t)

;; Enable xterm mouse mode
(xterm-mouse-mode t)

;; Goto line
(global-set-key (kbd "C-g") 'goto-line)

;; Enable { highlighting by default
(show-paren-mode 1)

;; Shrink/Enlarge buffers
(global-set-key (kbd "<C-s-M-up>") 'shrink-window)
(global-set-key (kbd "<C-s-M-down>") 'enlarge-window)
(global-set-key (kbd "<C-s-M-left>") 'shrink-window-horizontally)
(global-set-key (kbd "<C-s-M-right>") 'enlarge-window-horizontally)
(global-set-key (kbd "<M-up>") 'windmove-up)
(global-set-key (kbd "<M-down>") 'windmove-down)
(global-set-key (kbd "<M-left>") 'windmove-left)
(global-set-key (kbd "<M-right>") 'windmove-right)

;; GPG binary
;;(setq epg-gpg-program "/usr/bin/gpg2")
(setf epa-pinentry-mode 'loopback)

;; Disable window dialogs 
(setq use-dialog-box nil)

;; Y or N exit
(fset 'yes-or-no-p 'y-or-n-p)

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
(ido-mode 1)

;; Search regexp
(global-set-key (kbd "C-f") 'isearch-forward-regexp)
(define-key isearch-mode-map (kbd "<down>") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "<up>") 'isearch-repeat-backward)
(define-key isearch-mode-map (kbd "<return>") 'isearch-repeat-forward)

;; Replace regexp
(global-set-key (kbd "C-h") 'query-replace-regexp)

;; St Terminal compatibility Search, selection and CUA using shift, etc...
;; (keyboard-translate ?\C-h ?\C-?)
(define-key isearch-mode-map (kbd "<RET>") 'isearch-repeat-forward)
(global-set-key (kbd "C-@") 'my-ido-hippie-expand)
(global-set-key (kbd "C-M-@") (kbd "C-M-SPC"))
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
(global-set-key (kbd "C-w") 'kill-buffer-and-window)     ;; Unconditionally kill unmodified buffers.

;; Kill frame
(global-set-key (kbd "C-M-w") 'save-buffers-kill-terminal)

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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cua-keep-region-after-copy t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(region ((t (:background "navy" :foreground "#f6f3e8")))))
