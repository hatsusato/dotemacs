;; Create backup files in ~/.emacs.d/backup
(let ((my-backup-dir (concat user-emacs-directory "backup/")))
  (add-to-list 'auto-save-file-name-transforms
               (list ".*" my-backup-dir t) t)
  (add-to-list 'backup-directory-alist
               (cons "." my-backup-dir) t))
;; Show (row,column) number
(column-number-mode 1)
;; Enable to delete region
(delete-selection-mode 1)
;; Enable minibuffer completion
(icomplete-mode 1)
;; Use space as indent
(setq-default indent-tabs-mode nil)
;; Disable start page
(setq inhibit-startup-message t)
;; Costomizable initial scratch message
(setq initial-scratch-message
      (concat initial-scratch-message
	      ";; /sudo::/path-to-file\n"))
;; Highlight parens
(show-paren-mode 1)
;; Show trailing space
(setq-default show-trailing-whitespace t)
;; Suppress question about following symlinks to version controlled files
(setq vc-follow-symlinks t)
;; yes/no -> y/n
(defalias 'yes-or-no-p 'y-or-n-p)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("f5512c02e0a6887e987a816918b7a684d558716262ac7ee2dd0437ab913eaec6" default)))
 '(package-selected-packages
   (quote
    (zenburn-theme volatile-highlights undohist undo-tree smooth-scroll smartparens rainbow-delimiters multiple-cursors markdown-mode magit hlinum helm gitignore-mode gitconfig-mode flycheck bind-key auto-complete ace-jump-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
