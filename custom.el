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
(let ((extra-scratch-message
       (mapconcat 'identity
                  '(";; /sudo::/path-to-file"
                    ";; /ssh:user@host:/path-to-file"
                    "")
                  "\n")))
  (setq initial-scratch-message
        (concat initial-scratch-message extra-scratch-message)))
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
 '(package-selected-packages (quote (undo-tree smooth-scroll)))
 '(custom-safe-themes
   (quote
    ("a58cc789698ca7c1059749f08baccd51702f53a7bf757509c3a1153769f0f60d" default)))
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t :background "transparent"))))
