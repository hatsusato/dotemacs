;; init.elファイルを基準ディレクトリに設定する。
;; emacs -q -l ~/path/to/somewhere/init.el
(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

;; パッケージをバージョンごとに管理する。
(let ((versioned-dir (locate-user-emacs-file emacs-version)))
  (setq el-get-dir (expand-file-name "el-get" versioned-dir)
        package-user-dir (expand-file-name "elpa" versioned-dir)))

;; el-getをインストールする。
(add-to-list 'load-path (expand-file-name "el-get" el-get-dir))
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

;; インストールするパッケージのリスト
(el-get-bundle use-package)

;; use-packageがインストールされていないときのための空の設定
(unless (require 'use-package nil t)
  (defmacro use-package (&rest args)))

;; C-b C-t
(defun my-transpose-chars()
  (interactive)
  (backward-char)
  (transpose-chars 1))
;; C-u in terminal
(defun my-cancel-line()
  (interactive)
  (kill-line 0))

;; グローバルなキー設定。
(use-package bind-key
  :config
  (bind-key* "C-h" 'backward-delete-char-untabify)
  (bind-key* "C-x C-b" 'bs-show)
  (bind-key "C-t" 'my-transpose-chars)
  (bind-key "C-u" 'my-cancel-line))
