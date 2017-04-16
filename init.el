;; init.elファイルを基準ディレクトリに設定する。
(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

;; パッケージをバージョンごとに管理する。
(let ((versioned-dir (locate-user-emacs-file emacs-version)))
  (setq el-get-dir (expand-file-name "el-get" versioned-dir)
        package-user-dir (expand-file-name "elpa" versioned-dir)))

;; el-getをインストールする。
(add-to-list 'load-path (locate-user-emacs-file "el-get/el-get"))
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

;; インストールするパッケージのリスト
(el-get-bundle use-package)
