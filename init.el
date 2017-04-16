;; init.elファイルを基準ディレクトリに設定する。
(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))
