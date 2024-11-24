;; フォントの変更
;; 最初は上の設定でしてたけどなぜか読み込まれないので下の設定に変更
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(default ((t (:background nil)))))
;; ;; フォントサイズの設定
;; (set-face-attribute 'default nil
;; ;; :family "VL ゴシック"
;; :height 200)
(add-to-list 'default-frame-alist
             '(font . "menlo-20"))
(set-fontset-font "fontset-default" 'unicode "Hiragino Kaku Gothic ProN")

;; load-pathを追加する関数を定義
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
	     (expand-file-name (concat user-emacs-directory path))))
	(add-to-list 'load-path default-directory)
	(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
    (normal-top-level-add-subdirs-to-load-path))))))

(add-to-load-path "elisp" "conf" "public_repos")

(require 'package) ; package.elを有効化
;; パッケージリポジトリにMarmaladeとMELPAを追加
(add-to-list 'package-archives
	     '("marmalade"."https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
	     '("melpa"."https://melpa.org/packages/"))

(package-initialize) ; インストール済みのElispを読み込む

;; 折り返しトグルコマンド
(define-key global-map (kbd "C-c |") 'toggle-truncate-lines)

;; "C-t"でウィンドウを切り替える。初期値はtranspose-chars
(define-key global-map (kbd "C-t") 'other-window)

;; カラム番号も表示
(column-number-mode t)

;; ファイルサイズを表示
(size-indication-mode t)

;; 時計を表示（好みに応じてフォーマットを変更可能）
(setq display-time-day-and-date t)
(setq display-time-24hr-format t)
(display-time-mode t)
;; バッテリー残量を表示
(display-battery-mode t)

;; リージョン選択範囲の行数・文字数カウント
(defun count-lines-and-chars ()
  (if mark-active
      (format "(%dlines,%dchars) "
	      (count-lines (region-beginning) (region-end))
	      (- (region-end) (region-beginning)))
    ""))

;; 本だと'default-mode-line-formatに追加していたが、それをするとエラー
;; 以下のようにdefault除いたらできたのでいったんそれで
(add-to-list 'mode-line-format
	     '(:eval (count-lines-and-chars)))

;; タイトルバーにファイルのフルパスを表示
(setq frame-title-format "%f")

;; 行番号を常に表示する
;; 本ではglobal-linum-modeが紹介されていたがver26以降でしたのに変わったらしい
;; (global-display-line-numbers-mode nil)

;; ファイルが#!から始まる場合、+xをつけて保存する
(add-hook 'after-save-fook
	  'executable-make-buffer-file-executable-if-script-p)

;; テーマを設定
(load-theme 'modus-vivendi t)
  
;; auto-completeの設定
(when (require 'auto-complete-config nil t)
  (define-key ac-mode-map (kbd "M-<tab>") 'auto-complete)
  (ac-config-default)
  (setq ac-use-menu-map t)
  (setq ac-ignore-case nil))

;; moccur-editの設定
(require 'moccur-edit nil t)

;; wgrepの設定
(require 'wgrep nil t)

;; undohistの設定
(when (require 'undohist nil t)
  (undohist-initialize))

;; color-moccurの設定
(when (require 'color-moccur nil t)
  ;; スペース区切りでAND検索
  (setq moccur-split-word t)
  ;; ディレクトリ検索のときの除外するファイル
  (add-to-list 'dmoccur-exclusion-mask "\\.DS_Store")
  (add-to-list 'dmoccur-exclusion-mask "^#.+#$"))

;; howmメモ保存の場所
(setq howm-directory (concat user-emacs-directory "howm"))
;; howm-menuの言語を日本語に
(setq howm-menu-lang 'ja)
;; howmメモを1日1ファイルにする
; (setq howm-file-name-format "%Y/%m/%Y-%m-%d.howm")
;; howm-modeを読み込む
(when (require 'howm-mode nil t)
  ;; C-c,,でhowm-menuを起動
  (define-key global-map (kbd "C-c ,,") 'howm-menu))

;; cua-modeの設定
(cua-mode t) ; cua-modeをオン
(setq cua-enable-cua-keys nil) ; CUAキーバインドを無効にする

;; macのパスをEMACSへ連携
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))
(setenv "CLASSPATH" "/Library/Java/Extensions/mysql-connector-j-9.0.0.jar:/Library/Java/Extensions/postgresql-42.7.4.jar")

;; projcetile
(when (require 'projectile nil t)
  ;;自動的にプロジェクト管理を開始
  (projectile-mode)
  ;; プロジェクト管理から除外するディレクトリを追加
  (add-to-list
    'projectile-globally-ignored-directories
    "node_modules")
  ;; プロジェクト情報をキャッシュする
  (setq projectile-enable-caching t))

;; 絵文字入力
(when (require 'ac-emoji nil t)
  ;; text-modeとmarkdown-modeでauto-completeを有効にする
  (add-to-list 'ac-modes 'text-mode)
  (add-to-list 'ac-modes 'markdown-mode)
  ;; text-modeとmarkdown-modeでac-emojiを有効にする
  (add-hook 'text-mode-hook 'ac-emoji-setup)
  (add-hook 'markdown-mode-hook 'ac-emoji-setup))

