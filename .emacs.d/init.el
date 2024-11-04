;; init-loaderの設定
(require 'init-loader)
(setq init-loader-show-log-after-init 'error-only)
(init-loader-load "~/.emacs.d/conf") ; 設定ファイルがあるディレクトリを指定

;; multi-termの日本語表示対応
(setenv "LANG" "ja_JP.UTF-8")

;; 最終行の有無を確認したい
(setq-default indicate-empty-lines t)
(setq-default indicate-buffer-boundaries 'left)

;; undo-treeの設定
(when (require 'undo-tree nil t)
  ;; C-'にリドゥを割り当てる
  (define-key global-map (kbd "C-'") 'undo-tree-redo)
  (global-undo-tree-mode))

;; ElScreenのプレフィックスキーを変更する（初期値はC-z）
;; (setq elscreen-prefix-key (kbd "C-t"))
(when (require 'elscreen nil t)
  (elscreen-start)
  ;; C-z C-zをタイプした場合にデフォルトのC-zを利用する
  (if window-system
      (define-key elscreen-map (kbd "C-z") 'iconify-or-deiconify-frame)
    (define-key elscreen-map (kbd "C-z") 'suspend-emacs)))

;; point-undoの設定
(when (require 'point-undo nil t)
  ;;(define-key global-map [f5] 'point-undo)
  ;;(define-key global-map [f6] 'point-redo)
  ;; 筆者のお勧めキーバインド
  (define-key global-map (kbd "M-[") 'point-undo)
  (define-key global-map (kbd "M-]") 'point-redo)
  )

;; web-mode用の設定
(when (require 'web-mode nil t)
  ;; 自動的にweb-modeを起動したい拡張子を追加する
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.ctp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  ;; web-modeのインデント設定用フック
  (defun web-mode-hook ()
    "Hooks for Web mode."
    (setq web-mode-markup-indent-offset 2) ; HTMLのインデイント
    (setq web-mode-css-indent-offset 2) ; CSSのインデント
    (setq web-mode-code-indent-offset 2) ; JS, PHP, Rubyなどのインデント
    (setq web-mode-comment-style 2) ; web-mode内のコメントのインデント
    (setq web-mode-style-padding 1) ; <style>内のインデント開始レベル
    (setq web-mode-script-padding 1) ; <script>内のインデント開始レベル
    )
  (add-hook 'web-mode-hook  'web-mode-hook)
)
(put 'narrow-to-region 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("8b4af9cacdaaf0a85e968abb2111f563cf82a16a005b7fac6f6026cc5d13bd10" "45020ff9acfe0b482e86f300717f11c6a0003270e710d3b46504e5d125cdfd67" "95b51aab1acd95ebcc7f47a60dd02d1a6b4b2c4aa68027b6d4138c2f70c583ae" "b9f44212b4be6f0466811c5d8a297dda3c40dbf4c4cfd97c1686fceb2043b617" "9d02b3fe8c196e675a7f0ffe48f222b835e2e5d03cf1834e9013cb4358694bcc" "b3775ba758e7d31f3bb849e7c9e48ff60929a792961a2d536edec8f68c671ca5" "9b59e147dbbde5e638ea1cde5ec0a358d5f269d27bd2b893a0947c4a867e14c1" "96998f6f11ef9f551b427b8853d947a7857ea5a578c75aa9c4e7c73fe04d10b4" "e0d42a58c84161a0744ceab595370cbe290949968ab62273aed6212df0ea94b4" "3cc2385c39257fed66238921602d8104d8fd6266ad88a006d0a4325336f5ee02" "e9776d12e4ccb722a2a732c6e80423331bcb93f02e089ba2a4b02e85de1cf00e" "58c6711a3b568437bab07a30385d34aacf64156cc5137ea20e799984f4227265" "c48551a5fb7b9fc019bf3f61ebf14cf7c9cdca79bcb2a4219195371c02268f11" "ba4f725d8e906551cfab8c5f67e71339f60fac11a8815f51051ddb8409ea6e5c" "5b7c9906ed176f4a461af7655e1b67f6a5e3066fd1a3c3efa06bce2ebae6d7d3" "09b833239444ac3230f591e35e3c28a4d78f1556b107bafe0eb32b5977204d93" default))
 '(package-selected-packages
   '(treemacs-all-the-icons all-the-icons treemacs company clj-refactor cider clojure-mode doom-modeline doom-themes vertico-prescient nerd-icons-completion vertico-posframe nerd-icons vertico elfeed-org elfeed arjen-grey-theme chocolate-theme afternoon-theme github-dark-vscode-theme greymatters-theme sublime-themes material-theme calmer-forest-theme zenburn-theme howm elscreen undo-tree undohist wgrep multi-term htmlize helm-c-moccur auto-complete package-utils emojify org-ac org2blog multi-vterm magit ac-emoji helm-projectile projectile helm-gtags gtags exec-path-from-shell quickrun web-mode howm elscreen undo-tree undohist wgrep multi-term htmlize helm-c-moccur auto-complete)))
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
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil)))))
