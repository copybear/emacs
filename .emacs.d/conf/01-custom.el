(defun org2wpmd ()
  (interactive)

  (require 'ox-md nil t)

  (let ((org-export-with-toc nil) 
    (delete-active-region nil) ; for backward-delete-char
    orgstr mdstr p1 p2 es)
    (setq orgstr
      (if (use-region-p)
          (buffer-substring (region-beginning) (region-end))
        ;; no region
        (buffer-string)
        ))

    (with-current-buffer (get-buffer-create "*org2wpmd*")
      (erase-buffer)
      (insert "* TOP-LEVEL-HEADING\n") ; Dirty workaround to make '* ...' the top-level heading. 
      (insert orgstr)

      ;; BEGIN_SRC ... END_SRC
      (goto-char (point-min))
      (while (re-search-forward "^#\\+BEGIN_SRC\\( .*\\|\\)\n" nil t)
    ;; BEGIN_EXPORT html, for escape from (org-md-convert-region-to-md)  
    (replace-match "#+BEGIN_EXPORT html
<pre class=\"wp-block-code\"><code>" t)
    (setq p1 (point))
    (when (re-search-forward "\n#\\+END_SRC *" nil t)
      (replace-match "")
      (setq p2 (point))
      (insert "<\/code><\/pre>
#+END_EXPORT")
      (setq es (org-html-do-format-code (buffer-substring p1 p2)))
      (delete-region p1 p2)
      (goto-char p1)(insert es)(backward-delete-char 1)
      )
    )

      ;; convert to md
      (set-mark (point-min))
      (goto-char (point-max))
      (org-md-convert-region-to-md)

      ;; ](file:/// ... => ](/ ...
      (goto-char (point-min))
      (while (re-search-forward "\\](file:\/\/\/" nil t)
    (replace-match "](/")
    )

      ;; delete '# TOP-LEVEL-HEADING'
      (goto-char (point-min))
      (next-line 3)
      (delete-region (point-min) (point))

      ;; set kill
      (kill-new (buffer-string))
      )
    ))

;; Helmから切り替えを検討するため一旦コメントアウトHelmの設定
;; (global-set-key (kbd "M-x") 'helm-M-x) (setq helm-M-x-fuzzy-match
;; t) ;; optional fuzzy matching for helm-M-x (global-set-key (kbd
;; "M-y") 'helm-show-kill-ring) (global-set-key (kbd "C-x b")
;; 'helm-mini) (setq helm-buffers-fuzzy-matching t
;; helm-recentf-fuzzy-match t) (global-set-key (kbd "C-x C-f")
;; 'helm-find-files) ;; M-yにhelm-show-kill-ringを割り当てる
;; (define-key global-map (kbd "M-y") 'helm-show-kill-ring) ;; errorの
;; ためhelm関連をコメントアウト;; (helm-autoresize-mode t) ;;
;; (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
;; ; re;;bind tab to do persistent action ;; (define-key helm-map (kbd
;; "C-i") 'helm-execute-persistent-action) ; make ;;TAB works in
;; terminal ;; (define-key helm-map (kbd "C-z") 'helm-select-action) ;
;; list actions using C-z ;; (add-to-list
;; 'helm-sources-using-default-as-input 'helm-source-man-pages) ;; M-o
;; にhelm-moccurを割り当て(define-key global-map (kbd "M-o")
;; 'helm-occur) ;; Fuzzyマッチを無効にする;; (setq
;; helm-projectile-fuzzy-match nil) (when (require 'helm-projectile
;; nil t) (setq projectile-completion-system 'helm))
;; (custom-set-variables ;; custom-set-variables was added by Custom.
;; ;; If you edit it by hand, you could mess it up, so be careful.  ;;
;; Your init file should contain only one such instance.  ;; If there
;; is more than one, they won't work right.
;; '(package-selected-packages '(howm elscreen undo-tree undohist
;; wgrep multi-term htmlize helm-c-moccur auto-complete package-utils
;; emojify org-ac org2blog multi-vterm magit ac-emoji helm-projectile
;; projectile helm-gtags gtags exec-path-from-shell quickrun web-mode
;; howm elscreen undo-tree undohist wgrep multi-term htmlize
;; helm-c-moccur auto-complete))) (custom-set-faces ;;
;; custom-set-faces was added by Custom.  ;; If you edit it by hand,
;; you could mess it up, so be careful.  ;; Your init file should
;; contain only one such instance.  ;; If there is more than one, they
;; won't work right.  )
;; vertico関連の設定 = start =

(add-hook 'after-init-hook #'vertico-mode)

;; 右サイトを参考に設定：https://tam5917.hatenablog.com/?page=1719649073
;; まだ内容はきちんと理解できていないけど一旦動いたのでこれベースで
(with-eval-after-load 'vertico
  (setq vertico-cycle t)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t)

  ;; vertico-directory
  (define-key vertico-map "DEL" #'vertico-directory-delete-char)
  (keymap-set vertico-map "C-l" #'vertico-directory-up)

  ;; vertico-quick
  (keymap-set vertico-map "M-q" #'vertico-quick-insert) ;; avy-likeに候補選択してミニバッファ入力
  (keymap-set vertico-map "C-q" #'vertico-quick-exit)   ;; avy-likeに候補選択して即時実行
  
  ;; 以下のサイトを参考にverticoにカーソルを導入し，かつカーソルの色をfaceにより指定した
  ;; https://qiita.com/nobuyuki86/items/4150d5ec433e62757951
  (require 'nerd-icons)
  (defvar +vertico-current-arrow t)
  (cl-defmethod vertico--format-candidate :around
    (cand prefix suffix index start &context
          ((and +vertico-current-arrow
                (not (bound-and-true-p vertico-flat-mode)))
           (eql t)))
    (setq cand (cl-call-next-method cand prefix suffix index start))
    (if (bound-and-true-p vertico-grid-mode)
        (if (= vertico--index index)
            (concat (nerd-icons-faicon "nf-fa-hand_o_right" :face 'nerd-icons-blue)
                    "\t" cand)
          (concat #("_" 0 1 (display " ")) cand))
      (if (= vertico--index index)
          (concat " " (nerd-icons-faicon "nf-fa-hand_o_right" :face 'nerd-icons-blue)
                  "\t" cand)
        (concat "\t" cand))))
  
  ;; Emacs 28 and newer: Hide commands in M-x which do not work in the current
  ;; mode.  Vertico commands are hidden in normal buffers. This setting is
  ;; useful beyond Vertico.
  (setq read-extended-command-predicate #'command-completion-default-include-p)
  )

;; wikiからもってきた。エラーにならないので動いていると思うけど何の役に立っているのか理解できていない
;; https://github.com/minad/vertico/wiki#prefix-current-candidate-with-arrow
;; 自動的にverticoを縮小する
(defun +embark-live-vertico ()
  "Shrink Vertico minibuffer when `embark-live' is active."
  (when-let (win (and (string-prefix-p "*Embark Live" (buffer-name))
                      (active-minibuffer-window)))
    (with-selected-window win
      (when (and (bound-and-true-p vertico--input)
                 (fboundp 'vertico-multiform-unobtrusive))
        (vertico-multiform-unobtrusive)))))
 
(add-hook 'embark-collect-mode-hook #'+embark-live-vertico)

;; 最後のadd-to-listのところがエラーになる。wikiから持ってきたけど一旦封印
;; https://github.com/minad/vertico/wiki#prefix-current-candidate-with-arrow
;; ;; 候補表示変換、カスタム候補強調表示
;; (defvar +vertico-transform-functions nil)
 
;; (cl-defmethod vertico--format-candidate :around
;;   (cand prefix suffix index start &context ((not +vertico-transform-functions) null))
;;   (dolist (fun (ensure-list +vertico-transform-functions))
;;     (setq cand (funcall fun cand)))
;;   (cl-call-next-method cand prefix suffix index start))
 
;; (defun +vertico-highlight-directory (file)
;;   "If FILE ends with a slash, highlight it as a directory."
;;   (if (string-suffix-p "/" file)
;;       (propertize file 'face 'marginalia-file-priv-dir) ; or face 'dired-directory
;;     file))
 
;; ;; function to highlight enabled modes similar to counsel-M-x
;; (defun +vertico-highlight-enabled-mode (cmd)
;;   "If MODE is enabled, highlight it as fontlock-constant-face."
;;   (let ((sym (intern cmd)))
;;     (if (or (eq sym major-mode)
;;             (and
;;              (memq sym minor-mode-list)
;;              (boundp sym)))
;;       (propertize cmd 'face 'font-lock-constant-face)
;;       cmd)))
 
;; ;; add-to-list works if 'file isn't already in the alist
;; ;; setq can be used but will overwrite all existing values
;; (add-to-list 'vertico-multiform-categories
;;              '(file
;;                ;; this is also defined in the wiki, uncomment if used
;;                ;; (vertico-sort-function . sort-directories-first)
;;                (+vertico-transform-functions . +vertico-highlight-directory)))
;; (add-to-list 'vertico-multiform-commands
;;              '(execute-extended-command 
;;                reverse
;;                (+vertico-transform-functions . +vertico-highlight-enabled-mode)))


;; どこからもってきたのか忘れてしまった一旦封印
;; (use-package vertico
;;   :ensure t
;;   :demand
;;   :config
;;   (setq vertico-cycle t)
;;   ;; currently requires melpa version of vertico
;;   (setq vertico-preselect 'directory)
;;   :init
;;   (vertico-mode)
;;   (defun my/vertico-insert ()
;;     (interactive)
;;     (let* ((mb (minibuffer-contents-no-properties))
;;            (lc (if (string= mb "") mb (substring mb -1))))
;;       (cond ((string-match-p "^[/~:]" lc) (self-insert-command 1 ?/))
;;             ((file-directory-p (vertico--candidate)) (vertico-insert))
;;             (t (self-insert-command 1 ?/)))))
;;   :bind (:map vertico-map
;;               ("/" . #'my/vertico-insert)))

;; エラー調査中
;; ;; Configure directory extension.
;; (use-package vertico-directory
;;   :after vertico
;;   :ensure t
;;   :demand
;;   ;; More convenient directory navigation commands
;;   :bind (:map vertico-map
;;               ("RET"   . vertico-directory-enter)
;;               ("DEL"   . vertico-directory-delete-char)
;;               ("M-DEL" . vertico-directory-delete-word))
;;   ;; Tidy shadowed file names
;;   :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; vertico関連の設定 = end =


;; doom-modeline
(require 'doom-modeline)
(doom-modeline-mode 1)
;; If non-nil, cause imenu to see `doom-modeline' declarations.
;; This is done by adjusting `lisp-imenu-generic-expression' to
;; include support for finding `doom-modeline-def-*' forms.
;; Must be set before loading doom-modeline.
(setq doom-modeline-support-imenu t)
;; How tall the mode-line should be. It's only respected in GUI.
;; If the actual char height is larger, it respects the actual height.
(setq doom-modeline-height 25)
;; How wide the mode-line bar should be. It's only respected in GUI.
(setq doom-modeline-bar-width 4)
;; The limit of the window width.
;; If `window-width' is smaller than the limit, some information won't be
;; displayed. It can be an integer or a float number. `nil' means no limit."
(setq doom-modeline-window-width-limit 85)
;; How to detect the project root.
;; nil means to use `default-directory'.
;; The project management packages have some issues on detecting project root.
;; e.g. `projectile' doesn't handle symlink folders well, while `project' is unable
;; to hanle sub-projects.
;; You can specify one if you encounter the issue.
(setq doom-modeline-project-detection 'auto)
;; Determines the style used by `doom-modeline-buffer-file-name'.
;;
;; Given ~/Projects/FOSS/emacs/lisp/comint.el
;;   auto => emacs/l/comint.el (in a project) or comint.el
;;   truncate-upto-project => ~/P/F/emacs/lisp/comint.el
;;   truncate-from-project => ~/Projects/FOSS/emacs/l/comint.el
;;   truncate-with-project => emacs/l/comint.el
;;   truncate-except-project => ~/P/F/emacs/l/comint.el
;;   truncate-upto-root => ~/P/F/e/lisp/comint.el
;;   truncate-all => ~/P/F/e/l/comint.el
;;   truncate-nil => ~/Projects/FOSS/emacs/lisp/comint.el
;;   relative-from-project => emacs/lisp/comint.el
;;   relative-to-project => lisp/comint.el
;;   file-name => comint.el
;;   file-name-with-project => FOSS|comint.el
;;   buffer-name => comint.el<2> (uniquify buffer name)
;;
;; If you are experiencing the laggy issue, especially while editing remote files
;; with tramp, please try `file-name' style.
;; Please refer to https://github.com/bbatsov/projectile/issues/657.
(setq doom-modeline-buffer-file-name-style 'auto)
;; Whether display icons in the mode-line.
;; While using the server mode in GUI, should set the value explicitly.
(setq doom-modeline-icon t)
;; Whether display the icon for `major-mode'. It respects option `doom-modeline-icon'.
(setq doom-modeline-major-mode-icon t)
;; Whether display the colorful icon for `major-mode'.
;; It respects `nerd-icons-color-icons'.
(setq doom-modeline-major-mode-color-icon t)
;; Whether display the icon for the buffer state. It respects option `doom-modeline-icon'.
(setq doom-modeline-buffer-state-icon t)
;; Whether display the modification icon for the buffer.
;; It respects option `doom-modeline-icon' and option `doom-modeline-buffer-state-icon'.
(setq doom-modeline-buffer-modification-icon t)
;; Whether display the lsp icon. It respects option `doom-modeline-icon'.
(setq doom-modeline-lsp-icon t)
;; Whether display the time icon. It respects option `doom-modeline-icon'.
(setq doom-modeline-time-icon t)
;; Whether display the time icon. It respects option `doom-modeline-icon'.
(setq doom-modeline-time-icon nil)
;; Whether to use an analogue clock svg as the live time icon.
;; It respects options `doom-modeline-icon', `doom-modeline-time-icon', and `doom-modeline-time-live-icon'.
(setq doom-modeline-time-analogue-clock nil)
;; Whether to use unicode as a fallback (instead of ASCII) when not using icons.
(setq doom-modeline-unicode-fallback nil)
;; Whether display the buffer name.
(setq doom-modeline-buffer-name t)
;; Whether highlight the modified buffer name.
(setq doom-modeline-highlight-modified-buffer-name t)
;; When non-nil, mode line displays column numbers zero-based.
;; See `column-number-indicator-zero-based'.
(setq doom-modeline-column-zero-based t)
;; Specification of \"percentage offset\" of window through buffer.
;; See `mode-line-percent-position'.
(setq doom-modeline-percent-position '(-3 "%p"))
;; Format used to display line numbers in the mode line.
;; See `mode-line-position-line-format'.
(setq doom-modeline-position-line-format '("L%l"))
;; Format used to display column numbers in the mode line.
;; See `mode-line-position-column-format'.
(setq doom-modeline-position-column-format '("C%c"))
;; Format used to display combined line/column numbers in the mode line. See `mode-line-position-column-line-format'.
(setq doom-modeline-position-column-line-format '("%l:%c"))
;; Whether display the minor modes in the mode-line.
(setq doom-modeline-minor-modes nil)
;; If non-nil, a word count will be added to the selection-info modeline segment.
(setq doom-modeline-enable-word-count nil)
;; Major modes in which to display word count continuously.
;; Also applies to any derived modes. Respects `doom-modeline-enable-word-count'.
;; If it brings the sluggish issue, disable `doom-modeline-enable-word-count' or
;; remove the modes from `doom-modeline-continuous-word-count-modes'.
(setq doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode))
;; Whether display the buffer encoding.
(setq doom-modeline-buffer-encoding t)
;; Whether display the indentation information.
(setq doom-modeline-indent-info t)
;; Whether display the total line number。
(setq doom-modeline-total-line-number t)
;; Whether display the icon of vcs segment. It respects option `doom-modeline-icon'."
(setq doom-modeline-vcs-icon t)
;; The maximum displayed length of the branch name of version control.
(setq doom-modeline-vcs-max-length 15)
;; The function to display the branch name.
(setq doom-modeline-vcs-display-function #'doom-modeline-vcs-name)
;; Whether display the icon of check segment. It respects option `doom-modeline-icon'.
(setq doom-modeline-check-icon t)
;; If non-nil, only display one number for check information if applicable.
(setq doom-modeline-check-simple-format nil)
;; The maximum number displayed for notifications.
(setq doom-modeline-number-limit 99)
;; Whether display the workspace name. Non-nil to display in the mode-line.
(setq doom-modeline-workspace-name t)
;; Whether display the perspective name. Non-nil to display in the mode-line.
(setq doom-modeline-persp-name t)
;; If non nil the default perspective name is displayed in the mode-line.
(setq doom-modeline-display-default-persp-name nil)
;; If non nil the perspective name is displayed alongside a folder icon.
(setq doom-modeline-persp-icon t)
;; Whether display the `lsp' state. Non-nil to display in the mode-line.
(setq doom-modeline-lsp t)
;; Whether display the GitHub notifications. It requires `ghub' package.
(setq doom-modeline-github t)
;; The interval of checking GitHub.
(setq doom-modeline-github-interval (* 30 60))
;; Whether display the modal state.
;; Including `evil', `overwrite', `god', `ryo' and `xah-fly-keys', etc.
(setq doom-modeline-modal t)
;; Whether display the modal state icon.
;; Including `evil', `overwrite', `god', `ryo' and `xah-fly-keys', etc.
(setq doom-modeline-modal-icon t)
;; Whether display the modern icons for modals.
(setq doom-modeline-modal-modern-icon t)
;; Whether display the gnus notifications.
(setq doom-modeline-gnus t)
;; Whether gnus should automatically be updated and how often (set to 0 or smaller than 0 to disable)
(setq doom-modeline-gnus-timer 2)
;; Wheter groups should be excludede when gnus automatically being updated.
(setq doom-modeline-gnus-excluded-groups '("dummy.group"))
;; Whether display the misc segment on all mode lines.
;; If nil, display only if the mode line is active.
(setq doom-modeline-display-misc-in-all-mode-lines t)
;; The function to handle `buffer-file-name'.
(setq doom-modeline-buffer-file-name-function #'identity)
;; The function to handle `buffer-file-truename'.
(setq doom-modeline-buffer-file-truename-function #'identity)
;; Whether display the environment version.
(setq doom-modeline-env-version t)
;; Or for individual languages
(setq doom-modeline-env-enable-python t)
(setq doom-modeline-env-enable-ruby t)
(setq doom-modeline-env-enable-perl t)
(setq doom-modeline-env-enable-go t)
(setq doom-modeline-env-enable-elixir t)
(setq doom-modeline-env-enable-rust t)
;; Change the executables to use for the language version string
(setq doom-modeline-env-python-executable "python") ; or `python-shell-interpreter'
(setq doom-modeline-env-ruby-executable "ruby")
(setq doom-modeline-env-perl-executable "perl")
(setq doom-modeline-env-go-executable "go")
(setq doom-modeline-env-elixir-executable "iex")
(setq doom-modeline-env-rust-executable "rustc")
;; What to display as the version while a new one is being loaded
(setq doom-modeline-env-load-string "...")
;; Hooks that run before/after the modeline version string is updated
(setq doom-modeline-before-update-env-hook nil)
(setq doom-modeline-after-update-env-hook nil)

;; verticoの絞り込みにあいまい検索を追加
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; consultのリストなどにメタ情報を表示する
(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

;; 最近使ったファイルを時々保存
(use-package recentf
  :config
  (setq recentf-max-saved-items 2000)
  (setq recentf-exclude '(".recentf" "^/ssh:"))
  (setq recentf-auto-cleanup 'never)

  (setq recentf-auto-save-timer
        (run-with-idle-timer 30 t 'recentf-save-list))
  (recentf-mode 1)
  ;; recentf の メッセージをエコーエリア(ミニバッファ)に表示しない
  ;; (*Messages* バッファには出力される)
  (defun recentf-save-list-inhibit-message:around (orig-func &rest args)
    (setq inhibit-message t)
    (apply orig-func args)
    (setq inhibit-message nil)
    'around)
  (advice-add 'recentf-cleanup   :around 'recentf-save-list-inhibit-message:around)
  (advice-add 'recentf-save-list :around 'recentf-save-list-inhibit-message:around)
  )


(require 'org-ac)
;; Make config suit for you. About the config item, eval the following sexp.
;; (customize-group "org-ac")
(org-ac/config-default)

;---- emojify ----
(when (locate-library "emojify")
  (require 'emojify)
  (global-emojify-mode)
  )


;; Elfeedの設定最初はこっちでやろうとしたができなかったので下に変更
;; ;; Configure Elfeed
;; (use-package elfeed
;;   :custom
;;   (elfeed-db-directory
;;    (expand-file-name "elfeed" user-emacs-directory))
;;    (elfeed-show-entry-switch 'display-buffer)
;;   :bind
;;   ("C-c w e" . elfeed))
;; (setq elfeed-feeds
;;  '(("https://feeds.maketecheasier.com/MakeTechEasier" tech news)
;;    ("https://solar.lowtechmagazine.com/feeds/all-en.atom.xml" tech)
;;    ("https://old.reddit.com/r/f1technical.rss" tech f1)))


;; use an org file to organise feeds
(use-package elfeed-org
 :ensure t
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files (list "~/.emacs.d/elfeed/elfeed.org")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; elfeed feed reader                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;shortcut functions
(defun bjm/elfeed-show-all ()
  (interactive)
  (bookmark-maybe-load-default-file)
  (bookmark-jump "elfeed-all"))
(defun bjm/elfeed-show-emacs ()
  (interactive)
  (bookmark-maybe-load-default-file)
  (bookmark-jump "elfeed-emacs"))
(defun bjm/elfeed-show-daily ()
  (interactive)
  (bookmark-maybe-load-default-file)
  (bookmark-jump "elfeed-daily"))

;;functions to support syncing .elfeed between machines
;;makes sure elfeed reads index from disk before launching
(defun bjm/elfeed-load-db-and-open ()
  "Wrapper to load the elfeed db from disk before opening"
  (interactive)
  (elfeed-db-load)
  (elfeed)
  (elfeed-search-update--force))

;;write to disk when quiting
(defun bjm/elfeed-save-db-and-bury ()
  "Wrapper to save the elfeed db to disk before burying buffer"
  (interactive)
  (elfeed-db-save)
  (quit-window))
(use-package elfeed
  :ensure t
  :bind (:map elfeed-search-mode-map
              ("A" . bjm/elfeed-show-all)
              ("E" . bjm/elfeed-show-emacs)
              ("D" . bjm/elfeed-show-daily)
              ("q" . bjm/elfeed-save-db-and-bury)))

;; 使えなかったので削除した
;; ;; Twittering-mode
;; (require 'twittering-mode)
;; (setq twittering-use-master-password t)
;; (setq twittering-allow-insecure-server-cert t)

;; Clojure設定
(use-package clojure-mode)
(use-package cider)
(use-package cider
  :init
  (add-hook 'cider-mode-hook #'clj-refactor-mode)
  (add-hook 'cider-mode-hook #'company-mode)
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'company-mode)
  (add-hook 'cider-repl-mode-hook #'eldoc-mode)
  :diminish subword-mode
  :config
  (setq nrepl-log-messages t
        cider-repl-display-in-current-window t
        cider-repl-use-clojure-font-lock t
        cider-prompt-save-file-on-load 'always-save
        cider-font-lock-dynamically '(macro core function var)
        cider-overlays-use-font-lock t)
  (cider-repl-toggle-pretty-printing))
(use-package clj-refactor
  :diminish clj-refactor-mode
  :config (cljr-add-keybindings-with-prefix "C-c j"))
(use-package company
  :config
  (global-company-mode)
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 2
        company-selection-wrap-around t)

  (bind-keys :map company-mode-map
             ("C-i" . company-complete))
  (bind-keys :map company-active-map
             ("C-n" . company-select-next)
             ("C-p" . company-select-previous)
             ("C-s" . company-search-words-regexp))
  (bind-keys :map company-search-map
             ("C-n" . company-select-next)
             ("C-p" . company-select-previous)))

;; treemacsの設定
(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                2000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-header-scroll-indicators        '(nil . "^^^^^^")
          treemacs-hide-dot-git-directory          t
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-files-by-mouse-dragging    t
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'right
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-project-follow-into-home        nil
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               200
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       nil
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
  :after (treemacs persp-mode) ;;or perspective vs. persp-mode
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))

(use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
  :after (treemacs)
  :ensure t
  :config (treemacs-set-scope-type 'Tabs))

(use-package treemacs-nerd-icons
  :config
  (treemacs-load-theme "nerd-icons"))

;;(treemacs-start-on-boot)


;; ellamaの設定
(with-eval-after-load 'llm
  (require 'llm-ollama)
  ;; ellama-translateで翻訳する言語
  (setopt ellama-language "Japanese")
  ;; ellama-ask-selection などで生成されるファイルのネーミングルール
  (setopt ellama-naming-scheme 'ellama-generate-name-by-llm)
  ;; デフォルトのプロバイダー
  (setopt ellama-provider (make-llm-ollama
                           :chat-model "codestral:22b-v0.1-q4_K_S"
                           :embedding-model "codestral:22b-v0.1-q4_K_S"))
  ;; 翻訳で利用するプロバイダー
  (setopt ellama-translation-provider (make-llm-ollama
                                       :chat-model "aya:35b-23-q4_K_S"
                                       :embedding-model "aya:35b-23-q4_K_S"))
  ;; ellamaで使えるプロバイダー。ellama-provider-select で選択できる
  (setopt ellama-providers
          '(("codestral" . (make-llm-ollama
                            :chat-model "codestral:22b-v0.1-q4_K_S"
                            :embedding-model "codestral:22b-v0.1-q4_K_S"))
            ("gemma2" . (make-llm-ollama
                            :chat-model "gemma2:27b-instruct-q4_K_S"
                            :embedding-model "gemma2:27b-instruct-q4_K_S"))
            ("command-r" . (make-llm-ollama
                            :chat-model "command-r:35b"
                            :embedding-model "command-r:35b"))
            ("llama3.1" . (make-llm-ollama
                                  :chat-model "llama3.1:8b"
                                  :embedding-model "llama3.1:8b"))
            )))
(setq ellama-define-word-prompt-template "%s の定義を日本語で教えて")
(setq ellama-summarize-prompt-template "Text:\n%s\n日本語で要約して")
(setq ellama-code-review-prompt-template "以下のコードのレビューと改善案を日本語でだして:\n```\n%s\n```")
(setq ellama-change-prompt-template "以下のテキストを「%s」と変更して、引用符なしで日本語で出力して:\n%s")
(setq ellama-improve-grammar-prompt-template "誤字脱字・文法を日本語で校正して")
(setq ellama-improve-wording-prompt-template "語句を日本語で推敲して")
(setq ellama-improve-conciseness-prompt-template "日本語で、できるだけ簡潔にして")
(setq ellama-code-edit-prompt-template "以下のコードを「%s」と変更して、プロンプト無しでコードだけを\n```language\n...\n```\nの形式で出力して:\n```\n%s\n```\n")
(setq ellama-code-improve-prompt-template "以下のコードを改善して、プロンプト無しでコードだけを\n```language\n...\n```の形式で出力して:\n```\n%s\n```\n")
(setq ellama-code-complete-prompt-template "以下のコードの続きを書いて、プロンプト無しでコードだけを\n```language\n...\n```の形式で出力して:\n```\n%s\n```\n")
(setq ellama-code-add-prompt-template "Context: \n```\n%s\n```\nこのコードを文脈として、%s、プロンプト無しでコードだけを\n```\n...\n```\nの形式で出力して\n")
(setq ellama-generate-commit-message-template "あなたは熟練プログラマーです。後の変更点をもとに簡潔なコミットメッセージを書いてください。コミットメッセージの形式は、1行目は変更点の要約、2行目は空行、それ以降の行は変更全体の詳細な説明、です。出力はプロンプト無しで最終的なコミットメッセージだけにしてください。\n\n変更点:\n%s\n")
(setq ellama-make-format-prompt-template "以下のテキストを%sの形式に変換して:\n%s")
(setq ellama-make-list-prompt-template "Markdownのリスト形式にして")
(setq ellama-make-table-prompt-template "Markdownのテーブル形式にして")

(require 'mermaid-mode)
(add-hook 'markdown-mode-hook 'mermaid-mode)
(setq mermaid-chrome-executable "/Applications/Google\ Chrome.app/Contents/MacOS/Google\ Chrome")
(setenv "PUPPETEER_EXECUTABLE_PATH" "/Applications/Chromium.app/Contents/MacOS/Chromium")

(require 'auto-complete-config)
(ac-config-default)
(add-to-list 'ac-sources 'ac-source-xref)
