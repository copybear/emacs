
;;; init.el --- My init.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Naoya Yamashita

;; Author: Naoya Yamashita <conao3@gmail.com>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; My init.el.

;;; Code:

;; this enables this running method
;;   emacs -q -l ~/.debug.emacs.d/{{pkg}}/init.el


;; Leaf以外の設定を上に持ってきた
(setenv "LANG" "ja_JP.UTF-8")	 	;; multi-termの日本語表示対応
;; フッタ部の表示設定
(column-number-mode t)			;; カラム番号も表示
(size-indication-mode t)		;; ファイルサイズを表示
(setq display-time-day-and-date t)	;; 時計を表示（好みに応じてフォーマットを変更可能）
(setq display-time-24hr-format t)
(display-time-mode t)
(display-battery-mode t)		;; バッテリー残量を表示
(setq frame-title-format "%f")		;; タイトルバーにファイルのフルパスを表示

;; Leaf関連の設定
(eval-and-compile
  (when (or load-file-name byte-compile-current-file)
    (setq user-emacs-directory
          (expand-file-name
           (file-name-directory (or load-file-name byte-compile-current-file))))))

(eval-and-compile
  (customize-set-variable
   'package-archives '(("org"   . "https://orgmode.org/elpa/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("gnu"   . "https://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :init
    ;; optional packages if you want to use :hydra, :el-get, :blackout,,,
    (leaf hydra :ensure t)
    (leaf el-get :ensure t)
    (leaf blackout :ensure t)

    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init)))

;; ここにいっぱい設定を書く
;; leaf-convertについて書いてあったのでこれが最初じゃないとダメな気がした
(leaf leaf
  :config
  (leaf leaf-convert :ensure t)
  (leaf leaf-tree
    :ensure t
    :custom ((imenu-list-size . 30)
             (imenu-list-position . 'left))))

;; フォントの設定（デフォルトだとちょっと小さすぎた）
(leaf leaf-convert
  :config
  (add-to-list 'default-frame-alist
	       '(font . "menlo-20"))
  (set-fontset-font "fontset-default" 'unicode "Hiragino Kaku Gothic ProN"))

;; 雑多なキーバインドの設定
(leaf leaf-convert
  :bind (("C-c |" . toggle-truncate-lines)			;; 折り返しモードのキーバインド
	 ("C-t" . other-window)					;; C-c oの短縮（別ウィンドへ移動）
         ))
;; Web Mode設定
(leaf leaf-convert
  :preface
  (defun web-mode-hook nil
    "Hooks for Web mode."
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-comment-style 2)
    (setq web-mode-style-padding 1)
    (setq web-mode-script-padding 1))

  :when (require 'web-mode nil t)
  :mode (("\\.html\\'" . web-mode)
	 ("\\.css\\'" . web-mode)
	 ("\\.js\\'" . web-mode)
	 ("\\.jsx\\'" . web-mode)
	 ("\\.tpl\\.php\\'" . web-mode)
	 ("\\.ctp\\'" . web-mode)
	 ("\\.jsp\\'" . web-mode)
	 ("\\.as[cp]x\\'" . web-mode)
	 ("\\.erb\\'" . web-mode))
  :hook ((web-mode-hook . web-mode-hook)))
;; macのパスとJAVA環境変数の設定
(leaf leaf-convert
  :when (memq window-system
	      '(mac ns x))
  :config
  (exec-path-from-shell-initialize)
  (setenv "CLASSPATH" "/Library/Java/Extensions/mysql-connector-j-9.0.0.jar:/Library/Java/Extensions/postgresql-42.7.4.jar"))
;; doomの設定（見た目）
(leaf leaf-convert
  :require doom-modeline
  :setq ((doom-modeline-support-imenu . t)
	 (doom-modeline-height . 25)
	 (doom-modeline-bar-width . 4)
	 (doom-modeline-window-width-limit . 85)
	 (doom-modeline-project-detection quote auto)
	 (doom-modeline-buffer-file-name-style quote auto)
	 (doom-modeline-icon . t)
	 (doom-modeline-major-mode-icon . t)
	 (doom-modeline-major-mode-color-icon . t)
	 (doom-modeline-buffer-state-icon . t)
	 (doom-modeline-buffer-modification-icon . t)
	 (doom-modeline-lsp-icon . t)
	 (doom-modeline-time-icon . t)
	 (doom-modeline-time-icon)
	 (doom-modeline-time-analogue-clock)
	 (doom-modeline-unicode-fallback)
	 (doom-modeline-buffer-name . t)
	 (doom-modeline-highlight-modified-buffer-name . t)
	 (doom-modeline-column-zero-based . t)
	 (doom-modeline-percent-position quote
					 (-3 "%p"))
	 (doom-modeline-position-line-format quote
					     ("L%l"))
	 (doom-modeline-position-column-format quote
					       ("C%c"))
	 (doom-modeline-position-column-line-format quote
						    ("%l:%c"))
	 (doom-modeline-minor-modes)
	 (doom-modeline-enable-word-count)
	 (doom-modeline-continuous-word-count-modes quote
						    (markdown-mode gfm-mode org-mode))
	 (doom-modeline-buffer-encoding . t)
	 (doom-modeline-indent-info . t)
	 (doom-modeline-total-line-number . t)
	 (doom-modeline-vcs-icon . t)
	 (doom-modeline-vcs-max-length . 15)
	 (doom-modeline-vcs-display-function function doom-modeline-vcs-name)
	 (doom-modeline-check-icon . t)
	 (doom-modeline-check-simple-format)
	 (doom-modeline-number-limit . 99)
	 (doom-modeline-workspace-name . t)
	 (doom-modeline-persp-name . t)
	 (doom-modeline-display-default-persp-name)
	 (doom-modeline-persp-icon . t)
	 (doom-modeline-lsp . t)
	 (doom-modeline-github . t)
	 (doom-modeline-modal . t)
	 (doom-modeline-modal-icon . t)
	 (doom-modeline-modal-modern-icon . t)
	 (doom-modeline-gnus . t)
	 (doom-modeline-gnus-timer . 2)
	 (doom-modeline-gnus-excluded-groups quote
					     ("dummy.group"))
	 (doom-modeline-display-misc-in-all-mode-lines . t)
	 (doom-modeline-buffer-file-name-function function identity)
	 (doom-modeline-buffer-file-truename-function function identity)
	 (doom-modeline-env-version . t)
	 (doom-modeline-env-enable-python . t)
	 (doom-modeline-env-enable-ruby . t)
	 (doom-modeline-env-enable-perl . t)
	 (doom-modeline-env-enable-go . t)
	 (doom-modeline-env-enable-elixir . t)
	 (doom-modeline-env-enable-rust . t)
	 (doom-modeline-env-python-executable . "python")
	 (doom-modeline-env-ruby-executable . "ruby")
	 (doom-modeline-env-perl-executable . "perl")
	 (doom-modeline-env-go-executable . "go")
	 (doom-modeline-env-elixir-executable . "iex")
	 (doom-modeline-env-rust-executable . "rustc")
	 (doom-modeline-env-load-string . "...")
	 (doom-modeline-before-update-env-hook)
	 (doom-modeline-after-update-env-hook))
  :config
  (doom-modeline-mode 1)
  (setq doom-modeline-github-interval (* 30 60)))

(leaf nerd-icons
  :doc "Emacs Nerd Font Icons Library."
  :req "emacs-24.3"
  :tag "lisp" "emacs>=24.3"
  :url "https://github.com/rainstormstudio/nerd-icons.el"
  :added "2024-12-01"
  :emacs>= 24.3
  :ensure t)

(leaf vertico
  :ensure t
  :bind (
         :vertico-map
         (("C-l" . vertico-directory-up))
         (("M-q" . vertico-quick-insert))
         (("C-q" . vertico-quick-exit))
         (("DEL" . vertico-directory-delete-char)))
  :init
  (vertico-mode +1)
  :require
  nerd-icons
  :setq(
        (+vertico-current-arrow . t)
        (read-extended-command-predicate function command-completion-default-include-p))
  :config
  (cl-defmethod vertico--format-candidate :around
    (cand prefix suffix index start &context
	  ((and +vertico-current-arrow
		(not (bound-and-true-p vertico-flat-mode)))
	   (eql t)))
    (setq cand (cl-call-next-method cand prefix suffix index start))
    (if (bound-and-true-p vertico-grid-mode)
	(if (= vertico--index index)
	    (concat
	     (nerd-icons-faicon "nf-fa-hand_o_right" :face 'nerd-icons-blue)
	     "	" cand)
	  (concat
	   #("_" 0 1
	     (display " "))
	   cand))
      (if (= vertico--index index)
	  (concat " "
		  (nerd-icons-faicon "nf-fa-hand_o_right" :face 'nerd-icons-blue)
		  "	" cand)
	(concat "	" cand)))))



;; コードの補完パッケージ corfuとdabbrevとorderlessは一緒
(leaf corfu
  :doc "COmpletion in Region FUnction"
  :req "emacs-28.1" "compat-30"
  :tag "text" "completion" "matching" "convenience" "abbrev" "emacs>=28.1"
  :url "https://github.com/minad/corfu"
  :added "2024-12-01"
  :emacs>= 28.1
  :ensure t
  :init
  (global-corfu-mode)
  :require t
  :bind ((corfu-map
	  ("TAB" . corfu-insert)
	  ("<tab>" . corfu-insert)
	  ("RET")
	  ("<return>")))
  :config
  (let ((custom--inhibit-theme-enable nil))
    (unless (memq 'use-package custom-known-themes)
      (deftheme use-package)
      (enable-theme 'use-package)
      (setq custom-enabled-themes (remq 'use-package custom-enabled-themes)))
    (custom-theme-set-variables 'use-package
				'(corfu-auto t nil nil "Customized with use-package corfu")
				'(corfu-auto-delay 0 nil nil "Customized with use-package corfu")
				'(corfu-auto-prefix 1 nil nil "Customized with use-package corfu")
				'(corfu-cycle t nil nil "Customized with use-package corfu")
				'(corfu-on-exact-match nil nil nil "Customized with use-package corfu")
				'(tab-always-indent 'complete nil nil "Customized with use-package corfu")))
  (global-corfu-mode 1)
  (with-eval-after-load 'corfu
    (defun my/corfu-remap-tab-command nil
      (global-set-key
       [remap c-indent-line-or-region]
       #'indent-for-tab-command))

    (add-hook 'java-mode-hook #'my/corfu-remap-tab-command)
    (defun corfu-enable-always-in-minibuffer nil
      "Enable Corfu in the minibuffer if Vertico/Mct are not active."
      (unless (or
	       (bound-and-true-p mct--active)
	       (bound-and-true-p vertico--input))
	(setq-local corfu-echo-delay nil corfu-popupinfo-delay nil)
	(corfu-mode 1)))

    (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1)
    (with-eval-after-load 'lsp-mode
      (setq lsp-completion-provider :none))))

(leaf dabbrev
  :bind (("M-/" . dabbrev-completion)
	 ("C-M-/" . dabbrev-expand))
  :config
  (with-eval-after-load 'dabbrev
    (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
    (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
    (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
    (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode)))

(leaf orderless
  :doc "Completion style for matching regexps in any order."
  :req "emacs-27.1" "compat-30"
  :tag "extensions" "emacs>=27.1"
  :url "https://github.com/oantolin/orderless"
  :added "2024-12-01"
  :emacs>= 27.1
  :ensure t
  :pre-setq ((completion-styles quote
				(orderless basic))
	     (completion-category-defaults)
	     (completion-category-overrides))
  :require t
  :config
  (with-eval-after-load 'migemo
    (defun orderless-migemo (component)
      (let ((pattern (downcase
		      (migemo-get-pattern component))))
	(condition-case nil
	    (progn
	      (string-match-p pattern "")
	      pattern)

	  (invalid-regexp nil))))

    (add-to-list 'orderless-matching-styles 'orderless-migemo))

  (with-eval-after-load 'corfu
    (add-hook 'corfu-mode-hook
	      (lambda nil
		(setq-local orderless-matching-styles
			    '(orderless-flex))))))

;; 最近使ったファイルを保存するツール
(leaf recentf
  :preface
  (defun recentf-save-list-inhibit-message:around (orig-func &rest args)
    (setq inhibit-message t)
    (apply orig-func args)
    (setq inhibit-message nil)
    'around)

  :require t
  :setq ((recentf-max-saved-items . 2000)
	 (recentf-exclude quote
			  (".recentf" "^/ssh:"))
	 (recentf-auto-cleanup quote never))
  :config
  (setq recentf-auto-save-timer (run-with-idle-timer 30 t 'recentf-save-list))
  (recentf-mode 1)
  (advice-add 'recentf-cleanup :around 'recentf-save-list-inhibit-message:around)
  (advice-add 'recentf-save-list :around 'recentf-save-list-inhibit-message:around))

;; フォルダをツリー表示するツール
(leaf treemacs
  :ensure t
  :bind (("M-0" . treemacs-select-window)
	 ("C-x t 1" . treemacs-delete-other-windows)
	 ("C-x t t" . treemacs)
	 ("C-x t d" . treemacs-select-directory)
	 ("C-x t B" . treemacs-bookmark)
	 ("C-x t C-t" . treemacs-find-file)
	 ("C-x t M-t" . treemacs-find-tag))
  :config
  (with-eval-after-load 'winum
    (define-key winum-keymap
		(kbd "M-0")
		#'treemacs-select-window))

  (with-eval-after-load 'treemacs
    (progn
      (setq treemacs-collapse-dirs (if treemacs-python-executable
				       3 0)
	    treemacs-deferred-git-apply-delay 0.5
	    treemacs-directory-name-transformer #'identity
	    treemacs-display-in-side-window t
	    treemacs-eldoc-display 'simple
	    treemacs-file-event-delay 2000
	    treemacs-file-extension-regex treemacs-last-period-regex-value
	    treemacs-file-follow-delay 0.2
	    treemacs-file-name-transformer #'identity
	    treemacs-follow-after-init t
	    treemacs-expand-after-init t
	    treemacs-find-workspace-method 'find-for-file-or-pick-first
	    treemacs-git-command-pipe ""
	    treemacs-goto-tag-strategy 'refetch-index
	    treemacs-header-scroll-indicators '(nil . "^^^^^^")
	    treemacs-hide-dot-git-directory t
	    treemacs-indentation 2
	    treemacs-indentation-string " "
	    treemacs-is-never-other-window nil
	    treemacs-max-git-entries 5000
	    treemacs-missing-project-action 'ask
	    treemacs-move-files-by-mouse-dragging t
	    treemacs-move-forward-on-expand nil
	    treemacs-no-png-images nil
	    treemacs-no-delete-other-windows t
	    treemacs-project-follow-cleanup nil
	    treemacs-persist-file (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
	    treemacs-position 'right
	    treemacs-read-string-input 'from-child-frame
	    treemacs-recenter-distance 0.1
	    treemacs-recenter-after-file-follow nil
	    treemacs-recenter-after-tag-follow nil
	    treemacs-recenter-after-project-jump 'always
	    treemacs-recenter-after-project-expand 'on-distance
	    treemacs-litter-directories '("/node_modules" "/.venv" "/.cask")
	    treemacs-project-follow-into-home nil
	    treemacs-show-cursor nil
	    treemacs-show-hidden-files t
	    treemacs-silent-filewatch nil
	    treemacs-silent-refresh nil
	    treemacs-sorting 'alphabetic-asc
	    treemacs-select-when-already-in-treemacs 'move-back
	    treemacs-space-between-root-nodes t
	    treemacs-tag-follow-cleanup t
	    treemacs-tag-follow-delay 1.5
	    treemacs-text-scale nil
	    treemacs-user-mode-line-format nil
	    treemacs-user-header-line-format nil
	    treemacs-wide-toggle-width 200
	    treemacs-width 35
	    treemacs-width-increment 1
	    treemacs-width-is-initially-locked nil
	    treemacs-workspace-switch-cleanup nil)
      (treemacs-follow-mode t)
      (treemacs-filewatch-mode t)
      (treemacs-fringe-indicator-mode 'always)
      (when treemacs-python-executable
	(treemacs-git-commit-diff-mode t))
      (pcase (cons
	      (not (null (executable-find "git")))
	      (not (null treemacs-python-executable)))
	(`(t . t)
	 (treemacs-git-mode 'deferred))
	(`(t . _)
	 (treemacs-git-mode 'simple)))
      (treemacs-hide-gitignored-files-mode nil))))
;; treemacs関連
(leaf leaf-convert
  :config
  (leaf treemacs-evil
    :ensure t
    :config
    (with-eval-after-load 'evil
      (eval-after-load 'treemacs
	'(require 'treemacs-evil nil nil))))

  (leaf treemacs-projectile
    :ensure t
    :config
    (with-eval-after-load 'projectile
      (eval-after-load 'treemacs
	'(require 'treemacs-projectile nil nil))))

  (leaf treemacs-icons-dired
    :ensure t
    :commands treemacs-icons-dired-enable-once
    :hook ((dired-mode-hook . treemacs-icons-dired-enable-once)))

  (leaf treemacs-magit
    :ensure t
    :config
    (with-eval-after-load 'magit
      (eval-after-load 'treemacs
	'(require 'treemacs-magit nil nil))))

  (leaf treemacs-persp
    :ensure t
    :config
    (with-eval-after-load 'persp-mode
      (eval-after-load 'treemacs
	'(progn
	   (require 'treemacs-persp nil nil)
	   (treemacs-set-scope-type 'Perspectives)
	   t))))

  (leaf treemacs-tab-bar
    :ensure t
    :config
    (with-eval-after-load 'treemacs
      (require 'treemacs-tab-bar nil nil)
      (treemacs-set-scope-type 'Tabs)))

  (leaf treemacs-nerd-icons
    :require t
    :config
    (treemacs-load-theme "nerd-icons")))

;; ミニバッファで保管する時に細く情報を追加してくれる
(leaf marginalia
  :ensure t
  :init
  (marginalia-mode)
  :require t)


;; ellama関連
(leaf llm
  :after t
  :require llm-ollama
  :setq ((ellama-define-word-prompt-template . "%s の定義を日本語で教えて")
	 (ellama-summarize-prompt-template . "Text:\n%s\n日本語で要約して")
	 (ellama-code-review-prompt-template . "以下のコードのレビューと改善案を日本語でだして:\n```\n%s\n```")
	 (ellama-change-prompt-template . "以下のテキストを「%s」と変更して、引用符なしで日本語で出力して:\n%s")
	 (ellama-improve-grammar-prompt-template . "誤字脱字・文法を日本語で校正して")
	 (ellama-improve-wording-prompt-template . "語句を日本語で推敲して")
	 (ellama-improve-conciseness-prompt-template . "日本語で、できるだけ簡潔にして")
	 (ellama-code-edit-prompt-template . "以下のコードを「%s」と変更して、プロンプト無しでコードだけを\n```language\n...\n```\nの形式で出力して:\n```\n%s\n```\n")
	 (ellama-code-improve-prompt-template . "以下のコードを改善して、プロンプト無しでコードだけを\n```language\n...\n```の形式で出力して:\n```\n%s\n```\n")
	 (ellama-code-complete-prompt-template . "以下のコードの続きを書いて、プロンプト無しでコードだけを\n```language\n...\n```の形式で出力して:\n```\n%s\n```\n")
	 (ellama-code-add-prompt-template . "Context: \n```\n%s\n```\nこのコードを文脈として、%s、プロンプト無しでコードだけを\n```\n...\n```\nの形式で出力して\n")
	 (ellama-generate-commit-message-template . "あなたは熟練プログラマーです。後の変更点をもとに簡潔なコミットメッセージを書いてください。コミットメッセージの形式は、1行目は変更点の要約、2行目は空行、それ以降の行は変更全体の詳細な説明、です。出力はプロンプト無しで最終的なコミットメッセージだけにしてください。\n\n変更点:\n%s\n")
	 (ellama-make-format-prompt-template . "以下のテキストを%sの形式に変換して:\n%s")
	 (ellama-make-list-prompt-template . "Markdownのリスト形式にして")
	 (ellama-make-table-prompt-template . "Markdownのテーブル形式にして"))
  :config
  (setopt ellama-language "Japanese")
  (setopt ellama-naming-scheme 'ellama-generate-name-by-llm)
  (setopt ellama-provider
	  (make-llm-ollama :chat-model "codestral:22b-v0.1-q4_K_S" :embedding-model "codestral:22b-v0.1-q4_K_S"))
  (setopt ellama-translation-provider
	  (make-llm-ollama :chat-model "aya:35b-23-q4_K_S" :embedding-model "aya:35b-23-q4_K_S"))
  (setopt ellama-providers
	  '(("codestral" make-llm-ollama :chat-model "codestral:22b-v0.1-q4_K_S" :embedding-model "codestral:22b-v0.1-q4_K_S")
	    ("gemma2" make-llm-ollama :chat-model "gemma2:27b-instruct-q4_K_S" :embedding-model "gemma2:27b-instruct-q4_K_S")
	    ("command-r" make-llm-ollama :chat-model "command-r:35b" :embedding-model "command-r:35b")
	    ("llama3.1" make-llm-ollama :chat-model "llama3.1:8b" :embedding-model "llama3.1:8b"))))


;; Wiki作成ツール
(leaf howm
  :doc "Wiki-like note-taking tool."
  :req "cl-lib-0.5"
  :url "https://github.com/kaorahi/howm"
  :added "2024-12-01"
  :ensure t
  :config
  (setq howm-directory (concat user-emacs-directory "howm"))
  (setq howm-menu-lang 'ja)
  :bind (("C-c ,," . howm-menu)))

(leaf macrostep
  :ensure t
  :bind (("C-c e" . macrostep-expand)))

(leaf flycheck
  :doc "On-the-fly syntax checking"
  :req "emacs-26.1"
  :tag "tools" "languages" "convenience" "emacs>=26.1"
  :url "https://github.com/flycheck/flycheck"
  :added "2024-11-30"
  :emacs>= 26.1
  :ensure t)

;; gitのパッケージ
(leaf magit
  :doc "A Git porcelain inside Emacs."
  :req "emacs-26.1" "compat-30.0.0.0" "dash-2.19.1" "magit-section-4.1.1" "seq-2.24" "transient-0.7.6" "with-editor-3.4.2"
  :tag "vc" "tools" "git" "emacs>=26.1"
  :url "https://github.com/magit/magit"
  :added "2024-11-30"
  :emacs>= 26.1
  :ensure t
  :after compat magit-section with-editor)

;; mu4eの設定
(use-package mu4e
  :load-path  "/usr/local/share/emacs/site-lisp/mu/mu4e/")

(leaf leaf-convert
  :require smtpmail
  :setq ((mu4e-maildir . "~/.maildir")
	 (mu4e-update-interval . 300)
	 (mu4e-attachment-dir . "~/Desktop")
	 (mu4e-change-filenames-when-moving . t)
	 (mu4e-user-mail-address-list quote
				      ("hiro5125jp@me.com" "info@sasanet.cyou")))
  :config
  (setq mu4e-mu-binary (executable-find "mu"))
  (setq mu4e-get-mail-command (concat
			       (executable-find "mbsync")
			       " -a")))
(leaf leaf-convert
  :config
  (add-to-list 'mu4e-bookmarks
	       '(:name "Inbox - iCloud" :query "maildir:/icloud/INBOX" :key 105))
  (add-to-list 'mu4e-bookmarks
	       '(:name "Inbox - Nextcloud" :query "maildir:/nextcloud/INBOX" :key 110)))

(leaf leaf-convert
  :setq ((mu4e-context-policy quote pick-first)
	 (mu4e-compose-context-policy quote ask))
  :config
  (setq mu4e-contexts `(,(make-mu4e-context :name "icloud" :enter-func
					    (lambda nil
					      (mu4e-message "Enter hiro5125jp.me.com context"))
					    :leave-func
					    (lambda nil
					      (mu4e-message "Leave hiro5125jp.me.com context"))
					    :match-func
					    (lambda (msg)
					      (when msg
						(mu4e-message-contact-field-matches msg :to "hiro5125jp@me.com")))
					    :vars
					    '((user-mail-address . "hiro5125jp@me.com")
					      (user-full-name . "Hirofumi Mori")
					      (mu4e-drafts-folder . "/icloud/Drafts")
					      (mu4e-refile-folder . "/icloud/Archive")
					      (mu4e-sent-folder . "/icloud/Sent Messages")
					      (mu4e-trash-folder . "/icloud/Deleted Messages")))
			,(make-mu4e-context :name "nextcloud" :enter-func
					    (lambda nil
					      (mu4e-message "Enter info@sasanet.cyou context"))
					    :leave-func
					    (lambda nil
					      (mu4e-message "Leave info@sasanet.cyou context"))
					    :match-func
					    (lambda (msg)
					      (when msg
						(mu4e-message-contact-field-matches msg :to "info@sasanet.cyou")))
					    :vars
					    '((user-mail-address . "info@sasanet.cyou")
					      (user-full-name . "Hirofumi Mori")
					      (mu4e-drafts-folder . "/nextcloud/Drafts")
					      (mu4e-refile-folder . "/nextcloud/Archive")
					      (mu4e-sent-folder . "/nextcloud/Sent")
					      (mu4e-trash-folder . "/nextcloud/Trash"))))))

(leaf leaf-convert
  :preface
  (defun timu/set-msmtp-account nil
    (if (message-mail-p)
	(save-excursion
	  (let* ((from (save-restriction
			 (message-narrow-to-headers)
			 (message-fetch-field "from")))
		 (account (cond
			   ((string-match "hiro5125jp@me.com" from)
			    "icloud")
			   ((string-match "info@sasanet.cyou" from)
			    "nextcloud"))))
	    (setq message-sendmail-extra-arguments (list '"-a" account))))))

  :hook ((message-send-mail-hook . timu/set-msmtp-account)
	 (mu4e-compose-mode-hook . company-mode))
  :require epa-file
  :setq ((epa-pinentry-mode quote loopback)
	 (message-kill-buffer-on-exit . t)
	 (send-mail-function quote sendmail-send-it)
	 (message-send-mail-function quote sendmail-send-it)
	 (message-sendmail-envelope-from quote header))
  :config
  (epa-file-enable)
  (auth-source-forget-all-cached)
  (setq sendmail-program (executable-find "msmtp"))
  (add-hook 'mu4e-compose-mode-hook
	    (defun timu/add-cc-and-bcc nil
	      "My Function to automatically add Cc & Bcc: headers.\n    This is in the mu4e compose mode."
	      (save-excursion
		(message-add-header "Cc:\n"))
	      (save-excursion
		(message-add-header "Bcc:\n")))))

(leaf leaf-convert
  :setq ((org-mu4e-link-query-in-headers-mode)
	 (mu4e-confirm-quit)
	 (mu4e-headers-visible-lines . 20)
	 (mu4e-headers-show-threads)
	 (mu4e-hide-index-messages . t)
	 (message-citation-line-format . "%N @ %Y-%m-%d %H:%M :\n")
	 (message-citation-line-function quote message-insert-formatted-citation-line)
	 (mu4e-headers-include-related)
	 (mu4e-split-view quote single-window)))

;; elfeedの設定
(leaf elfeed-org
  :ensure t
  :require t
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files (list "~/.emacs.d/elfeed/elfeed.org")))

(leaf elfeed
  :when (version<= "24.3" emacs-version)
  :config
  (leaf-handler-package elfeed elfeed nil)
  :preface
  (defun bjm/elfeed-show-all nil
    (interactive)
    (bookmark-maybe-load-default-file)
    (bookmark-jump "elfeed-all"))

  (defun bjm/elfeed-show-emacs nil
    (interactive)
    (bookmark-maybe-load-default-file)
    (bookmark-jump "elfeed-emacs"))

  (defun bjm/elfeed-show-daily nil
    (interactive)
    (bookmark-maybe-load-default-file)
    (bookmark-jump "elfeed-daily"))

  (defun bjm/elfeed-load-db-and-open nil
    "Wrapper to load the elfeed db from disk before opening"
    (interactive)
    (elfeed-db-load)
    (elfeed)
    (elfeed-search-update--force))

  (defun bjm/elfeed-save-db-and-bury nil
    "Wrapper to save the elfeed db to disk before burying buffer"
    (interactive)
    (elfeed-db-save)
    (quit-window))

  :config
  (leaf elfeed
    :ensure t
    :bind ((elfeed-search-mode-map
	    ("A" . bjm/elfeed-show-all)
	    ("E" . bjm/elfeed-show-emacs)
	    ("D" . bjm/elfeed-show-daily)
	    ("q" . bjm/elfeed-save-db-and-bury))))
  )

(leaf undo-tree
  :doc "Treat undo history as a tree"
  :req "queue-0.2"
  :tag "tree" "history" "redo" "undo" "files" "convenience"
  :url "https://www.dr-qubit.org/undo-tree.html"
  :added "2024-11-30"
  :ensure t
  :require t
  :bind (("M-/" . undo-tree-redo))
  :config
  (global-undo-tree-mode))

(leaf elscreen
  :doc "Emacs window session manager."
  :req "emacs-24"
  :tag "convenience" "window" "emacs>=24"
  :url "https://github.com/knu/elscreen"
  :added "2024-11-30"
  :emacs>= 24
  :ensure t
    :when (require 'elscreen nil t)
  :config
  (elscreen-start)
  (if window-system
      (define-key elscreen-map
		  (kbd "C-z")
		  'iconify-or-deiconify-frame)4
    (define-key elscreen-map
		(kbd "C-z")
		'suspend-emacs))) 

(provide 'init)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; init.el ends here

