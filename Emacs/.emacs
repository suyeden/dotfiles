;;; 環境を日本語,UTF-8にする
(set-locale-environment nil)
(set-language-environment "Japanese")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)

;;; スタートアップメッセージを表示させない
(setq inhibit-startup-message t)

;;; バックアップファイルを作成させない
(setq make-backup-files nil)

;;; 終了時にオートセーブファイルを削除する
(setq delete-auto-save-files t)

;;; タブにスペースを使用しない
(setq-default tab-width 3 indent-tabs-mode nil)

;;; 改行コードを表示する
(setq eol-mnemonic-dos "(CRLF)")
(setq eol-mnemonic-mac "(CR)")
(setq eol-mnemonic-unix "(LF)")

;;; メニューバーを消す
(menu-bar-mode -1)

;;; ツールバーを消す
(tool-bar-mode -1)

;;; カーソルの点滅をやめる
(blink-cursor-mode 0)

;;; 対応する括弧を光らせる
(show-paren-mode 1)

;;; 列数を表示する
(column-number-mode t)

;;; スクロールは1行ごとに
(setq scroll-conservatively 35
      scroll-margin 0
      scroll-step 1)
;;(setq scroll-conservatively 1)

;;; "yes or no" の選択を "y or n" にする
(fset 'yes-or-no-p 'y-or-n-p)

;;; beep音を消す
(defun my-bell-function()
  (unless (memq this-command
                '(isearch-abort abort-recursive-edit exit-minibuffer
                                keyboard-quit mwheel-scroll down up next-line previous-line backward-char forward-char))
    (ding)))
(setq ring-bell-function 'my-bell-function)

;;; 括弧の自動補完
(electric-pair-mode 1)


;;;;
;;;; package, mode, file 関連
;;;;


;;; カスタムファイルの指定
(setq custom-file "~/.emacs.d/custom.el")
(if (file-exists-p (expand-file-name custom-file))
    (load-file (expand-file-name custom-file)))

;;; package.el の有効化, パッケージ読み込み
;;
(require 'package)
;;
;; HTTPS系のリポジトリ
;; (add-to-list 'package-archives '("melpa"."https://melpa.milkbox.net/packages/")t)
;; (add-to-list 'package-archives '("melpa-stable"."https://stable.melpa.org/packages/")t)
;; (add-to-list 'package-archives '("marmalade"."https://marmalade-repo.org/packages/")t)
;;
;; HTTP系のリポジトリ
(add-to-list 'package-archives '("melpa"."http://melpa.milkbox.net/packages/")t)
(add-to-list 'package-archives '("melpa-stable"."http://stable.melpa.org/packages/")t)
(add-to-list 'package-archives '("org"."http://orgmode.org/elpa/")t)
(add-to-list 'package-archives '("ELPA"."http://tromey.com/elpa/")t)
;;
;; marmaladeはHTTPアクセスすると証明書エラーでフリーズするので注意
;; (add-to-list 'package-archives '("marmalade"."http://marmalade-repo.org/packages/")t)
;;
;; パッケージのインストール先を load-path へ自動追加
;; (package-install でインストールしたパッケージは require や autoload が不要になる)
(package-initialize)

;;; カラーテーマの変更
(load-theme 'ir-black t)

;;; dired 設定
(require 'dired-x)

;;; auto-complete
;;
;; auto-complete-config の設定ファイルを読み込む
(require 'auto-complete-config)
;;
(ac-config-default)
;; TABキーで自動補完を有効にする
(ac-set-trigger-key "TAB")
;; auto-complete-mode を起動時に有効にする
(global-auto-complete-mode t)

;;; undo-tree
;;
(require 'undo-tree)
;; undo-tree を起動時に有効にする
(global-undo-tree-mode t)
;; M-/ を redo に設定する
(global-set-key (kbd "M-/") 'undo-tree-redo)

;;; dirtree
(require 'dirtree)

;;; SBCL をデフォルトの Common Lisp 処理系に設定
(setq inferior-lisp-program "sbcl")
;; ~/slime を load-path に追加
(add-to-list `load-path (expand-file-name "~/slime"))
;; SLIME のロード
(require 'slime)
(slime-setup '(slime-repl slime-fancy slime-banner))

;; ;;; Schemeモードの設定
;; ;; gaucheに渡す文字コードをUTF-8に設定
;; (setq process-coding-system-alist
;;       (cons '("gosh" utf-8 . utf-8) process-coding-system-alist))
;; ;; Emacsのバッファ内で動かすScheme処理系をgaucheに設定
;; (setq scheme-program-name "gosh -i")
;; ;; Schemeモードをより便利なcmuscheme.elで使用する設定
;; (autoload 'scheme-mode "cmuscheme" "Major mode for Scheme." t)
;; (autoload 'run-scheme "cmuscheme" "Run an inferior Scheme process." t)
;; ;; ウィンドウを2つに分け、一方でgaucheインタプリタを実行する関数（コマンド）を定義し、C-c S で起動できるように設定
;; (defun scheme-other-window ()
;;   "Run scheme on other window"
;;   (interactive)
;;   (switch-to-buffer-other-window
;;    (get-buffer-create "*scheme*"))
;;   (run-scheme scheme-program-name))
;; (define-key global-map
;;   "\C-cS" 'scheme-other-window)

;;; org-mode の設定
;;
;; ファイルの場所 (org-default-notes-file の場所)
(setq org-directory "~/org/")
;; メモの挿入先 (org-default-notes-file のファイル名)
(setq org-default-notes-file "notes.org")
;; .org ファイルは自動的に org-mode
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
;; 見出しの余分な * を消す
(setq org-hide-leading-stars t)
;; 画像をインラインで表示
(setq org-startup-with-inline-images t)
;; LOGBOOK drawer に時間を格納する
(setq org-clock-into-drawer t)
;; org-capture のテンプレート(メニュー)の設定
(setq org-capture-templates
      '(("m"
         "Memo"
         entry
         (file+datetree "~/org/notes.org")
         "* %U\n %?\n"
         :empty-lines 1)
        ("t"
         "Task"
         entry
         (file+datetree "~/org/notes.org")
         "* TODO %? %T\n"
         :empty-lines 1)
        ("c"
         "Check"
         plain
         (file+datetree "~/org/notes.org")
         "%?"
         :jump-to-captured 1
         :unnarrowed 1)))
;; org-directory 内のファイルすべてから agenda を作成する
(setq my-org-agenda-dir "~/org/")
(setq org-agenda-files (list my-org-agenda-dir))
;; TODO の状態
(setq org-todo-keywords
      '((sequence "TODO(t)" "|" "DONE(d)")))
;; DONE の時刻を記録
(setq org-log-done 'time)
;; 行の折り返し
(setq org-startup-truncated nil)
;; org-refile の設定
(setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
;; キーバインド
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-cc" 'org-capture)
(define-key global-map "\C-ca" 'org-agenda)

;;; python-mode における module のリロード問題の解決
;;
;; Run python and pop-up its shell
;; Kill process to solve the reload module problem
(defun my-python-shell-run ()
  (interactive)
  (when (get-buffer-process "*Python*")
    (set-process-query-on-exit-flag (get-buffer-process "*Python*") nil)
    (kill-process (get-buffer-process "*Python*"))
    ;; If you want to clean the buffer too
    ;; (kill-buffer "*Python*")
    ;; Not so fast!
    (sleep-for 0.5))
  (run-python (python-shell-parse-command) nil nil)
  (python-shell-send-buffer)
  ;; Pop new window only if shell isn't visible
  ;; in any frame
  (unless (get-buffer-window "*Python*" t)
    (python-shell-switch-to-shell)))
;;
(defun my-python-shell-run-region ()
  (interactive)
  (python-shell-send-region (region-beginning) (region-end))
  (python-shell-switch-to-shell))
;;
(eval-after-load "python"
  '(progn
     (define-key python-mode-map (kbd "C-c C-c") 'my-python-shell-run)
     (define-key python-mode-map (kbd "C-c C-r") 'my-python-shell-run-region)
     (define-key python-mode-map (kbd "C-h f") 'python-eldoc-at-point)))

;;; web-mode
;;
(require 'web-mode)
;;
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[gj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ctp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(setq web-mode-engines-alist
      '(("php" . "\\.phtml\\'")
        ("blade" . "\\.blade\\.")))
;;
(defun web-mode-hook ()
  "Hooks for Web mode"
  ;; indent
  (setq web-mode-html-offset 2)
  (setq web-mode-style-padding 2)
  (setq web-mode-css-offset 2)
  (setq web-mode-script-offset 2)
  (setq web-mode-java-offset 2)
  (setq web-mode-asp-offset 2)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  ;; auto tag closing
  (setq web-mode-enable-auto-paring t)
  (setq web-mode-enable-auto-closing t))
;;
(add-hook 'web-mode-hook 'web-mode-hook)

;;; js2-mode
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;;; 自作メジャーモード (自作 Emacs-Lisp ファイル) のロード
(add-to-list 'load-path "~/.emacs.d/lisp")
;;
;; blog-mode の読み込み
(load-library "blog-mode.el")


;;;;
;;;; key-bindings
;;;;


;;; C-k で行のカーソル以降を削除する
(defun forward-delete-line ()
  "Delete chars forward until encountering the end of a line."
  (interactive)
  (let ((p (point)))
    (if (= p (progn (end-of-line) (point)))
        (delete-forward-char 1)
      (delete-region p (progn (end-of-line) (point))))))
;;
(define-key global-map "\C-k" 'forward-delete-line)

;;; C-c k で行のカーソル以降をkillする
(defun forward-kill-line ()
  "Kill chars forward until encountering the end of a line."
  (interactive)
  (kill-region
   (point)
   (progn
     (end-of-line)
     (point))))
;;
(define-key global-map "\C-ck" 'forward-kill-line)

;;; ウィンドウ間の移動のキーバインド変更
(global-set-key "\C-t" 'other-window)

;;; C-c m で compile コマンドを呼び出す
(define-key mode-specific-map "m" 'compile)

;;; C-c d でカーソル位置から行頭まで削除する
;; カーソル位置から行頭まで削除
(defun backward-delete-line ()
  "Delete chars backward until encountering the beginning of a line."
  (interactive)
  (let ((p (point)))
    (delete-region
     (progn
       (beginning-of-line)
       (point))
     p)))
;; C-c d に設定
(define-key global-map (kbd "C-c d") 'backward-delete-line)

;;; C-n で半ページ先に飛ぶ
(define-key global-map "\C-n" 'my-next-line)
;;
(defun my-next-line ()
  (interactive)
  (next-line 19))

;;; C-p で次の括弧に飛ぶ
(define-key global-map "\C-p" 'my-move-forward-paren)
;;
(defun my-move-forward-paren ()
  (interactive)
  (re-search-forward "[()]" nil t)
  (goto-char (match-end 0)))

;;; C-c-p で前の括弧に飛ぶ
(define-key global-map "\C-c\C-p" 'my-move-backward-paren)
;;
(defun my-move-backward-paren ()
  (interactive)
  (re-search-backward "[()]" nil t)
  (goto-char (match-beginning 0)))

;;; M-n でカーソルを固定したまま画面を次ページにスクロール
(define-key global-map "\M-n" 'my-move-forward)
;;
(defun my-move-forward ()
  (interactive)
  (scroll-up 1))

;;; M-p でカーソルを固定したまま画面を前ページにスクロール
(define-key global-map "\M-p" 'my-move-backward)
;;
(defun my-move-backward ()
  (interactive)
  (scroll-down 1))

;;; M-a で行の真ん中に飛ぶ
(define-key global-map "\M-a" 'my-move-char)
;;
(defun my-move-char ()
  (interactive)
  (let (my-point)
    (setq my-point (progn (end-of-line) (current-column)))
    (move-to-column (/ my-point 2))))
