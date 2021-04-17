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
(setq custom-file "~/.emacs.d/custom.elc")
(if (file-exists-p (expand-file-name custom-file))
    (load-file (expand-file-name custom-file)))

;;; package.el の有効化, パッケージ読み込み
;;
(require 'package)
;;
;; HTTPS系のリポジトリ
;; (add-to-list 'package-archives '("melpa"."https://melpa.org/packages/")t)
;; (add-to-list 'package-archives '("melpa-stable"."https://stable.melpa.org/packages/")t)
;; (add-to-list 'package-archives '("marmalade"."https://marmalade-repo.org/packages/")t)
;;
;; HTTP系のリポジトリ
(add-to-list 'package-archives '("melpa"."http://melpa.org/packages/")t)
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

;; ;;; SBCL をデフォルトの Common Lisp 処理系に設定
;; (setq inferior-lisp-program "sbcl")
;; ;; ~/slime を load-path に追加
;; (add-to-list `load-path (expand-file-name "~/slime"))
;; ;; SLIME のロード
;; (require 'slime)
;; (slime-setup '(slime-repl slime-fancy slime-banner))

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

;; ;;; python-mode における module のリロード問題の解決
;; ;;
;; ;; Run python and pop-up its shell
;; ;; Kill process to solve the reload module problem
;; (defun my-python-shell-run ()
;;   (interactive)
;;   (when (get-buffer-process "*Python*")
;;     (set-process-query-on-exit-flag (get-buffer-process "*Python*") nil)
;;     (kill-process (get-buffer-process "*Python*"))
;;     ;; If you want to clean the buffer too
;;     ;; (kill-buffer "*Python*")
;;     ;; Not so fast!
;;     (sleep-for 0.5))
;;   (run-python (python-shell-parse-command) nil nil)
;;   (python-shell-send-buffer)
;;   ;; Pop new window only if shell isn't visible
;;   ;; in any frame
;;   (unless (get-buffer-window "*Python*" t)
;;     (python-shell-switch-to-shell)))
;; ;;
;; (defun my-python-shell-run-region ()
;;   (interactive)
;;   (python-shell-send-region (region-beginning) (region-end))
;;   (python-shell-switch-to-shell))
;; ;;
;; (eval-after-load "python"
;;   '(progn
;;      (define-key python-mode-map (kbd "C-c C-c") 'my-python-shell-run)
;;      (define-key python-mode-map (kbd "C-c C-r") 'my-python-shell-run-region)
;;      (define-key python-mode-map (kbd "C-h f") 'python-eldoc-at-point)))

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
(defvar web-mode-html-offset)
(defvar web-mode-css-offset)
(defvar web-mode-script-offset)
(defvar web-mode-java-offset)
(defvar web-mode-asp-offset)
(defvar web-mode-enable-auto-paring)
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

;;; magit
(defalias 'magit 'magit-status)
(global-set-key "\C-xg" 'magit-status)

;;; grip-mode
;; Python を導入済み、および pip install grip を実行済みであることを確認
;; C-c C-g で grip-mode を起動する
(define-key global-map "\C-c\C-g" 'grip-mode)

;;; 自作メジャーモード (自作 Emacs-Lisp ファイル) のロード
(add-to-list 'load-path "~/.emacs.d/lisp")
;;
;; blog-mode の読み込み
(load-library "blog-mode.elc")


;;;;
;;;; key-bindings
;;;;


;;; C-k で行のカーソル以降を削除する
(define-key global-map "\C-k" 'my-Emacs-del-back-line)
;;
(defun my-Emacs-del-back-line ()
  (interactive)
  (let ((p (point)))
    (if (= p (progn (end-of-line) (point)))
        (delete-char 1)
      (delete-region p (progn (end-of-line) (point))))))

;;; C-c k で行のカーソル以降をkillする
(define-key global-map "\C-ck" 'my-Emacs-kill-line)
;;
(defun my-Emacs-kill-line ()
  (interactive)
  (kill-region
   (point)
   (progn
     (end-of-line)
     (point))))

;;; ウィンドウ間の移動のキーバインド変更
(global-set-key "\C-t" 'other-window)

;;; C-c m で compile コマンドを呼び出す
(define-key mode-specific-map "m" 'compile)

;;; C-c d でカーソル位置から行頭まで削除する
(define-key global-map "\C-cd" 'my-Emacs-del-for-line)
;;
(defun my-Emacs-del-for-line ()
  (interactive)
  (let ((p (point)))
    (delete-region
     (progn
       (beginning-of-line)
       (point))
     p)))

;; ;;; C-q でカーソル位置から行末までの中央に飛ぶ
;; (define-key global-map "\C-q"
;;   '(lambda ()
;;      (interactive)
;;      (let (my-end-point)
;;        (save-excursion
;;          (setq my-end-point (progn (end-of-line) (current-column))))
;;        (if (not (= 0 (/ (- my-end-point (current-column)) 2)))
;;            (move-to-column (+ (current-column) (/ (- my-end-point (current-column)) 2)))
;;          (forward-line 1)))))

;;; M-N で次の括弧に飛ぶ
(define-key global-map "\M-N"
  '(lambda ()
    (interactive)
    (re-search-forward "[()]" nil t)))

;;; M-P で前の括弧に飛ぶ
(define-key global-map "\M-P"
  '(lambda ()
     (interactive)
     (re-search-backward "[()]" nil t)))

;;; M-n でカーソルを固定したまま画面を次ページにスクロール
(define-key global-map "\M-n"
  '(lambda ()
     (interactive)
     (scroll-up 1)))

;;; M-p でカーソルを固定したまま画面を前ページにスクロール
(define-key global-map "\M-p"
  '(lambda ()
     (interactive)
     (scroll-down 1)))

;;; C-q でパスを補完する
(define-key global-map "\C-q" 'my-Emacs-completing-file-path)
;;
(defun my-Emacs-completing-file-path ()
  (interactive)
  (let (path-intelli-dir path-intelli-alist path-intelli-insert-file)
    (catch 'foo 
      (save-excursion
        (if (re-search-forward "\"" nil t)
            nil
          (message "Place the cursor at the correct position.")
          (throw 'foo t))
        (if (re-search-backward "\"\\(.+\\)\"" nil t)
            (progn
              (setq path-intelli-dir (buffer-substring (match-beginning 1) (match-end 1)))
              (setq path-intelli-alist (directory-files (format "%s" path-intelli-dir)))
              (let (path-intelli-alist x)
                (while path-intelli-alist
                  (if (or (string= "." (format "%s" (car path-intelli-alist))) (string= ".." (format "%s" (car path-intelli-alist))))
                      nil
                    (setq x (append x (car path-intelli-alist))))
                  (setq x (cdr x)))
                (setq path-intelli-alist x))
              (setq path-intelli-insert-file (completing-read "Which file ? : " path-intelli-alist)))
          (message "There is no File-Path.")
          (throw 'foo t)))
      (insert (format "%s" path-intelli-insert-file)))))

;;; Org ファイルを開いている時 C-x x で諸機能を提供
(define-key global-map "\C-xx" 'my-Emacs-org-option)
;;
(defun my-Emacs-org-option ()
  ;; x : Markdown に書き出して整形
  ;; t : Org ファイルのひな形を作成
  (interactive)
  (let (my-org-option-mode)
    (if (string-match ".+\\.org" (format "%s" (buffer-name)))
        (progn
          (setq my-org-option-mode (read-string "[x] Export to Markdown\n[t] Make a Template for Org-file\ncommand ? : "))
          (while (not (or (string= "x" my-org-option-mode) (string= "t" my-org-option-mode)))
            (setq my-org-option-mode (read-string "[x] Export to Markdown\n[t] Make a Template for Org-file\ncommand ? : ")))
          (if (string= "x" my-org-option-mode)
              (my-org-to-md)
            (if (string= "t" my-org-option-mode)
                (my-insert-org-template)
              nil)))
      (message "Not Org-file !"))))
;;
;; バイトコンパイル時の org-md-export-to-markdown に対する警告メッセージ対策
(declare-function org-md-export-to-markdown (locate-library "ox-md.el"))
;;
(defun my-org-to-md ()
  "Org ファイルを Markdown ファイルに書き出す"
  (let (my-file-name my-lang-name my-src-begin my-src-end)
    (save-excursion
      (goto-char (point-min))
      (org-md-export-to-markdown)
      (setq my-file-name (format "%s" (buffer-name)))
      (with-temp-buffer
        (insert my-file-name)
        (goto-char (point-min))
        (re-search-forward "\\(.+\\)\\.org" nil t)
        (setq my-file-name (buffer-substring (match-beginning 1) (match-end 1)))) ; with-temp-buffer
      (find-file (format "%s.md" my-file-name))
      (goto-char (point-min))
      (while (re-search-forward "\\[\\\\!" nil t)
        (forward-char -1)
        (delete-char -1))
      (goto-char (point-min))
      (switch-to-buffer (format "%s.org" my-file-name))
      (while (re-search-forward "+begin_src" nil t)
        (skip-chars-forward " ")
        (setq my-lang-name (buffer-substring (point) (progn (end-of-line) (point))))
        (forward-line)
        (skip-chars-forward " ")
        (setq my-src-begin (buffer-substring (point) (progn (end-of-line) (point))))
        (re-search-forward "+end_src" nil t)
        (forward-line -1)
        (skip-chars-forward " ")
        (setq my-src-end (buffer-substring (point) (progn (end-of-line) (point))))
        (switch-to-buffer (format "%s.md" my-file-name))
        (re-search-forward (format "%s" my-src-begin) nil t)
        (beginning-of-line)
        (skip-chars-forward " ")
        (delete-region (point) (progn (beginning-of-line) (point)))
        (insert (format "```%s\n" my-lang-name))
        (forward-line)
        (catch 'foo
          (while t
            (skip-chars-forward " ")
            (delete-region (point) (progn (beginning-of-line) (point)))
            (if (string= (format "%s" my-src-end) (buffer-substring (point) (progn (end-of-line) (point))))
                (progn
                  (end-of-line)
                  (insert "\n```")
                  (throw 'foo t))
              (forward-line)))) ; catch
        (switch-to-buffer (format "%s.org" my-file-name))) ; while
      (switch-to-buffer (format "%s.md" my-file-name))
      (save-buffer)
      (kill-buffer (format "%s.md" my-file-name))
      (switch-to-buffer (format "%s.org" my-file-name))
      (message "Done!"))))
;;
(defun my-insert-org-template ()
  "Org ファイルのひな形を作成する"
  (insert "#+TITLE: \n#+AUTHOR: suyeden\n#+EMAIL: \n#+OPTIONS: toc:nil num:nil author:nil creator:nil LaTeX:t \\n:t\n#+STARTUP: showall\n\n* ")
  (goto-char (point-min))
  (re-search-forward "+TITLE: " nil t))

;;; ファイルマネージャを起動する
(define-key global-map "\C-cf" 'my-Emacs-open-file-manager)
;;
(defun my-Emacs-open-file-manager ()
  "現在使用している OS を判定してファイルマネージャを起動する"
  (interactive)
  (let (file-manager-open-file)
    (if (string= "windows-nt" (format "%s" system-type))
        (progn
          (setq file-manager-open-file (expand-file-name (read-directory-name "File manager: " default-directory)))
          (with-temp-buffer
            (insert file-manager-open-file)
            (goto-char (point-min))
            (while (re-search-forward "/" nil t)
              (delete-char -1)
              (insert "\\"))
            (goto-char (point-min))
            (setq file-manager-open-file (buffer-substring-no-properties (point) (progn (end-of-line) (point)))))
          (shell-command-to-string (format "explorer %s" file-manager-open-file)))
      (if (string= "gnu/linux" (format "%s" system-type))
          (progn
            (setq file-manager-open-file (expand-file-name (read-directory-name "File manager: " default-directory)))
            (call-process-shell-command (format "exo-open --launch FileManager %s &" file-manager-open-file) nil 0))
        nil))))

;;; 現在のカーソル位置を保持して、再度呼ばれた時に記録したカーソル位置に戻る
(defvar my-Emacs-record-marker)
(define-key global-map "\C-c\C-p" 'my-Emacs-record-point)
;;
(defun my-Emacs-record-point ()
  "マーカーが記録されていなければマーカーを作成し、記録されていればマーカー位置に移動してマーカーを削除する"
  (interactive)
  (if (not (boundp 'my-Emacs-record-marker))
      (progn
        (setq my-Emacs-record-marker (make-marker))
        (set-marker my-Emacs-record-marker (point))
        (message "Point recorded!"))
    (goto-char (marker-position my-Emacs-record-marker))
    (makunbound 'my-Emacs-record-marker)
    (message "Move point!")))

;;; 記録したカーソル位置を破棄して、新しいカーソル位置を記録する
(define-key global-map "\C-cp" 'my-Emacs-force-record-point)
;;
(defun my-Emacs-force-record-point ()
  "新しいカーソル位置を強制的に記録する"
  (interactive)
  (if (not (boundp 'my-Emacs-record-marker))
      (progn
        (setq my-Emacs-record-marker (make-marker))
        (set-marker my-Emacs-record-marker (point))
        (message "Point recorded!"))
    (if (y-or-n-p "Other point has already been recorded! Continue this process?")
        (progn
          (set-marker my-Emacs-record-marker nil)
          (set-marker my-Emacs-record-marker (point))
          (message "Point recorded!"))
      (message "Process killed"))))
