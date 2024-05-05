;;; .emacs --- suyeden's configuration file for Emacs -*- Emacs-Lisp -*-

;; Copyright (C) 2019-2024 suyeden

;; Author: suyeden
;; Keywords: internal, local
;; Package-Requires: ((emacs "27.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Place this file in home directory.

;;; Code:

;;; 環境を日本語, 基本 UTF-8 にする
(set-locale-environment nil)
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-default 'buffer-file-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(if (string= "windows-nt" (format "%s" system-type))
    (progn
      (set-file-name-coding-system 'cp932)
      (set-terminal-coding-system 'cp932)
      (set-keyboard-coding-system 'cp932))
  (set-file-name-coding-system 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8))

;;; スタートアップメッセージを表示させない
(setq inhibit-startup-message t)

;;; バックアップファイルを作成させない
(setq make-backup-files nil)

;;; 終了時にオートセーブファイルを削除する
(setq delete-auto-save-files t)

;;; タブにスペースを使用しない
(setq-default tab-width 2 indent-tabs-mode nil)

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

;;; 行番号を左端に表示
(global-display-line-numbers-mode)

;;; face の設定
(if (string= "windows-nt" (format "%s" system-type))
    (set-face-attribute 'default t :inherit nil :stipple nil :background "#1e1e1e" :foreground "#d4d4d4" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant 'normal :weight 'normal :height 120 :width 'normal :foundry "outline" :family "Ricty Diminished")
  (if (string= "gnu/linux" (format "%s" system-type))
      (progn
        (set-face-attribute 'default t :inherit nil :stipple nil :background "#1e1e1e" :foreground "#d4d4d4" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant 'normal :weight 'normal :height 135 :width 'normal :foundry "outline" :family "Ricty Diminished")
        ;; Japanese font
        (set-fontset-font t 'japanese-jisx0208 (font-spec :family "IPAExGothic")))
    (set-face-attribute 'default t :inherit nil :stipple nil :background "#1e1e1e" :foreground "#d4d4d4" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant 'normal :weight 'normal :height 120 :width 'normal :foundry "outline" :family "Ricty Diminished")))


;;;;
;;;; package, mode, file 関連
;;;;


;;; カスタムファイルの指定
(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p (expand-file-name custom-file))
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
(load-theme 'vscode-dark-plus t)

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
;; C-z を undo に設定する
(define-key global-map "\C-z" 'undo-tree-undo)
;; C-q を redo に設定する
(define-key global-map "\C-q" 'undo-tree-redo)

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
;; 見出しの色の設定
(require 'org-faces)
(set-face-attribute 'org-level-4 nil :foreground "#99cc99")
(set-face-attribute 'org-level-5 nil :foreground "#ffffb6")
(set-face-attribute 'org-level-6 nil :foreground "gray45")
;; キーバインド
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-cc" 'org-capture)
(define-key global-map "\C-ca" 'org-agenda)

;;; grip-mode
;; Python(3) (および python3-pip) を導入済みであることを確認
;; また pip(3) install grip を実行済みであることを確認
;; C-c g で grip-mode を起動する
(define-key global-map "\C-cg" 'grip-mode)

;;; exec-path-from-shell
;; gnu/linux のときは PATH の設定を引き継ぐ
(when (string= "gnu/linux" (format "%s" system-type))
  (exec-path-from-shell-initialize))

;;; 自作メジャーモードのロード
(add-to-list 'load-path "~/.emacs.d/lisp")
;;
;; blog-mode の読み込み
(load-library "blog-mode.el")


;;;;
;;;; key-bindings
;;;;


;;; C-k で行のカーソル以降を削除する
(define-key global-map "\C-k"
  '(lambda ()
     "カーソル位置から行末まで削除"
     (interactive)
     (if (= (point) (save-excursion (end-of-line) (point)))
         (delete-char 1)
       (delete-region (point) (progn (end-of-line) (point))))))

;;; C-c k で行のカーソル以降をkillする
(define-key global-map "\C-ck"
  '(lambda ()
     "カーソル位置から行末まで切り取り"
     (interactive)
     (kill-region (point) (progn (end-of-line) (point)))))

;;; ウィンドウ間の移動のキーバインド変更
(global-set-key "\C-t" 'other-window)

;;; C-c m で compile コマンドを呼び出す
(define-key mode-specific-map "m" 'compile)

;;; C-c d でカーソル位置から行頭まで削除する
(define-key global-map "\C-cd"
  '(lambda ()
     "カーソル位置から行頭まで削除"
     (interactive)
     (delete-region (point) (progn (beginning-of-line) (point)))))

;;; M-n でカーソルを固定したまま画面を次ページにスクロール
(define-key global-map "\M-n"
  '(lambda ()
     "カーソル位置固定で1行下にスクロール"
     (interactive)
     (scroll-up 1)))

;;; M-p でカーソルを固定したまま画面を前ページにスクロール
(define-key global-map "\M-p"
  '(lambda ()
     "カーソル位置固定で1行上にスクロール"
     (interactive)
     (scroll-down 1)))

;;; Org ファイルを開いている時 C-x x で諸機能を提供
(define-key global-map "\C-xx"
  '(lambda ()
     "Org ファイル編集機能
x : Markdown に書き出して整形
t : Org ファイルのひな形を作成"
     (interactive)
     (let (my-org-option-mode)
       (if (string-match ".+\\.org" (format "%s" (buffer-name)))
           (progn
             (while (not (or (string= "x" my-org-option-mode) (string= "t" my-org-option-mode)))
               (setq my-org-option-mode (read-string "[x] Export to Markdown\n[t] Insert a Template for Org-file\ncommand ? : ")))
             (if (string= "x" my-org-option-mode)
                 (my-org-to-md)
               (if (string= "t" my-org-option-mode)
                   (my-insert-org-template)
                 nil)))
         (message "Not Org-file !")))))
;;
;; バイトコンパイル時の org-md-export-to-markdown に対する警告メッセージ対策
(declare-function org-md-export-to-markdown (locate-library "ox-md.el"))
;;
(defun my-org-to-md ()
  "Org ファイルを Markdown ファイルに書き出す"
  (let (my-file-name my-lang-name my-src-begin my-line-count my-current-buf my-md-buf)
    (save-excursion
      (org-md-export-to-markdown)
      ;; カレントバッファ（org）の記録
      (setq my-current-buf (current-buffer))
      ;; ファイル名の記録
      (setq my-file-name (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))
      ;; カレントバッファ（md）の記録
      (find-file (format "%s.md" my-file-name))
      (setq my-md-buf (current-buffer))
      ;; バッジ部分の修正
      (goto-char (point-min))
      (save-excursion
        (while (re-search-forward "\\[\\\\!" nil t)
          (forward-char -1)
          (delete-char -1)))
      ;; コードブロック部分の修正
      (switch-to-buffer my-current-buf)
      (goto-char (point-min))
      (let ((current-line-count 0))
        (while (search-forward "+begin_src" nil t)
          (setq my-line-count 0)
          (setq current-line-count 0)
          ;; 使用言語の記録
          (skip-chars-forward " ")
          (setq my-lang-name (buffer-substring (point) (progn (end-of-line) (point))))
          ;; コードブロックの始まりを記録
          (forward-line 1)
          (skip-chars-forward " ")
          (setq my-src-begin (buffer-substring (point) (progn (end-of-line) (point))))
          (save-excursion
            (setq current-line-count 1)
            (while (not (= (point) (point-min)))
              (forward-line -1)
              (setq current-line-count (1+ current-line-count))))
          (setq my-line-count current-line-count)
          ;; 終了までの行数を数える
          (search-forward "+end_src" nil t)
          (forward-line -1)
          (save-excursion
            (setq current-line-count 1)
            (while (not (= (point) (point-min)))
              (forward-line -1)
              (setq current-line-count (1+ current-line-count))))
          (setq my-line-count (- current-line-count my-line-count))
          ;; md ファイル編集
          (switch-to-buffer my-md-buf)
          (search-forward my-src-begin)
          (beginning-of-line)
          (insert (format "```%s\n" my-lang-name))
          (while (<= 0 my-line-count)
            (skip-chars-forward " ")
            (delete-region (point) (progn (beginning-of-line) (point)))
            (forward-line 1)
            (setq my-line-count (1- my-line-count)))
          (insert "```\n")
          (save-buffer)
          (switch-to-buffer my-current-buf)))
      (switch-to-buffer my-md-buf)
      (save-buffer)
      (kill-buffer my-md-buf)
      (message "Done!"))))
;;
(defun my-insert-org-template ()
  "Org ファイルのひな形を作成する"
  (insert "#+TITLE: \n#+AUTHOR: suyeden\n#+EMAIL: \n#+OPTIONS: toc:nil num:nil author:nil creator:nil LaTeX:t \\n:t\n#+STARTUP: showall\n\n* ")
  (goto-char (point-min))
  (re-search-forward "+TITLE: " nil t))

;;; ファイルマネージャを起動する
(define-key global-map "\C-cf"
  '(lambda ()
     "ファイルマネージャ起動"
     (interactive)
     (let (file-manager-open-file)
       (if (string= "windows-nt" (format "%s" system-type))
           (progn
             (setq file-manager-open-file (expand-file-name (read-directory-name "File manager: " default-directory)))
             (while (string-match "/" file-manager-open-file)
               (setq file-manager-open-file (replace-match "\\\\" nil nil file-manager-open-file)))
             (shell-command-to-string (format "explorer %s" file-manager-open-file)))
         (if (string= "gnu/linux" (format "%s" system-type))
             (progn
               (setq file-manager-open-file (expand-file-name (read-directory-name "File manager: " default-directory)))
               (call-process-shell-command (format "pcmanfm %s" file-manager-open-file) nil 0))
           nil)))))

;;; ブックマーク機能
(defvar MyEmacs-RecordedPoint)
(defvar MyEmacs-RecordedBuffername)
;;
;; 現在のカーソル位置を保持して、再度呼ばれた時に記録したカーソル位置に戻る
(define-key global-map "\C-cp"
  '(lambda ()
     "カーソル位置の保存、または保存位置へのジャンプ"
     (interactive)
     (if (and (boundp 'MyEmacs-RecordedPoint) (boundp 'MyEmacs-RecordedBuffername))
         (if (string= (buffer-name (current-buffer)) MyEmacs-RecordedBuffername)
             (progn
               (goto-char (marker-position MyEmacs-RecordedPoint))
               (makunbound 'MyEmacs-RecordedPoint)
               (makunbound 'MyEmacs-RecordedBuffername)
               (message "Moved point!"))
           (message (format "Point has already been recorded in %s ! Switch buffer !" MyEmacs-RecordedBuffername)))
       (if (not (boundp 'MyEmacs-RecordedPoint))
           (setq MyEmacs-RecordedPoint (make-marker)))
       (set-marker MyEmacs-RecordedPoint (point))
       (setq MyEmacs-RecordedBuffername (buffer-name (current-buffer)))
       (message (format "Point recorded in %s !" MyEmacs-RecordedBuffername)))))
;;
;; 記録したカーソル位置を破棄して、新しいカーソル位置を記録する
(define-key global-map "\C-c\M-p"
  '(lambda ()
     "保存したカーソル位置の破棄、および新たなカーソル位置の保存"
     (interactive)
     (if (and (not (boundp 'MyEmacs-RecordedPoint)) (not (boundp 'MyEmacs-RecordedBuffername)))
         (progn
           (if (not (boundp 'MyEmacs-RecordedPoint))
               (setq MyEmacs-RecordedPoint (make-marker)))
           (set-marker MyEmacs-RecordedPoint (point))
           (setq MyEmacs-RecordedBuffername (buffer-name (current-buffer)))
           (message (format "Point recorded in %s !" MyEmacs-RecordedBuffername)))
       (if (y-or-n-p (format "Other point has already been recorded in %s ! Continue this process ?" MyEmacs-RecordedBuffername))
           (progn
             (if (boundp 'MyEmacs-RecordedPoint)
                 (progn
                   (set-marker MyEmacs-RecordedPoint nil)
                   (set-marker MyEmacs-RecordedPoint (point)))
               (setq MyEmacs-RecordedPoint (make-marker))
               (set-marker MyEmacs-RecordedPoint (point)))
             (setq MyEmacs-RecordedBuffername (buffer-name (current-buffer)))
             (message (format "Point recorded in %s !" MyEmacs-RecordedBuffername)))
         (message "Process killed")))))

;;; 重複行のマージ
(define-key global-map "\C-c\M-m"
  '(lambda ()
     "重複行のマージ
範囲選択している場合、選択範囲の開始行から終了行までを対象に重複行マージ
上記以外の場合、カーソル行以降を対象に重複行マージ"
     (interactive)
     (let (line-content (del-count 0) start-point end-point)
       (save-excursion
         (if (use-region-p)
             ;; 範囲選択している場合、選択範囲の開始行から終了行までを対象に重複行マージ
             (save-restriction
               (setq start-point (region-beginning))
               (setq end-point (region-end))
               (goto-char start-point)
               (beginning-of-line)
               (setq start-point (point))
               (goto-char end-point)
               (forward-line 1)
               (setq end-point (point))
               (narrow-to-region start-point end-point)
               (goto-char (point-min))
               (while (not (= (point) (point-max)))
                 (setq line-content (buffer-substring (point) (progn (end-of-line) (point))))
                 (save-excursion
                   (while (re-search-forward (format "^%s$" line-content) nil t)
                     (beginning-of-line)
                     (delete-region (point) (progn (forward-line 1) (point)))
                     (setq del-count (1+ del-count))))
                 (forward-line 1)))
           ;; 上記以外の場合、カーソル行以降を対象に重複行マージ
           (beginning-of-line)
           (while (not (= (point) (point-max)))
             (setq line-content (buffer-substring (point) (progn (end-of-line) (point))))
             (save-excursion
               (while (re-search-forward (format "^%s$" line-content) nil t)
                 (beginning-of-line)
                 (delete-region (point) (progn (forward-line 1) (point)))
                 (setq del-count (1+ del-count))))
             (forward-line 1))))
       (message "%s lines merged !" del-count))))

;;; パス区切り文字の変換
(define-key global-map "\C-c\M-d"
  '(lambda ()
     "渡されたパスをWindows表記からUnix系表記に変換
パス区切り文字をバックスラッシュからスラッシュに変換する"
     (interactive)
     (let (path)
       (setq path (expand-file-name (read-string "path? : ")))
       (kill-new path)
       (message "\"%s\" has been copied to the clipboard !" path))))
;;; .emacs ends here
