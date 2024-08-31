;;; init.el --- suyeden's configuration file for Emacs -*- Emacs-Lisp -*-

;; Copyright (C) 2019-2024 suyeden

;; Author: suyeden
;; Keywords: internal, local
;; Package-Requires: ((emacs "29.4"))

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

;; Place this file in .emacs.d directory.

;;; Code:

;;; 環境を日本語, 基本 UTF-8 にする
(set-locale-environment nil)
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-default 'buffer-file-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(if (equal 'windows-nt system-type)
    (progn
      (set-file-name-coding-system 'cp932)
      (set-terminal-coding-system 'cp932)
      (set-keyboard-coding-system 'cp932))
  (set-file-name-coding-system 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8))

;;; カスタムファイル
(setq custom-file (locate-user-emacs-file "custom.el"))
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
(load-file custom-file)

;;; package
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("ELPA" . "http://tromey.com/elpa/") t)
(package-initialize)

;;; use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;; load-path 追加関数
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory (locate-user-emacs-file path)))
        (add-to-list 'load-path default-directory)
        (when (fboundp 'normal-top-level-add-subdirs-to-load-path)
          (normal-top-level-add-subdirs-to-load-path))))))

;;; load-path 追加
(add-to-load-path "conf" "elisp" "public_repos")

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

;;; フォント
(add-to-list 'default-frame-alist '(font . "Consolas 11"))

;;; カラーテーマ
(use-package modus-themes
  :ensure t
  :config
  (load-theme 'modus-vivendi :no-confirm))

;;; auto-complete
(use-package auto-complete
  :ensure t
  :init
  (make-auto-save-file-name)
  :config
  ;; auto-complete-config の設定ファイルを読み込む
  (require 'auto-complete-config)
  (ac-config-default)
  ;; TABキーで自動補完を有効にする
  (ac-set-trigger-key "TAB")
  ;; auto-complete-mode を起動時に有効にする
  (global-auto-complete-mode t))

;;; undo-tree
(use-package undo-tree
  :ensure t
  :bind
  ("\C-z" . undo-tree-undo)
  ("\C-q" . undo-tree-redo)
  :config
  ;; undo-tree を起動時に有効にする
  (global-undo-tree-mode t)
  ;; 履歴ファイルを作らない
  (setq undo-tree-auto-save-history nil))

;;; grip-mode
;; Python(3) (および python3-pip) を導入済みであることを確認
;; また pip(3) install grip を実行済みであることを確認
(use-package grip-mode
  :ensure t
  :bind
  ("\C-cg" . grip-mode))

;;; exec-path-from-shell
;; gnu/linux のときは PATH の設定を引き継ぐ
(use-package exec-path-from-shell
  :if (equal 'gnu/linux system-type)
  :ensure t
  :config
  (exec-path-from-shell-initialize))

;;; point-undo
(when (require 'point-undo nil t)
  (define-key global-map "\M-[" 'point-undo)
  (define-key global-map "\M-]" 'point-redo))

;;; 行のカーソル以降を削除する
(define-key global-map "\C-k" 'my-delete-line-forward)
(defun my-delete-line-forward ()
  "カーソル位置から行末まで削除"
  (interactive)
  (if (= (point) (save-excursion (end-of-line) (point)))
      (delete-char 1)
    (delete-region (point) (progn (end-of-line) (point)))))

;;; 行のカーソル以降をkillする
(define-key global-map "\C-ck" 'my-kill-line)
(defun my-kill-line ()
  "カーソル位置から行末まで切り取り"
  (interactive)
  (kill-region (point) (progn (end-of-line) (point))))

;;; ウィンドウ間の移動のキーバインド変更
(global-set-key "\C-t" 'other-window)

;;; カーソル位置から行頭まで削除する
(define-key global-map "\C-cd" 'my-delete-line-backward)
(defun my-delete-line-backward ()
  "カーソル位置から行頭まで削除"
  (interactive)
  (delete-region (point) (progn (beginning-of-line) (point))))

;;; カーソルを固定したまま画面を次ページにスクロール
(define-key global-map "\M-n" 'my-scroll-up)
(defun my-scroll-up ()
  "カーソル位置固定で1行下にスクロール"
  (interactive)
  (scroll-up 1))

;;; カーソルを固定したまま画面を前ページにスクロール
(define-key global-map "\M-p" 'my-scroll-down)
(defun my-scroll-down ()
  "カーソル位置固定で1行上にスクロール"
  (interactive)
  (scroll-down 1))

;;; 重複行のマージ
(define-key global-map "\C-cm" 'delete-duplicate-lines)

;;; ファイルマネージャを起動する（Windows）
(define-key global-map "\C-cf" 'my-open-file-manager)
(defun my-open-file-manager ()
  "ファイルマネージャ起動"
  (interactive)
  (let (file-manager-open-file)
    (when (equal 'windows-nt system-type)
      (setq file-manager-open-file (expand-file-name (read-directory-name "File manager: " default-directory)))
      (while (string-match "/" file-manager-open-file)
        (setq file-manager-open-file (replace-match "\\\\" nil nil file-manager-open-file)))
      (shell-command-to-string (format "explorer %s" file-manager-open-file)))))

;;; パス区切り文字の変換
(define-key global-map "\C-c\M-d" 'my-convert-dir-separator)
(defun my-convert-dir-separator ()
  "渡されたパスをWindows表記からUnix系表記に変換
パス区切り文字をバックスラッシュからスラッシュに変換する"
  (interactive)
  (let (path)
    (setq path (expand-file-name (read-string "path? : ")))
    (kill-new path)
    (message "\"%s\" has been copied to the clipboard !" path)))

;;; org-mode
;; org ファイルは自動的に org-mode
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
;; 見出しの余分な * を消す
(setq org-hide-leading-stars t)
;; 見出しごとにインデント
(setq org-startup-indented t)
;; すべての記述内容を表示
(setq org-startup-folded 'showall)
;; 画像をインラインで表示
(setq org-startup-with-inline-images t)
;; 行の折り返し
(setq org-startup-truncated nil)
;; TODO の状態
(setq org-todo-keywords
      '((sequence "TODO(t)" "|" "DONE(d)")))
;; 見出しの色の設定
(require 'org-faces)
(set-face-attribute 'org-level-4 nil :foreground "#99cc99")
(set-face-attribute 'org-level-5 nil :foreground "#ffffb6")
(set-face-attribute 'org-level-6 nil :foreground "gray45")
;; リンク保存
(define-key global-map "\C-cl" 'org-store-link)

;;; org-capture
(define-key global-map "\C-cc" 'org-capture)
;; org 用ディレクトリ
(setq org-directory "~/org")
;; 既定ファイル名
(setq org-default-notes-file (concat org-directory "/notes.org"))
;; capture 用テンプレート
(setq org-capture-templates
      '(("m"
         "Memo"
         entry
         (file+datetree org-default-notes-file)
         "* %U\n %?\n"
         :empty-lines 1)
        ("t"
         "Task"
         entry
         (file+datetree org-default-notes-file)
         "* TODO %? %T\n"
         :empty-lines 1)
        ("c"
         "Check"
         plain
         (file+datetree org-default-notes-file)
         "%?"
         :jump-to-captured 1
         :unnarrowed 1)))

;;; Org ファイル編集
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
;; Markdown 出力
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
;;; init.el ends here
