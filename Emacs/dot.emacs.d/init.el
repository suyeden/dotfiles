;;; init.el --- suyeden's configuration file for Emacs -*- Emacs-Lisp -*-

;; Copyright (C) 2019-2025 suyeden

;; Author: suyeden
;; Keywords: internal, local
;; Package-Requires: ((emacs "30.1"))

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

;;; スタートアップメッセージを表示させない
(setq inhibit-startup-message t)

;;; バックアップファイルを作成させない
(setq make-backup-files nil)

;;; 終了時にオートセーブファイルを削除する
(setq delete-auto-save-files t)

;;; タブにスペースを使用しない
(setq-default tab-width 2 indent-tabs-mode nil)

;;; 改行コードの表示方法
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
(setq ring-bell-function 'ignore)

;;; 括弧の自動補完
(electric-pair-mode 1)

;;; 行番号を左端に表示
(global-display-line-numbers-mode)

;;; フォント
(if (equal 'windows-nt system-type)
    (add-to-list 'default-frame-alist '(font . "Consolas 11"))
  (add-to-list 'default-frame-alist '(font . "Cica")))

;;; カラーテーマ
(load-theme 'deeper-blue t)

;;; redo
(global-set-key (kbd "C-S-z") #'undo-redo)

;;; 重複行のマージ
(define-key global-map "\C-cm" 'delete-duplicate-lines)
;;; init.el ends here
