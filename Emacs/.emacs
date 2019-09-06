(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (dirtree auto-complete undo-tree))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Ricty Diminished" :foundry "outline" :slant normal :weight normal :height 100 :width normal)))))

;;環境を日本語,UTF-8にする
(set-locale-environment nil)
(set-language-environment "Japanese")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)

;;バックアップファイルを作成させない
(setq make-backup-files nil)

;;終了時にオートセーブファイルを削除する
(setq delete-auto-save-files t)

;;タブにスペースを使用する
(setq-default tab-width 3 indent-tabs-mode nil)

;;改行コードを表示する
(setq eol-mnemonic-dos "(CRLF)")
(setq eol-mnemonic-mac "(CR)")
(setq eol-mnemonic-unix "(LF)")

;;メニューバーを消す
(menu-bar-mode -1)

;;ツールバーを消す
(tool-bar-mode -1)

;;列数を表示する
(column-number-mode t)

;;行数を表示する
(global-linum-mode t)

;;カーソルの点滅をやめる
(blink-cursor-mode 0)

;;対応する括弧を光らせる
(show-paren-mode 1)

;;スクロールは1行ごとに
(setq scroll-conservatively 1)

;;C-kで行全体を削除する
(setq kill-whole-line t)

;;dired設定
(require 'dired-x)

;;"yes or no"の選択を"y or n"にする
(fset 'yes-or-no-p 'y-or-n-p)

;;beep音を消す
(defun my-bell-function()
  (unless (memq this-command
                '(isearch-abort abort-recursive-edit exit-minibuffer
                                keyboard-quit mwheel-scroll down up next-line previous-line backward-char forward-char))
    (ding)))
(setq ring-bell-function 'my-bell-function)

;;括弧の自動補完
(electric-pair-mode 1)

;;カーソル行をハイライトする
(global-hl-line-mode t)

;;ウィンドウ間の移動のキーバインド変更
(global-set-key "\C-t" 'other-window)

;;パッケージ読み込み(package.elの有効化)
;;
(require 'package)
;;
;;HTTPS系のリポジトリ
;;(add-to-list 'package-archives '("melpa"."https://melpa.milkbox.net/packages/")t)
;;(add-to-list 'package-archives '("melpa-stable"."https://stable.melpa.org/packages/")t)
;;(add-to-list 'package-archives '("marmalade"."https://marmalade-repo.org/packages/")t)
;;
;;HTTP系のリポジトリ
(add-to-list 'package-archives '("melpa"."http://melpa.milkbox.net/packages/")t)
(add-to-list 'package-archives '("melpa-stable"."http://stable.melpa.org/packages/")t)
(add-to-list 'package-archives '("org"."http://orgmode.org/elpa/")t)
(add-to-list 'package-archives '("ELPA"."http://tromey.com/elpa/")t)
;;
;;marmaladeはHTTPアクセスすると証明書エラーでフリーズするので注意
;;(add-to-list 'package-archives '("marmalade"."http://marmalade-repo.org/packages/")t)
;;
(package-initialize)

;;auto-complete
;;
;;auto-complete-configの設定ファイルを読み込む
(require 'auto-complete-config)
;;
(ac-config-default)
;;TABキーで自動補完を有効にする
(ac-set-trigger-key "TAB")
;;auto-complete-modeを起動時に有効にする
(global-auto-complete-mode t)

;;undo-tree
;;
(require 'undo-tree)
;;undo-treeを起動時に有効にする
(global-undo-tree-mode t)
;;M-/をredoに設定する
(global-set-key (kbd "M-/") 'undo-tree-redo)

;;dirtree
(require 'dirtree)

;;C-c c で compile コマンドを呼び出す
(define-key mode-specific-map "c" 'compile)

;;C-c d でカーソル位置から行頭まで削除する
;;カーソル位置から行頭まで削除
(defun backward-kill-line (arg)
  "Kill chars backward until encountering the end of a line."
  (interactive "p")
  (kill-line 0))
;;C-c d に設定
(define-key global-map (kbd "C-c d") 'backward-kill-line)
