(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (tangotango-theme flatland-black-theme ir-black-theme dirtree auto-complete undo-tree))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "gray12" :foreground "#F6F3E8" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 135 :width normal :foundry "outline" :family "Ricty Diminished"))))
 '(font-lock-comment-face ((t (:foreground "gray45")))))

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
(setq scroll-conservatively 1)

;;; C-kで行のカーソル以降を削除する
(defun forward-delete-line ()
  "Delete chars forward until encountering the end of a line."
  (interactive)
  (let ((p (point)))
    (if (= p (progn (end-of-line) (point)))
        (delete-forward-char 1)
      (delete-region p (progn (end-of-line) (point))))))
;;
(define-key global-map "\C-k" 'forward-delete-line)

;;; C-c K で行のカーソル以降をkillする
(defun forward-kill-line ()
  "Kill chars forward until encountering the end of a line."
  (interactive)
  (kill-region
   (point)
   (progn
     (end-of-line)
     (point))))
;;
(define-key global-map "\C-cK" 'forward-kill-line)

;;; dired設定
(require 'dired-x)

;;; "yes or no"の選択を"y or n"にする
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

;;; ウィンドウ間の移動のキーバインド変更
(global-set-key "\C-t" 'other-window)

;;; パッケージ読み込み(package.elの有効化)
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
(package-initialize)

;;; auto-complete
;;
;; auto-complete-configの設定ファイルを読み込む
(require 'auto-complete-config)
;;
(ac-config-default)
;; TABキーで自動補完を有効にする
(ac-set-trigger-key "TAB")
;; auto-complete-modeを起動時に有効にする
(global-auto-complete-mode t)

;;; undo-tree
;;
(require 'undo-tree)
;; undo-treeを起動時に有効にする
(global-undo-tree-mode t)
;; M-/をredoに設定する
(global-set-key (kbd "M-/") 'undo-tree-redo)

;;; dirtree
(require 'dirtree)

;;; C-c c で compile コマンドを呼び出す
(define-key mode-specific-map "c" 'compile)

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

;;; SBCL をデフォルトの Common Lisp 処理系に設定
(setq inferior-lisp-program "sbcl")
;; ~/slime を load-path に追加
(add-to-list `load-path (expand-file-name "~/slime"))
;; SLIME のロード
(require 'slime)
(slime-setup '(slime-repl slime-fancy slime-banner))

;;; カラーテーマの変更
(load-theme 'ir-black t)

;;; Schemeモードの設定
;; gaucheに渡す文字コードをUTF-8に設定
(setq process-coding-system-alist
      (cons '("gosh" utf-8 . utf-8) process-coding-system-alist))
;; Emacsのバッファ内で動かすScheme処理系をgaucheに設定
(setq scheme-program-name "gosh -i")
;; Schemeモードをより便利なcmuscheme.elで使用する設定
(autoload 'scheme-mode "cmuscheme" "Major mode for Scheme." t)
(autoload 'run-scheme "cmuscheme" "Run an inferior Scheme process." t)
;; ウィンドウを2つに分け、一方でgaucheインタプリタを実行する関数（コマンド）を定義し、C-c S で起動できるように設定
(defun scheme-other-window ()
  "Run scheme on other window"
  (interactive)
  (switch-to-buffer-other-window
   (get-buffer-create "*scheme*"))
  (run-scheme scheme-program-name))
(define-key global-map
  "\C-cS" 'scheme-other-window)

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

;;; C-c p で前の括弧に飛ぶ
(define-key global-map "\C-cp" 'my-move-backward-paren)
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
