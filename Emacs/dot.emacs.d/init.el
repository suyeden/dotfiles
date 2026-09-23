;;; init.el --- suyeden's configuration file for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2026 suyeden

;; Author: suyeden
;; Keywords: internal, local
;; Package-Requires: ((emacs "31.1"))

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

;;; straight

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        user-emacs-directory))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;; use-package

(setq straight-use-package-by-default t)

(require 'use-package)

(when (eq system-type 'windows-nt)
  (setq w32-pipe-read-delay 0)
  (setq w32-pipe-buffer-size (* 1024 1024)))

(setq read-process-output-max (* 1024 1024))
(setq gc-cons-threshold (* 100 1024 1024))

;;; 外部パッケージ

;; テーマ
(use-package doom-themes
  :config
  (load-theme 'doom-dracula t))

;; undo-tree
(use-package undo-tree
  :config
  (global-undo-tree-mode 1)
  (setq undo-tree-auto-save-history nil))

;; exec-path-from-shell
(use-package exec-path-from-shell
  :if (eq system-type 'gnu/linux)
  :config
  (exec-path-from-shell-initialize))

;; magit
(use-package magit)

;; git-gutter
(use-package git-gutter
  :config
  (global-git-gutter-mode 1))

;; completion
(use-package corfu
  :init
  (setq corfu-auto t
        corfu-cycle t
        corfu-preselect 'prompt)
  :config
  (global-corfu-mode))

(use-package corfu-terminal
  :unless (display-graphic-p)
  :after corfu
  :config
  (corfu-terminal-mode 1))

;; LSP
(use-package eglot
  :hook ((typescript-ts-mode . eglot-ensure)
         (tsx-ts-mode . eglot-ensure)
         (js-ts-mode . eglot-ensure)
         (html-ts-mode . eglot-ensure)
         (css-ts-mode . eglot-ensure)
         (json-ts-mode . eglot-ensure))
  :config
  (setq eglot-ignored-server-capabilities
        '(:semanticTokensProvider
          :documentHighlightProvider)))

;; Vue.js
(use-package web-mode
  :mode ("\\.vue\\'" . web-mode)
  :hook (web-mode . eglot-ensure)
  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '(web-mode . config-vue-ls-contact))))

;; Markdown
(use-package markdown-mode
  :mode ("\\.md\\'" . gfm-mode))

;; SQL
(use-package sql-indent
  :hook (sql-mode . sqlind-minor-mode))

;; Apheleia
(use-package apheleia
  :config
  (apheleia-global-mode +1)
  (setf (alist-get 'web-mode apheleia-mode-alist) 'prettier
        (alist-get 'sql-mode apheleia-mode-alist) 'sqlformat))

;; HTTP REST client
(use-package restclient
  :mode ("\\.http\\'" . restclient-mode))

;;; 言語別設定

;; Tree-sitter
(setq treesit-language-source-alist
      '((typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (json "https://github.com/tree-sitter/tree-sitter-json"))
      treesit-auto-install-grammar 'always
      treesit-font-lock-level 4)

;; TypeScript / JavaScript
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js-ts-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . tsx-ts-mode))

;; HTML
(add-to-list 'auto-mode-alist '("\\.html\\'" . html-ts-mode))
(setq sgml-basic-offset 2)

;; CSS
(add-to-list 'auto-mode-alist '("\\.css\\'" . css-ts-mode))
(setq css-indent-offset 2)

;; JSON
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-ts-mode))

;;; 基本設定

;; 言語・文字コード
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)

(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file t)

;; 一般挙動
(setq inhibit-startup-message t
      vc-follow-symlinks t
      make-backup-files nil
      delete-auto-save-files t
      global-auto-revert-non-file-buffers t
      tab-width 2
      scroll-conservatively 35
      scroll-step 1
      ring-bell-function 'ignore
      eol-mnemonic-dos "(CRLF)"
      eol-mnemonic-mac "(CR)"
      eol-mnemonic-unix "(LF)")

(normal-erase-is-backspace-mode 1)
(which-key-mode 1)
(global-auto-revert-mode 1)

(setq-default indent-tabs-mode nil)

(defalias 'yes-or-no-p 'y-or-n-p)

;; repeat-mode
(repeat-mode 1)

(defvar config-undo-repeat-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "u") #'undo-tree-undo)
    (define-key map (kbd "r") #'undo-tree-redo)
    map))

(put 'undo-tree-undo 'repeat-map 'config-undo-repeat-map)
(put 'undo-tree-redo 'repeat-map 'config-undo-repeat-map)

;; 矩形選択
(cua-mode 1)
(setq cua-enable-cua-keys nil)

;; カーソル移動・履歴
(setq set-mark-command-repeat-pop t
      mark-ring-max 32
      global-mark-ring-max 64)

;;; UI 設定

(set-face-attribute 'delete-selection-replacement nil
                    :foreground 'unspecified
                    :background 'unspecified
                    :inherit nil)

(delete-selection-mode 1)
(show-paren-mode 1)
(column-number-mode 1)
(electric-pair-mode 1)
(global-display-line-numbers-mode 1)
(tab-bar-mode 1)
(global-tab-line-mode 1)

(menu-bar-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode -1)

;;; フォント設定

(add-to-list 'default-frame-alist
             (cons 'font
                   (if (eq system-type 'windows-nt)
                       "Consolas 11"
                     "Cica 12")))

;;; 自作関数（コマンド）

(defun my-smart-move-beginning-of-line ()
  "Move point to first non-whitespace character or beginning of line."
  (interactive)
  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(defun my-kill-emacs ()
  "Confirm before exiting Emacs."
  (interactive)
  (if (y-or-n-p "Kill Emacs?")
      (save-buffers-kill-terminal)
    (message "")))

;;; 自作関数（設定・hook・advice 用）

(defun config-org-capture-finalize (old-func &rest args)
  "After org-capture-finalize, kill the buffer associated with the last captured entry."
  (apply old-func args)
  (when (ignore-errors (org-capture-goto-last-stored) t)
    (kill-buffer (current-buffer)))
  (message ""))

(defun config-vue-ls-contact (_interactive)
  "Build the contact for `vue-language-server' with an absolute tsdk path."
  (let* ((root (if (project-current)
                   (project-root (project-current))
                 default-directory))
         (tsdk (expand-file-name "node_modules/typescript/lib" root)))
    (list "vue-language-server" "--stdio"
          :initializationOptions
          (list :typescript (list :tsdk tsdk)))))

;;; キーバインド

(global-set-key (kbd "C-a") #'my-smart-move-beginning-of-line)
(global-set-key (kbd "M-n") #'forward-list)
(global-set-key (kbd "M-p") #'backward-list)
(global-set-key (kbd "C-x g") #'magit-status)
(global-set-key (kbd "C-x <down>") #'bury-buffer)
(global-set-key (kbd "C-x C-<down>") #'bury-buffer)
(global-set-key (kbd "C-x C-c") #'my-kill-emacs)
(global-set-key (kbd "C-c u") #'undo-tree-undo)
(global-set-key (kbd "C-c r") #'undo-tree-redo)
(global-set-key (kbd "C-c m") #'delete-duplicate-lines)
(global-set-key (kbd "C-c c") #'org-capture)
(global-set-key (kbd "C-c l") #'org-store-link)

;;; dired

(setq dired-kill-when-opening-new-dired-buffer t
      dired-dwim-target t
      dired-recursive-copies 'always
      dired-isearch-filenames t)

;;; org-mode

(setq org-directory "~/org"
      org-hide-leading-stars t
      org-startup-indented t
      org-startup-folded 'showall
      org-startup-with-inline-images t
      org-startup-truncated nil
      org-todo-keywords '((sequence "TODO(t)" "|" "DONE(d)"))
      org-tags-column 0
      org-support-shift-select t)

(with-eval-after-load 'org
  (setcdr (assoc 'file org-link-frame-setup) 'find-file))

(setq org-capture-templates
      `(("n" "Note" entry
         (file ,(expand-file-name "notes.org" org-directory))
         "* %^{Title} :tech:%^g:\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n{事象}%?\n\n** Context\n\n{背景}\n\n** Solution\n\n{手順}\n\n#+begin_src {LANG}\n\n#+end_src\n\n** Comments\n\n{補足}\n\n"
         :empty-lines 1 :kill-buffer 1)

        ("N" "Check Notes" plain
         (file ,(expand-file-name "notes.org" org-directory))
         nil :unnarrowed 1 :kill-buffer 1)

        ("m" "Memo" entry
         (file ,(expand-file-name "memos.org" org-directory))
         "* %?" :empty-lines 1 :kill-buffer 1)

        ("M" "Check Memos" plain
         (file ,(expand-file-name "memos.org" org-directory))
         nil :unnarrowed 1 :kill-buffer 1)

        ("t" "Task" entry
         (file ,(expand-file-name "tasks.org" org-directory))
         "* TODO %?" :kill-buffer 1)

        ("T" "Check Tasks" plain
         (file ,(expand-file-name "tasks.org" org-directory))
         nil :unnarrowed 1 :kill-buffer 1)))

(advice-add 'org-capture-finalize :around #'config-org-capture-finalize)

;;; init.el ends here
