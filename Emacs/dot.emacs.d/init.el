;;; init.el --- suyeden's configuration file for Emacs -*- Emacs-Lisp -*-

;; Copyright (C) 2019-2026 suyeden

;; Author: suyeden
;; Keywords: internal, local
;; Package-Requires: ((emacs "30.2"))

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

;;; package

(require 'package)

(setq package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "http://stable.melpa.org/packages/")
        ("org" . "http://orgmode.org/elpa/")
        ("ELPA" . "http://tromey.com/elpa/")))

(unless package-archive-contents
  (package-refresh-contents))

;;; use-package

(require 'use-package)

(setq use-package-always-ensure t)

;;; 外部パッケージ

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

;; which-key
(use-package which-key
  :config
  (which-key-mode 1))

;; magit
(use-package magit)

;; git-gutter
(use-package git-gutter
  :config
  (global-git-gutter-mode 1))

;; projectile
(use-package projectile
  :config
  (projectile-mode 1)
  :bind-keymap
  ("C-c p" . projectile-command-map))

;; complement
(use-package corfu
  :init
  (setq corfu-auto t
        corfu-cycle t)
  :config
  (global-corfu-mode))

;; Common Lisp
(use-package slime
  :config
  (setq inferior-lisp-program "sbcl")
  (slime-setup '(slime-fancy)))

;; LSP
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook ((typescript-mode . lsp-deferred)
         (js-mode . lsp-deferred)
         (html-mode . lsp-deferred)
         (css-mode . lsp-deferred)
         (web-mode . lsp-deferred)
         (json-mode . lsp-deferred)
         (lsp-mode . config-lsp-format-on-save))
  :config
  (setq lsp-prefer-flymake t
        lsp-enable-on-type-formatting nil))

;; TypeScript
(use-package typescript-mode
  :mode "\\.ts\\'")

;; Web templates
(use-package web-mode
  :mode (("\\.jsx\\'" . web-mode)
         ("\\.tsx\\'" . web-mode))
  :config
  (setq web-mode-enable-auto-closing t
        web-mode-enable-auto-quoting t
        web-mode-markup-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-content-types-alist
        '(("jsx" . "\\.[jt]sx\\'"))))

;; JSON
(use-package json-mode
  :mode "\\.json\\'")

;; Markdown
(use-package markdown-mode
  :mode
  ("\\.md\\'" . gfm-mode))

;;; 言語別設定

;; HTML
(add-hook 'html-mode-hook #'sgml-electric-tag-pair-mode)
(setq sgml-basic-offset 2)

;; CSS
(setq css-indent-offset 2)

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
      tab-width 2
      scroll-conservatively 35
      scroll-step 1
      ring-bell-function 'ignore
      eol-mnemonic-dos "(CRLF)"
      eol-mnemonic-mac "(CR)"
      eol-mnemonic-unix "(LF)")

(setq-default indent-tabs-mode nil)

(defalias 'yes-or-no-p 'y-or-n-p)

;; カーソル移動・履歴
(setq set-mark-command-repeat-pop t
      mark-ring-max 32
      global-mark-ring-max 64)

;;; UI 設定

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

;;; テーマ

(load-theme 'deeper-blue t)

;;; 自作関数（コマンド）

(defun my-kill-emacs ()
  "Confirm before exiting Emacs."
  (interactive)
  (if (y-or-n-p "Kill Emacs?")
      (save-buffers-kill-terminal)
    (message "")))

;;; 自作関数（設定・hook・advice 用）

(defun config-dired-setup ()
  "Custom keybindings for `dired-mode'."
  (define-key dired-mode-map (kbd "C-t") #'other-window))

(defun config-org-capture-finalize (old-func &rest args)
  "After org-capture-finalize, kill the buffer associated with the last captured entry."
  (apply old-func args)
  (when (ignore-errors (org-capture-goto-last-stored) t)
    (kill-buffer (current-buffer)))
  (message ""))

(defun config-lsp-format-on-save ()
  "Enable LSP-based formatting before saving the current buffer."
  (add-hook 'before-save-hook
            (lambda ()
              (when (bound-and-true-p lsp-mode)
                (lsp-format-buffer)))
            nil t))

;;; キーバインド

(global-set-key (kbd "C-t") #'other-window)
(global-set-key (kbd "C-z") #'undo-tree-undo)
(global-set-key (kbd "C-S-z") #'undo-tree-redo)
(global-set-key (kbd "C-_") #'undo-tree-undo)
(global-set-key (kbd "M-_") #'undo-tree-redo)
(global-set-key (kbd "M-n") #'scroll-up-line)
(global-set-key (kbd "M-p") #'scroll-down-line)
(global-set-key (kbd "M-]") #'forward-list)
(global-set-key (kbd "M-[") #'backward-list)
(global-set-key (kbd "C-x g") #'magit-status)
(global-set-key (kbd "C-x C-<down>") #'bury-buffer)
(global-set-key (kbd "C-x C-c") #'my-kill-emacs)
(global-set-key (kbd "C-c m") #'delete-duplicate-lines)
(global-set-key (kbd "C-c c") #'org-capture)
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c z") #'my-zettelhub-open-index)

;;; dired

(setq dired-kill-when-opening-new-dired-buffer t)

(add-hook 'dired-mode-hook #'config-dired-setup)

;;; org-mode

(setq org-directory "~/org"
      org-hide-leading-stars t
      org-startup-indented t
      org-startup-folded 'showall
      org-startup-with-inline-images t
      org-startup-truncated nil
      org-todo-keywords '((sequence "TODO(t)" "|" "DONE(d)")))

(with-eval-after-load 'org
  (setcdr (assoc 'file org-link-frame-setup) 'find-file))

(setq org-capture-templates
      `(("n" "Note" entry
         (file ,(expand-file-name "notes.org" org-directory))
         "* %?" :empty-lines 1 :kill-buffer 1)

        ("N" "Check Notes" plain
         (file ,(expand-file-name "notes.org" org-directory))
         nil :unnarrowed 1 :kill-buffer 1)

        ("t" "Task" entry
         (file ,(expand-file-name "tasks.org" org-directory))
         "* TODO %?" :kill-buffer 1)

        ("T" "Check Tasks" plain
         (file ,(expand-file-name "tasks.org" org-directory))
         nil :unnarrowed 1 :kill-buffer 1)))

(advice-add 'org-capture-finalize :around #'config-org-capture-finalize)

;;; init.el ends here
