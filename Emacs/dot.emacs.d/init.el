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

;;; 基本設定

(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)

(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file t)

(setq inhibit-startup-message t
      make-backup-files nil
      delete-auto-save-files t
      tab-width 2
      indent-tabs-mode nil
      scroll-conservatively 35
      scroll-step 1
      ring-bell-function 'ignore
      eol-mnemonic-dos "(CRLF)"
      eol-mnemonic-mac "(CR)"
      eol-mnemonic-unix "(LF)")

(defalias 'yes-or-no-p 'y-or-n-p)

;;; UI 設定

(delete-selection-mode 1)
(show-paren-mode 1)
(column-number-mode 1)
(electric-pair-mode 1)
(global-display-line-numbers-mode 1)

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

(defun my-zettelhub-open-index ()
  "Open the main Zettelhub index.org."
  (interactive)
  (find-file my-zettelhub-index-file))

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

;;; キーバインド

(global-set-key (kbd "C-t") #'other-window)
(global-set-key (kbd "C-z") #'undo-only)
(global-set-key (kbd "C-S-z") #'undo-redo)
(global-set-key (kbd "C-c m") #'delete-duplicate-lines)
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c c") #'org-capture)
(global-set-key (kbd "M-n") #'scroll-up-line)
(global-set-key (kbd "M-p") #'scroll-down-line)
(global-set-key (kbd "M-[") #'backward-list)
(global-set-key (kbd "M-]") #'forward-list)
(global-set-key (kbd "C-x C-c") #'my-kill-emacs)
(global-set-key (kbd "C-c z") #'my-zettelhub-open-index)

;;; dired

(setq dired-kill-when-opening-new-dired-buffer t)

(add-hook 'dired-mode-hook #'config-dired-setup)

;;; org-mode

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

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
         (file+datetree ,(expand-file-name "tasks.org" org-directory))
         "* TODO %? %T" :kill-buffer 1)

        ("T" "Check Tasks" plain
         (file+datetree ,(expand-file-name "tasks.org" org-directory))
         nil :unnarrowed 1 :kill-buffer 1)))

(advice-add 'org-capture-finalize :around #'config-org-capture-finalize)

;;; zettelhub

(defvar my-zettelhub-directory
  (expand-file-name "zettelhub" org-directory)
  "Directory for Zettelhub notes.")

(defvar my-zettelhub-index-file
  (expand-file-name "index.org" my-zettelhub-directory)
  "Main index file for Zettelhub.")

;;; init.el ends here
