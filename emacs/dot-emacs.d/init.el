;; https://github.com/radian-software/straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
;; Configure use-package to use straight.el by default
(use-package straight
  :custom (straight-use-package-by-default t))
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-initialize)
  (add-to-list 'default-frame-alist '(alpha . 90))
  :custom (initial-frame-alist '((width . 165)(height . 45)(top . 50)(left . 50))))
(use-package zenburn-theme
  :config (load-theme 'zenburn t)
  (set-face-attribute 'region nil :background "#666" :foreground "#ffffff"))
(use-package better-defaults
  :config (scroll-bar-mode t)(ido-mode 0))
(use-package edit-server
  :init (edit-server-start)
  :custom (edit-server-new-frame nil))
;; corfu
(use-package corfu
  :straight (:includes corfu-history-mode)
  :init (global-corfu-mode)
  :custom ((corfu-auto t)
           (corfu-cycle t)
           (corfu-quit-no-match t)))
  ;; :bind (:map corfu-map
  ;;             ("TAB" . nil)
  ;;             ("\t" . nil)))
(use-package cape
  :init (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-history)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-tex)
  (add-to-list 'completion-at-point-functions #'cape-sgml)
  (add-to-list 'completion-at-point-functions #'cape-rfc1345)
  (add-to-list 'completion-at-point-functions #'cape-abbrev)
  (add-to-list 'completion-at-point-functions #'cape-dict)
  ;; (add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  ;; (add-to-list 'completion-at-point-functions #'cape-line)
  (add-to-list 'completion-at-point-functions #'tempel-complete))
(use-package tempel
  :bind (("M-+" . tempel-complete)
         ("M-*" . tempel-insert)))
(use-package tempel-collection)
(use-package orderless
  :custom (completion-styles '(orderless basic)))
(use-package nerd-icons-corfu
  :after corfu
  :config (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))
;; vertico/consult
(use-package consult)
(use-package vertico
  :straight (:includes vertico-directory)
  :init (vertico-mode)
  :custom ((vertico-count 20)
           (vertico-cycle t)
           (read-extended-command-predicate #'command-completion-default-include-p))
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-word)))
(use-package marginalia
  :init (marginalia-mode))
(use-package recentf
  :init (recentf-mode 1))
;; eglot
;; (use-package eglot
;;   :init (add-to-list 'eglot-server-programs '((ruby-mode ruby-ts-mode) . "ruby-lsp"))
;; (add-hook 'ruby-mode-hook 'eglot-ensure)
;;   :hook (prog-mode . eglot-ensure))
;; lsp-mode
(use-package lsp-mode
  :hook (prog-mode . lsp)
  :config
  (add-to-list 'lsp-language-id-configuration '(".*\\.html\\.erb$" . "ruby")))
;; tree-sitter
(use-package treesit-auto
  :config
  (setq treesit-auto-install t)
  (setq treesit-font-lock-level 4)
  (global-treesit-auto-mode))
;; tramp
(use-package tramp
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (add-to-list 'tramp-remote-path "/home/vscode/.rbenv/shims")
  )
;; flycheck
(use-package flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
;; git
(use-package magit)
;; docker
(use-package docker
  :ensure t
  :bind ("C-c d" . docker))
;; copilot
(use-package copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-mode-map
              ("C-<tab>" . copilot-accept-completion)))
;; minor modes
(use-package golden-ratio
  :config (golden-ratio-mode 1))
(use-package highlight-indent-guides
  :hook ((text-mode prog-mode) . highlight-indent-guides-mode)
  :custom ((highlight-indent-guides-method 'character)
           (highlight-indent-guides-character ?|)
           (highlight-indent-guides-auto-character-face-perc 70)
           (highlight-indent-guides-responsive 'top)
           (highlight-indent-guides-auto-top-character-face-perc 100)))
(use-package mwim
  :bind (("C-a" . mwim-beginning)
         ("C-e" . mwim-end)))
(use-package rainbow-delimiters
  :hook ((text-mode prog-mode) . rainbow-delimiters-mode))
(use-package smartparens
  :disabled
  :hook ((text-mode prog-mode) . smartparens-mode)(after-init . smartparens-global-strict-mode)
  :config (require 'smartparens-config))
(use-package whitespace
  :config (global-whitespace-mode t)
  :custom (whitespace-style '(tab-mark)))
(use-package ag)
(use-package which-key
  :config (which-key-mode))
;; major modes
(use-package markdown-mode
  :mode ("\\.md\\'" . gfm-mode))
(use-package terraform-mode)
(use-package elixir-mode)
(use-package go-mode
  :hook (before-save . gofmt-before-save))
(use-package haskell-mode)
(use-package org
  :bind (:map org-mode-map ("C-'" . nil))
  :straight (:type built-in))
(use-package ruby-mode)
(use-package slim-mode)
(use-package web-mode
  :mode "\\.erb\\'")
(use-package yaml-mode)
(use-package yard-mode
  :hook ruby-mode)
(use-package javascript-mode)
(use-package typescript-mode)
(use-package kotlin-mode)
(use-package php-mode
  :mode "\\.php\\'")
(use-package dockerfile-mode)



;; messing with ime
(setq default-input-method "MacOSX")
;; (mac-set-input-method-parameter "com.google.inputmethod.Japanese.base" `title "あ")

(add-to-list 'default-frame-alist '(font . "HackGen-16"))

;; starting to mess with 23
(setq line-move-visual 'nil)
(setq ns-pop-up-frames nil)


(setq ns-command-modifier 'meta)
(setq ns-alternate-modifier 'super)
(setq mac-allow-anti-aliasing t)
(setq yank-excluded-properties t)

;; not working
(when (functionp 'mac-auto-ascii-mode)
  (mac-auto-ascii-mode 1))

;;tabs, indent
(setq standard-indent 2)
(setq-default tab-width 2)

;; M-n, M-p to `next/prev error`
(global-set-key (kbd "M-n") 'next-error)
(global-set-key (kbd "M-p") 'previous-error)

;; リージョンを色付きにする
(setq-default transient-mark-mode 1)
;; 時間を表示
(display-time-mode -1)
;; 列数表示
(column-number-mode 1)
;; startup message disable
(setq inhibit-startup-message t)
;; BS で選択範囲を消す
(delete-selection-mode 1)
;; バッファの最後で next-line しても新しい行を挿入しない
(setq next-line-add-newlines nil)
;; window タイトル
(setq frame-title-format (format "%%f - Emacs@%s" (system-name)))
;; auto-indent on return
(global-set-key (kbd "RET") 'newline-and-indent)
;; C-jでうまくインデントが動かないので強引に
(global-set-key (kbd "C-j") 'newline-and-indent)
(electric-indent-mode 1)
;; C-o に動的略語展開機能を割り当てる
(define-key global-map "\C-o" 'dabbrev-expand)
;; C-; にC-x b(switch-to-buffer)を割り当てる
(define-key global-map (kbd "C-;") 'switch-to-buffer)

(setq dabbrev-case-fold-search nil) ; 大文字小文字を区別
;; 現在行強調表示
(setq hl-line-face 'hl-line)
(global-hl-line-mode t)
;; 行番号
(global-display-line-numbers-mode t)

(setq scroll-step 1)
(setq read-file-name-completion-ignore-case nil)

;;http://d.hatena.ne.jp/rubikitch/20100210/emacs
(defun other-window-or-split ()
  (interactive)
  (when (one-window-p)
    (split-window-horizontally))
  (other-window 1))
(global-set-key (kbd "C-'") 'other-window-or-split)
;; emacsclient
(require 'server)
(unless (server-running-p)
  (server-start))
(defun iconify-emacs-when-server-is-done ()
  (unless server-clients (iconify-frame)))
;; iconify when server ends
(add-hook 'server-done-hook
          (lambda ()
            (do-applescript "tell application \"Terminal\"
                                activate
                             end")))
;; C-x C-c to server end
(global-set-key (kbd "C-x C-c") 'server-edit)
;; M-x exit to end emacs
(defalias 'end 'save-buffer-kill-emacs)

;; bindings
;;(global-set-key (kbd "C-c C-c") 'comment-dwim)
(global-set-key (kbd "M-c") 'kill-ring-save)

;; feature
(add-to-list 'auto-mode-alist '("\\.feature$" . feature-mode))

;; css
(setq css-indent-offset 2)
;; scss
(add-to-list 'auto-mode-alist '("\\.scss$" . scss-mode))
(defun my-scss-mode-hook ()
  "Hooks for SASS mode."
  (setq-default scss-compile-at-save nil)
  ;; (setq-default scss-output-directory "/dev/shm")
  ;; (flymake-mode-on)
  )
(add-hook 'scss-mode-hook 'my-scss-mode-hook)

;;csv-mode
(require 'csv-mode nil 't)
(add-to-list 'auto-mode-alist '("\\.csv$" . csv-mode))

;; remove whitespace at the last of the line on save.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; 自動改行の調整
(setq truncate-partial-width-windows nil)
