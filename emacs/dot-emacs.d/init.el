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
(use-package better-defaults
  :config (scroll-bar-mode t))
(use-package zenburn-theme
  :config (load-theme 'zenburn t)
  (set-face-attribute 'region nil :background "#666" :foreground "#ffffff"))
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config (exec-path-from-shell-initialize)
  (add-to-list 'default-frame-alist '(alpha . 80))
  :custom (initial-frame-alist '((width . 165)(height . 45)(top . 50)(left . 50))))
(use-package flx-ido)
(use-package ido-completing-read+)
(use-package ido-vertical-mode)
(use-package ido-yes-or-no)
(use-package projectile
  :init (projectile-global-mode))
(use-package projectile-rails
  :init (projectile-rails-global-mode))
(use-package golden-ratio
  :config (golden-ratio-mode 1))
(use-package edit-server
  :init (edit-server-start)
  :custom (edit-server-new-frame nil))
(use-package company)
(use-package flycheck)
(use-package flycheck-color-mode-line)
(use-package smex)
(use-package yasnippet)
(use-package magit)
;; eglot
(use-package eglot
 :hook ((go-mode ruby-mode) . eglot-ensure))
;; minor modes
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
;; major modes
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

;; ido
(ido-mode 1)
(ido-everywhere 1)
(ido-ubiquitous-mode 1)
(flx-ido-mode 1)
(ido-yes-or-no-mode 1)
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)
;; disable ido faces to see flx highlights.
;; (setq ido-use-faces nil)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)
;; (setq flycheck-check-syntax-automatically '(mode-enabled save))
(eval-after-load "flycheck"
  '(progn
     (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)
     ))
;; M-n, M-p to `next/prev error`
(global-set-key (kbd "M-n") 'next-error)
(global-set-key (kbd "M-p") 'previous-error)

;; php :p
;;(require 'php-mode)
(autoload 'php-mode "php-mode" "Major mode for editing php code." t)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))

;; リージョンを色付きにする
(setq-default transient-mark-mode 1)
;; 時間を表示
(display-time-mode -1)
;; 列数表示
(column-number-mode 1)
; startup message disable
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

;; yasnippet
;; (add-to-list 'load-path "~/Dropbox/dotfiles/elisp/yasnippet/")
;; (require 'yasnippet) ;; not yasnippet-bundle
;; (yas-global-mode 1)
;; (global-set-key (kbd "C-M-;") 'yas/insert-snippet)
;;(setq yas/trigger-key (kbd "SPC"))
;;(setq yas/next-field-key (kbd "TAB"))
;; (require 'dropdown-list)
;; (setq yas/prompt-functions '(yas/dropdown-prompt))
;; (require 'anything-c-yasnippet)
;; (setq anything-c-yas-space-match-any-greedy t) ;スペース区切りで絞り込めるようにする デフォルトは nil
;; (global-set-key (kbd "M-SPC") 'anything-c-yas-complete) ;C-c yで起動 (同時にお使いのマイナーモードとキーバインドがかぶるかもしれません)
;; (yas/initialize)
;; (yas/load-directory "~/Dropbox/dotfiles/elisp/yasnippet/snippets")

;; company
(add-hook 'after-init-hook 'global-company-mode)
(with-eval-after-load 'company
  (setq company-auto-expand t) ;; 1個目を自動的に補完
  (setq company-transformers '(company-sort-by-backend-importance)) ;; ソート順
  (setq company-idle-delay 0) ; 遅延なしにすぐ表示
  (setq company-minimum-prefix-length 2) ; デフォルトは4
  (setq company-selection-wrap-around t) ; 候補の最後の次は先頭に戻る
  (setq completion-ignore-case t)
  (setq company-dabbrev-downcase nil)
  (global-set-key (kbd "C-M-i") 'company-complete)
  ;; C-n, C-pで補完候補を次/前の候補を選択
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map [tab] 'company-complete-selection) ;; TABで候補を設定
  (define-key company-active-map (kbd "C-g") 'company-abort)
  )

;; modes

;; js/jsx
;; https://github.com/ananthakumaran/tide/
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  ;; configure javascript-tide checker to run after your default javascript checker
  (flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append)
  ;; configure jsx-tide checker to run after your default jsx checker
  (flycheck-add-next-checker 'javascript-eslint 'jsx-tide 'append)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
;; (add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)
(setq-default typescript-indent-level 2)
;; enable typescript-tslint checker
(with-eval-after-load 'flycheck
  (flycheck-add-mode 'typescript-tslint 'web-mode)
  (flycheck-add-mode 'javascript-eslint 'web-mode))

;; tsx
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))
;; js/jsx
(add-to-list 'auto-mode-alist '("\\.js[x]?\\'" . rjsx-mode))
(add-hook 'rjsx-mode-hook #'setup-tide-mode)

;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
              '(javascript-jshint))

;; customize flycheck temp file prefix
(setq-default flycheck-temp-prefix ".flycheck")

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

;;javascript-mode to json
(add-to-list 'auto-mode-alist '("\\.json$" . javascript-mode))
(setq js-indent-level 2)

;; remove whitespace at the last of the line on save.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; 自動改行の調整
(setq truncate-partial-width-windows nil)
