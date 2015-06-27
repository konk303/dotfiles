;; ;; load path
;; (setq load-path
;;       (append
;;        (list
;;         (expand-file-name "~/.emacs.d/elisp")
;;         )
;;        load-path))

(require 'cask "/usr/local/share/emacs/site-lisp/cask.el")
(cask-initialize)

;;backup file location ~/bakに
(setq make-backup-files t)
(setq backup-directory-alist
      (cons (cons "\\.*$" (expand-file-name "~/bak"))
            backup-directory-alist))

;; starting to mess with 23
(setq line-move-visual 'nil)
(setq ns-pop-up-frames nil)


(setq ns-command-modifier (quote meta))
(setq ns-alternate-modifier (quote super))
(setq mac-allow-anti-aliasing t)
(setq yank-excluded-properties t)

;; not working
(when (functionp 'mac-auto-ascii-mode)
  (mac-auto-ascii-mode 1))

;;emacs version/system oriented
(if window-system
    (progn
      (add-to-list 'default-frame-alist '(alpha . (85 85)))
      (setq initial-frame-alist '((width . 180)(height . 65)(top . 30)(left . 50)))
      (load-theme 'wombat t)
      (cond
       ;; carbon emacs
       ((featurep 'carbon-emacs-package)
        (setq mac-option-modifier 'meta)
        (setq yank-excluded-properties t)
        (require 'display-buffer-for-wide-screen)
        )
       ;; cocoa emacs
       ((eq window-system 'ns)
        (setq mac-command-modifier 'meta)
        (setq mac-allow-anti-aliasing t)
        ;; (load "emacs23mac_font.el")
        ))
      ))
;;tabs, indent
(setq standard-indent 2)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
;; ruby
(require 'ruby-mode)
(setq ruby-deep-indent-paren-style nil)
;; magickコメントを入れない
(defun ruby-mode-set-encoding () ())

;; php :p
;;(require 'php-mode)
(autoload 'php-mode "php-mode" "Major mode for editing php code." t)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))

;; C-h でカーソルの左にある文字を消す
(define-key global-map "\C-h" 'delete-backward-char)
;; C-h に割り当てられている関数 help-command を C-x C-h に割り当てる
(define-key global-map "\C-x\C-h" 'help-command)

;; 括弧の対応をハイライト.
(show-paren-mode 1)
;; リージョンを色付きにする
(setq-default transient-mark-mode 1)
;; 時間を表示
(display-time-mode 1)
;; 列数表示
(column-number-mode 1)
; startup message disable
(setq inhibit-startup-message t)
;; メニューバーを消す
(menu-bar-mode -1)
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
;; (setq hl-line-face 'hl-line)
;; (global-hl-line-mode t)
;; 行番号
(global-linum-mode t)

;; extensions
;; package
;; (require 'package)
;; (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
;; (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
;; (package-initialize)
;; auto-install
;; (require 'auto-install)
;; (setq auto-install-directory "~/.emacs.d/auto-install/")

;;http://blog.clouder.jp/archives/000673.html
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq scroll-step 1)
(setq read-file-name-completion-ignore-case nil)

;; https://github.com/roman/golden-ratio.el
(golden-ratio-mode 1)
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

;; anything -> helm
(require 'helm-config)
;; (add-to-list 'load-path "~/Dropbox/dotfiles/elisp/anything-config/")
;;(require 'anything-startup)
;; (require 'anything-config)
(global-set-key (kbd "M-x") 'helm-M-x)
(setq recentf-max-saved-items 500)

;; yasnippet
;; (add-to-list 'load-path "~/Dropbox/dotfiles/elisp/yasnippet/")
(require 'yasnippet) ;; not yasnippet-bundle
(yas-global-mode 1)
(global-set-key (kbd "C-M-;") 'yas/insert-snippet)
;;(setq yas/trigger-key (kbd "SPC"))
;;(setq yas/next-field-key (kbd "TAB"))
;; (require 'dropdown-list)
;; (setq yas/prompt-functions '(yas/dropdown-prompt))
;; (require 'anything-c-yasnippet)
;; (setq anything-c-yas-space-match-any-greedy t) ;スペース区切りで絞り込めるようにする デフォルトは nil
;; (global-set-key (kbd "M-SPC") 'anything-c-yas-complete) ;C-c yで起動 (同時にお使いのマイナーモードとキーバインドがかぶるかもしれません)
;; (yas/initialize)
;; (yas/load-directory "~/Dropbox/dotfiles/elisp/yasnippet/snippets")

;; auto-complete
(require 'auto-complete-config)
(ac-config-default)
;; (add-to-list 'ac-dictionary-directories "~/Dropbox/dotfiles/elisp/ac-dict")
(define-key ac-complete-mode-map (kbd "C-n") 'ac-next)
(define-key ac-complete-mode-map (kbd "C-p") 'ac-previous)
(define-key ac-complete-mode-map (kbd "C-g") 'ac-stop)
(define-key ac-complete-mode-map "\t" 'ac-complete)
(define-key ac-complete-mode-map "\n" 'ac-complete)

;; (defun my-anything ()
;;   (interactive)
;;   (anything-other-buffer
;;    '(anything-c-source-buffers+
;;      anything-c-source-files-in-current-dir+
;;      anything-c-source-recentf
;;      anything-c-source-kill-ring
;;      anything-c-source-yasnippet
;;      anything-c-source-find-files
;;      anything-c-source-file-name-history
;;      anything-c-source-complex-command-history
;;      anything-c-source-emacs-commands
;;      anything-c-source-google-suggest
;; ;;     anything-c-source-info-pages
;; ;;      anything-c-source-info-elisp
;; ;;      anything-c-source-man-pages
;; ;;     anything-c-source-locate
;;      )
;;    " *my-anything*"))
;; (global-set-key (kbd "C-;") 'my-anything)
;; (defun my-mark-rings ()
;;   (interactive)
;;   (anything-other-buffer
;;    '(
;;      anything-c-source-global-mark-ring
;;      anything-c-source-mark-ring
;;      anything-c-source-kill-ring
;;      anything-c-source-yasnippet
;;      )
;;    " *my-mark-rings*"))
;; (global-set-key (kbd "M-'") 'my-mark-rings)



;; modes
;; erb
(add-to-list 'auto-mode-alist '("\\.erb$" . web-mode))

;; feature
(add-to-list 'auto-mode-alist '("\\.feature$" . feature-mode))

;; scss
(add-to-list 'auto-mode-alist '("\\.scss$" . scss-mode))
(defun my-scss-mode-hook ()
  "Hooks for SASS mode."
  (setq-default scss-compile-at-save nil)
  ;; (setq-default scss-output-directory "/dev/shm")
  ;; (flymake-mode-on)
)
(add-hook 'scss-mode-hook 'my-scss-mode-hook)

;;yaml-mode
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

;;action script mode
(require 'actionscript-mode nil 't)
(add-to-list 'auto-mode-alist '("\\.as$" . actionscript-mode))

;;csv-mode
(require 'csv-mode nil 't)
(add-to-list 'auto-mode-alist '("\\.csv$" . csv-mode))

;;javascript-mode to json
(add-to-list 'auto-mode-alist '("\\.json$" . javascript-mode))


;;ruby-mode
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rash$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))


;; rspec-mode
;; (require 'rspec-mode)

;; Rinari
;; (add-to-list 'load-path "~/Dropbox/dotfiles/elisp/rinari")
;; (require 'rinari)
;;; rhtml-mode
;; (add-to-list 'load-path "~/Dropbox/dotfiles/elisp/rhtml")
;; (require 'rhtml-mode)
;; (add-hook 'rhtml-mode-hook
;;           (lambda () (rinari-launch)))
;; flymake for ruby
;; (load "flymakeruby")
;; auto indent
(add-hook 'ruby-mode-hook '(lambda ()
                             (local-set-key (kbd "RET") 'newline-and-indent)
                             ))

;; flycheck
(setq flycheck-check-syntax-automatically '(mode-enabled save))
(add-hook 'ruby-mode-hook 'flycheck-mode)

;; remove whitespace at the last of the line on save.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;(add-hook 'ruby-mode-hook '(lambda () (add-hook 'write-contents-hooks '(lambda () (indent-region (point-min) (point-max))))))
;; ruby-electric.el --- electric editing commands for ruby files
;; (require 'ruby-electric)
;; (add-hook 'ruby-mode-hook '(lambda () (ruby-electric-mode t)))
;;haml-mode
(require 'haml-mode nil 't)
(add-to-list 'auto-mode-alist '("\\.haml$" . haml-mode))
;;sass-mode
(require 'sass-mode nil 't)
(add-to-list 'auto-mode-alist '("\\.sass$" . sass-mode))

;;rails
;; (defun try-complete-abbrev (old)
;;   (if (expand-abbrev) t nil))

;; (setq hippie-expand-try-functions-list
;;       '(try-complete-abbrev
;;         try-complete-file-name
;;         try-expand-dabbrev))
;; (setq rails-use-mongrel t)
;; (require 'rails)

;; chrome edit with emacs
;; http://www.emacswiki.org/emacs/Edit_with_Emacs
(require 'edit-server)
(setq edit-server-new-frame nil)
(edit-server-start)

;====================================
;;全角スペースとかに色を付ける
;====================================
(defface my-face-b-1 '((t (:background "medium aquamarine"))) nil)
(defface my-face-b-1 '((t (:background "dark turquoise"))) nil)
;(defface my-face-b-2 '((t (:background "cyan"))) nil)
(defface my-face-b-2 '((t (:background "DimGray"))) nil)
(defface my-face-b-2 '((t (:background "SeaGreen"))) nil)
(defface my-face-u-1 '((t (:foreground "SteelBlue" :underline t))) nil)
(defvar my-face-b-1 'my-face-b-1)
(defvar my-face-b-2 'my-face-b-2)
(defvar my-face-u-1 'my-face-u-1)
(defadvice font-lock-mode (before my-font-lock-mode ())
  (font-lock-add-keywords
   major-mode
   '(
     ("　" 0 my-face-b-1 append)
     ("\t" 0 my-face-b-2 append)
     ("[ ]+$" 0 my-face-u-1 append)
     )))
(ad-enable-advice 'font-lock-mode 'before 'my-font-lock-mode)
(ad-activate 'font-lock-mode)
(add-hook 'find-file-hooks '(lambda ()
                              (if font-lock-mode
                                  nil
                                (font-lock-mode t))))

;; (add-to-list 'default-frame-alist '(font . "ricty-13.5"))
;; italic japanese fonts hack
;; http://gongo.hatenablog.com/entry/2011/12/08/232953
;; 日本語コメント
(when (x-list-fonts "Ricty")
  (let* ((size 13)
         (asciifont "Ricty")
         (jpfont "Ricty")
         (h (* size 11))
         (fontspec)
         (jp-fontspec))
    (set-face-attribute 'default nil :family asciifont :height h)
    (setq fontspec (font-spec :family asciifont))
    (setq jp-fontspec (font-spec :family jpfont))
    (set-fontset-font nil 'japanese-jisx0208 jp-fontspec)
    (set-fontset-font nil 'japanese-jisx0212 jp-fontspec)
    (set-fontset-font nil 'japanese-jisx0213-1 jp-fontspec)
    (set-fontset-font nil 'japanese-jisx0213-2 jp-fontspec)
    (set-fontset-font nil '(#x0080 . #x024F) fontspec)
    (set-fontset-font nil '(#x0370 . #x03FF) fontspec)))

;; 自動改行の調整
(setq truncate-partial-width-windows nil)