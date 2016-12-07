(require 'cask)
(cask-initialize)
(pallet-mode t)

;; programming modes settings
;; rainbow delimiters
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; messing with ime
(setq default-input-method "MacOSX")
(mac-set-input-method-parameter "com.google.inputmethod.Japanese.base" `title "あ")

(add-to-list 'default-frame-alist '(font . "Ricty Diminished-15"))

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
      (add-to-list 'default-frame-alist '(alpha . 85))
      (setq initial-frame-alist '((width . 160)(height . 55)(top . 50)(left . 50)))
      ;; (load-theme 'solarized-dark t)
      (load-theme 'zenburn t)
      (set-face-attribute 'region nil :background "#666" :foreground "#ffffff")
      ;; (load-theme 'wombat t)
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
        (setq exec-path-from-shell-check-startup-files nil)
        (exec-path-from-shell-initialize)
        ;; (load "emacs23mac_font.el")
        ))
      ))
;;tabs, indent
(setq standard-indent 2)
(setq-default tab-width 2)

;; flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)
;; (setq flycheck-check-syntax-automatically '(mode-enabled save))
(eval-after-load "flycheck"
  '(progn
     (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)
     ;; use eslint with web-mode for jsx files
     (flycheck-add-mode 'javascript-eslint 'web-mode)
     (flycheck-add-mode 'javascript-eslint 'js2-jsx-mode)
     ))
;; M-n, M-p to `next/prev error`
(global-set-key (kbd "M-n") 'next-error)
(global-set-key (kbd "M-p") 'previous-error)


;; php :p
;;(require 'php-mode)
(autoload 'php-mode "php-mode" "Major mode for editing php code." t)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))

;; C-h でカーソルの左にある文字を消す
(define-key global-map "\C-h" 'delete-backward-char)
;; C-h に割り当てられている関数 help-command を C-x C-h に割り当てる
(define-key global-map "\C-x\C-h" 'help-command)

;; リージョンを色付きにする
(setq-default transient-mark-mode 1)
;; 時間を表示
(display-time-mode 1)
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
;; (setq hl-line-face 'hl-line)
;; (global-hl-line-mode t)
;; 行番号
(global-linum-mode t)

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
;; (require 'helm-config)
;; (add-to-list 'load-path "~/Dropbox/dotfiles/elisp/anything-config/")
;;(require 'anything-startup)
;; (require 'anything-config)
;; (global-set-key (kbd "M-x") 'helm-M-x)
;; (setq recentf-max-saved-items 500)

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

;; html
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))

;; js/jsx
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-jsx-mode))
(add-hook 'js2-jsx-mode-hook (lambda () (setq js2-basic-offset 2)))
;; (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
              '(javascript-jshint))
;; customize flycheck temp file prefix
(setq-default flycheck-temp-prefix ".flycheck")

;; erb
(add-to-list 'auto-mode-alist '("\\.erb$" . web-mode))

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
(setq js-indent-level 2)

;;ruby-mode
;; magickコメントを入れない
(setq ruby-encoding-magic-comment-style nil)
;; `C-c C-s` to r c instead of pry
(eval-after-load 'inf-ruby
  '(define-key inf-ruby-minor-mode-map
     (kbd "C-c C-s") 'inf-ruby-console-auto))
(add-hook 'ruby-mode-hook 'yard-mode)

;; remove whitespace at the last of the line on save.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;haml-mode
(require 'haml-mode nil 't)
(add-to-list 'auto-mode-alist '("\\.haml$" . haml-mode))
;;sass-mode
(require 'sass-mode nil 't)
(add-to-list 'auto-mode-alist '("\\.sass$" . sass-mode))

;; geiser, for scheme(racket).
(setq geiser-racket-binary "/usr/local/bin/racket")
(setq geiser-active-implementations '(racket))

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

;; 自動改行の調整
(setq truncate-partial-width-windows nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.

 ;; Ignore prefiexes and use binstub version
 '(rspec-spec-command "bin/rspec")
 '(rspec-use-bundler-when-possible nil)
 '(rspec-use-rake-when-possible nil)
 '(rspec-use-spring-when-possible nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(add-hook 'after-init-hook 'inf-ruby-switch-setup)
