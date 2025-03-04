(setq
 is-gui     (display-graphic-p)
 on-mac     (eq system-type 'darwin)
 on-linux   (eq system-type 'gnu/linux)
 on-windows (eq system-type 'windows-nt))

(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

(load-theme 'tango-dark t)

(cond
 (on-linux (add-to-list 'default-frame-alist '(font . "Monospace-14")))
 (on-mac (add-to-list 'default-frame-alist '(font . "Monaco 18")))
 (on-windows (add-to-list 'default-frame-alist '(font . "Consolas 16"))))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

(global-set-key (kbd "C-c n") 'display-line-numbers-mode)
(global-set-key (kbd "C-c w") 'whitespace-mode)

(defun sort-lines-nocase ()
  (interactive)
  (let ((sort-fold-case t))
    (call-interactively 'sort-lines)))

(require 'evil)
(setq evil-emacs-state-modes
      (append '(haskell-interactive-mode) evil-emacs-state-modes))
(setq evil-lookup-func #'man)
(evil-define-motion evil-lookup ()
  (call-interactively evil-lookup-func))
(evil-mode 1)

(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)(define-key evil-normal-state-map [escape] 'keyboard-quit)

(require 'evil-search-highlight-persist)
(global-evil-search-highlight-persist t)

(require 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
  "/" 'evil-search-highlight-persist-remove-all
  "a" 'pop-tag-mark
  "n" 'evil-next-buffer
  "p" 'evil-prev-buffer
  "b" 'switch-to-buffer
  "s" 'save-buffer
  "c" 'smart-compile
  "d" 'kill-buffer
  "e" 'next-error
  "o" 'other-frame
  "f" 'find-file
  "g" 'dired
  "h" 'google-this
  "j" 'scroll-up-command
  "k" 'scroll-down-command
  "l" 'display-line-numbers-mode
  "r" 'find-grep
  "t" 'delete-trailing-whitespace
  "w" 'whitespace-mode)
(evil-leader/set-key-for-mode 'haskell-mode
  "i" 'haskell-process-do-info
  "l" 'haskell-process-load-file
  "t" 'haskell-process-do-type
  "v" 'haskell-interactive-switch)

(require 'evil-surround)
(global-evil-surround-mode 1)

(require 'auto-complete)
(global-auto-complete-mode t)

(require 'smart-compile)
(setq smart-compile-alist
      (append
       '(("\\.c\\'"          . "cc -Wall -O2 %f -o %n && ./%n")
         ("\\.[Cc]+[Pp]+\\'" . "c++ -std=c++14 -Wall -O2 %f -o %n && ./%n")
         ("\\.cs\\'"         . "mcs %f && mono %n.exe")
         ("\\.go\\'"         . "go build %f && ./%n")
         ("\\.hs\\'"         . "stack ghc -- -O2 %f && ./%n")
         ("\\.js\\'"         . "node %f")
         ("\\.lua\\'"        . "lua %f")
         ("\\.py\\'"         . "python3 %f")
         ("\\.tex\\'"        . "xelatex -shell-escape %f"))
       smart-compile-alist))

(setq auto-mode-alist
      (append
       '(("\\.cl\\'" . c-mode)
         ("\\.glsl.*\\'" . glsl-mode)
         ("\\.hlsl.*\\'" . glsl-mode)
         ("\\.cg.*\\'" . glsl-mode)
         ("\\.shader.*\\'" . glsl-mode)
         ("\\.sc\\'" . python-mode))
       auto-mode-alist))

(ido-mode 1)
(savehist-mode 1)
(if (not (and is-gui on-mac)) (menu-bar-mode 0))
(tool-bar-mode 0)
(if is-gui (scroll-bar-mode 0))
(show-paren-mode 1)
(which-function-mode 1)
(line-number-mode 1)
(column-number-mode 1)
(dynamic-completion-mode 1)
(auto-compression-mode 1)
(global-display-line-numbers-mode 1)
(global-hl-line-mode 1)
(global-auto-revert-mode 1)

(fset 'yes-or-no-p 'y-or-n-p)
(setq
 confirm-nonexistent-file-or-buffer nil
 inhibit-startup-message t
 visible-bell nil
 ring-bell-function 'ignore
 scroll-step 1
 make-backup-files nil
 windmove-wrap-around t
 parens-require-spaces nil
 ido-enable-flex-matching t
 ido-create-new-buffer 'always)

(if on-mac
  (setq shell-file-name "zsh"
        explicit-shell-file-name "zsh"))
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function kill-buffer-query-functions))

(setq-default
 tab-width 8
 fill-column 80
 default-fill-column 80
 indent-tabs-mode nil
 show-trailing-whitespace t
 scroll-preserve-screen-position t
 lua-indent-level 2
 js-indent-level 2)

(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'go-mode-hook
 (lambda ()
   (add-hook 'before-save-hook 'gofmt-before-save)
   (setq-default)
   (setq tab-width 2)
   (setq standard-indent 2)
   (setq indent-tabs-mode t)))

(defadvice c-lineup-arglist (around my activate)
  "Improve indentation of continued C++11 lambda function opened as argument."
  (setq ad-return-value
        (if (and (equal major-mode 'c++-mode)
                 (ignore-errors
                   (save-excursion
                     (goto-char (c-langelem-pos langelem))
                     ;; Detect "[...](" or "[...]{". preceded by "," or "(",
                     ;;   and with unclosed brace.
                     (looking-at ".*[(,][ \t]*\\[[^]]*\\][ \t]*[({][^}]*$"))))
            0                           ; no additional indent
          ad-do-it)))                   ; default behavior

(defun linux-set-c-style ()
  (c-set-style "linux")
  (setq indent-tabs-mode t))
(add-hook 'c-mode-hook 'linux-set-c-style)

(require 'google-c-style)
(add-hook 'c++-mode-hook 'google-set-c-style)

;; (load-file
;;  (let ((coding-system-for-read 'utf-8))
;;    (shell-command-to-string "agda-mode locate")))

;; (load "~/.emacs.d/lisp/PG/generic/proof-site")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(asn1-mode auto-complete caml cmake-mode csharp-mode csproj-mode
               dockerfile-mode evil evil-leader evil-search-highlight-persist
               evil-surround glsl-mode go-mode google-c-style google-this
               haskell-mode idris-mode lean-mode lua-mode php-mode powershell
               protobuf-mode qml-mode qt-pro-mode rust-mode smart-compile tuareg
               typescript-mode yaml-mode zig-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
