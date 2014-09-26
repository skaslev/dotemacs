(setq on-mac     (eq 'darwin system-type))
(setq on-linux   (eq 'gnu/linux system-type))
(setq on-windows (eq 'windows-nt system-type))

(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

(load-theme 'busybee t)
(cond
 (on-mac (set-default-font "Monaco 18"))
 (on-linux (set-default-font "Bitstream Vera Sans Mono-14")))
(setq initial-frame-alist '((fullscreen . maximized)))

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

(global-set-key (kbd "C-c n") 'linum-mode)
(global-set-key (kbd "C-c w") 'whitespace-mode)

(require 'evil)
(setq evil-emacs-state-modes
      (append
       '(haskell-interactive-mode)
       evil-emacs-state-modes))
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
  "f" 'find-file
  "g" 'dired
  "b" 'switch-to-buffer
  "d" 'kill-buffer
  "j" 'scroll-up-command
  "k" 'scroll-down-command
  "n" 'linum-mode
  "w" 'whitespace-mode
  "c" 'smart-compile
  "e" 'next-error
  "/" 'evil-search-highlight-persist-remove-all)
(evil-leader/set-key-for-mode 'haskell-mode
  "l" 'haskell-process-load-file
  "t" 'haskell-process-do-type
  "i" 'haskell-process-do-info
  "v" 'haskell-interactive-switch)

(require 'evil-surround)
(global-evil-surround-mode 1)

(require 'p4)
(setenv "P4CONFIG" ".p4config")

(require 'smart-compile)
(setq smart-compile-alist
      (append
       '(("\\.c\\'"          . "clang -Wall -O2 %f -o %n && ./%n")
         ("\\.[Cc]+[Pp]*\\'" . "clang++ -std=c++11 -Wall -O2 %f -o %n && ./%n")
         ("\\.hs\\'"         . "ghc -O2 %f && ./%n")
         ("\\.js\\'"         . "node %f")
         ("\\.lua\\'"        . "lua %f")
         ("\\.py\\'"         . "python %f")
         ("\\.go\\'"         . "go build %f && ./%n"))
       smart-compile-alist))

(setq auto-mode-alist
      (append '(("\\.cl\\'" . c-mode)
                ("\\.h\\'"  . c++-mode)
                ("\\.sc\\'" . python-mode))
              auto-mode-alist))

(ido-mode 1)
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(show-paren-mode 1)
(which-func-mode 1)
(line-number-mode 1)
(column-number-mode 1)
(dynamic-completion-mode 1)
(auto-compression-mode 1)
(global-linum-mode 1)
(global-auto-revert-mode 1)
(global-auto-complete-mode t)

(fset 'yes-or-no-p 'y-or-n-p)
(setq confirm-nonexistent-file-or-buffer nil)
(setq inhibit-startup-message t)
(setq visible-bell nil)
(setq ring-bell-function 'ignore)
(setq scroll-step 1)
(setq make-backup-files nil)
(setq windmove-wrap-around t)
(setq parens-require-spaces nil)
(setq ido-enable-flex-matching t)
(setq ido-create-new-buffer 'always)
(setq lua-indent-level 2)
(setq shell-file-name "bash")
(setq explicit-shell-file-name "bash")
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function kill-buffer-query-functions))

(setq-default tab-width 8)
(setq-default fill-column 80)
(setq-default default-fill-column 80)
(setq-default indent-tabs-mode nil)
(setq-default show-trailing-whitespace t)
(setq-default scroll-preserve-screen-position t)

(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

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

(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
