;;; package --- Summary
;;; Commentary:
;;; Code:

;; ********************************************************
;; ** Package Archives:
;; ********************************************************
(package-initialize)

(setq straight-repository-branch "develop")
 (defvar bootstrap-version)
   (let ((bootstrap-file
          (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
         (bootstrap-version 5))
     (unless (file-exists-p bootstrap-file)
       (with-current-buffer
           (url-retrieve-synchronously
            "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
            'silent 'inhibit-cookies)
         (goto-char (point-max))
      (eval-print-last-sexp)))
      (load bootstrap-file nil 'nomessage))

(setq package-enable-at-startup nil)

(straight-use-package 'use-package)
(straight-use-package 'git)
(straight-use-package 'magit)
(straight-use-package 'projectile)
(straight-use-package 'dash)
(straight-use-package 'dash-functional)
(straight-use-package 'ov)
(straight-use-package 'frame-local)
(straight-use-package 'clipmon)
;;(straight-use-package 'org)
(straight-use-package 'xclip)
(straight-use-package 'helm)
(straight-use-package 'ryo-modal)
(straight-use-package 'counsel)
(straight-use-package 'anzu)
(straight-use-package 'auto-sudoedit)
(straight-use-package 'undo-fu)
(straight-use-package 'live-py-mode)

(add-to-list 'term-file-aliases '("alacritty" . "xterm")) 

(setq gc-cons-threshold 100000000)

(remove-hook 'find-file-hooks 'vc-find-file-hook)

(setq package-archives
        '(("Elpa"     . "https://elpa.gnu.org/packages/")
          ;;("cselpa" . "https://elpa.thecybershadow.net/packages/")
          ;;("Melpa Stable" . "https://stable.melpa.org/packages/")
          ("Melpa"        . "https://melpa.org/packages/"))
       package-archive-priorities
       '(("GNU ELPA"     . 5)
         ("MELPA"        . 0)))
(require 'use-package)
;; *********************************************************
;; ** Kill emacs:
;; **********************************************************
(defun server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server"
  (interactive)
  (save-some-buffers)
  (kill-emacs))
;; *********************************************************
;; ** Load Path:
;; **********************************************************
(defun update-load-path (&rest _)
   "Update `load-path'."
   (push (expand-file-name "site-lisp" user-emacs-directory) load-path)
   (push (expand-file-name "My-emacs" user-emacs-directory) load-path)
   (push (expand-file-name "plugins" user-emacs-directory) load-path)
   (push (expand-file-name "emacs-webkit" user-emacs-directory) load-path)
   (push (expand-file-name "lisp" user-emacs-directory) load-path))
(update-load-path)
;; ****************
;;
;; ****************

;; **********************************************************
;; ** Line numbers & Fringe setup
;; **********************************************************
(global-linum-mode 1)
(setq display-line-numbers-width-start 1)
(setq display-line-numbers 'relative)
(setq linum-format "%d ")
(require 'linum-highlight-current-line-number)
(setq linum-format 'linum-highlight-current-line-number)
(set-face-foreground 'linum "blue")
(set-frame-parameter (selected-frame) 'internal-border-width 15)

;; **********************************************************
;; ** Duplicate lines:
;; **********************************************************
(defun duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (forward-line 1)
  (yank))

;; **********************************************************
;; Modeline:
;; **********************************************************
(use-package doom-modeline

:ensure t
:straight t

:defer t

:hook (after-init . doom-modeline-init))

(set-face-background 'mode-line "#101010")
(set-face-foreground 'mode-line "#999999")
;;(setq doom-modeline-icon (display-graphic-p))
;;(setq doom-modeline-major-mode-icon t)
;;(setq doom-modeline-major-mode-color-icon t)
;; Whether display the icon for the buffer state. It respects `doom-modeline-icon'.
;;(setq doom-modeline-buffer-state-icon t)
;;(setq doom-modeline-modal-icon t)

(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(display-time)

(menu-bar-mode +1)

;; **********************************************************
;; Rainbow delimeters:
;; **********************************************************
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
;; **********************************************************
 (use-package redo+ :straight t)
  (global-set-key (kbd "C-?") 'redo)

  (setq undo-tree-auto-save-history 1)

  (auto-fill-mode -1)
;; *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** ***
;; History:
;; *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** ***

(use-package savehist
  ;;:ensure nil
  :straight t
  :hook (after-init . savehist-mode)
  ; Allow commands in minibuffers
  :init (setq enable-recursive-minibuffers t 
  history-length 1000
  savehist-additional-variables '(mark-ring
  global-mark-ring
  search-ring
  regexp-search-ring
  extended-command-history)
  savehist-autosave-interval 60))

(setq savehist-save-minibuffer-history t)

(setq savehist-additional-variables
        '(kill-ring
          search-ring
          regexp-search-ring
          last-kbd-macro
          kmacro-ring
          shell-command-history
          Info-history-list
          register-alist))

(savehist-mode t)

;; *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** ***

(setq warning-minimum-level :emergency)

;(normal-erase-is-backspace-mode 1)

;;(ryo-modal-mode -1)

(defun my-kill-emacs ()
  ;; "Set kill-emacs-query-functions , confirm-kill-emacs , and kill-emacs-hook to nil to quit emacs without any pesky prompts (except to save buffers)."
  (interactive)
  (let (kill-emacs-query-functions confirm-kill-emacs kill-emacs-hook)
    (save-buffers-kill-emacs)))

(require 'use-package :straight t)

(setq vc-follow-symlinks t)

;; ********************************************************
;; MySwitchBuff:
;; ********************************************************
(setq resize-mini-windows t)

(defun myswitchbuff()
  (interactive)
  (counsel-switch-buffer)
  (ryo-off)
  (recentf-save-list/silent))

(defun my-create-bm ()
  (interactive)
  (bookmark-set)
  (bookmark-save))

;;******************************************************
;; Recent files:
;;******************************************************
(use-package recentf
    :straight t)
(recentf-mode 1)
(setq recentf-max-menu-items 50)
(global-set-key "\C-x\ \C-r" 'consult-buffer)

(defun recentf-save-list/silent ()
 (interactive)
 (let ((save-silently t)) (recentf-save-list))
 (message nil))

(run-at-time (current-time) 180 'recentf-save-list/silent)

(add-hook 'find-file-hook 'recentf-save-list/silent)

;; *******************************************************************
;; ** Centaur tabs:
;; *******************************************************************
(use-package centaur-tabs
   :straight t
   :demand
   :config
   (centaur-tabs-mode t))

(setq centaur-tabs-set-icons t)
(centaur-tabs-headline-match)
(defun centaur-tabs-hide-tab (x)
  "Do no to show buffer X in tabs."
  (let ((name (format "%s" x)))
    (or
     ;; Current window is not dedicated window.
     (window-dedicated-p (selected-window))

     ;; Buffer name not match below blacklist.
     (string-prefix-p "*epc" name)
     (string-prefix-p "*helm" name)
     (string-prefix-p "*Helm" name)
     (string-prefix-p "*Compile-Log*" name)
     (string-prefix-p "*lsp" name)
     (string-prefix-p "*company" name)
     (string-prefix-p "*flycheck" name)
     (string-prefix-p "*tramp" name)
     (string-prefix-p " *Mini" name)
     (string-prefix-p "*help" name)
     (string-prefix-p "*straight" name)
     (string-prefix-p " *temp" name)
     (string-prefix-p "*Help" name)
     (string-prefix-p "*mybuf" name)

     ;; Is not magit buffer.
     (and (string-prefix-p "magit" name)
	  (not (file-name-extension name)))
     )))

;; ********************************************************
;; *** KKK - Keyboard Shortcuts 
;; ********************************************************
(global-set-key (kbd "C-x b") 'myswitchbuff)
;;(global-set-key (kbd "C-x b") 'counsel-switch-buffer)
(global-set-key (kbd "M-x") 'helm-M-x)

(global-unset-key (kbd "C-s"))
(global-set-key (kbd "C-s") 'save-buffer)

(global-set-key (kbd "C-x k") 'kill-this-buffer)

(global-set-key (kbd "C-x C-f") 'find-file)

;; ** Copy and paste keys:
;; overwrite selected text
(delete-selection-mode t)

(global-set-key (kbd "M-y") 'counsel-yank-pop)
(global-set-key (kbd "C-x c")  'kill-ring-save) ;; Copy
(global-unset-key (kbd "C-y"))
(global-set-key (kbd "C-y") 'undo)
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-z") 'undo)
(global-unset-key (kbd "M-c"))
(global-set-key (kbd "M-c")  'kill-ring-save) ;; Copy
(global-unset-key (kbd "M-v"))
(global-set-key (kbd "M-v")  'counsel-yank-pop) ;; Copy

;; ** Find:
(global-unset-key (kbd "C-f"))
(global-set-key (kbd "C-f")'swiper)
(global-set-key (kbd "s-c")'consult-line)

;; ** Quit:
(global-unset-key (kbd "C-q"))
;(global-set-key (kbd "C-q")'my-kill-emacs) 
(global-set-key (kbd "C-q")'server-edit) 

(global-set-key (kbd "M-d") 'duplicate-line)

;; ** Mouse:
(xterm-mouse-mode t)
(global-set-key (kbd "<mouse-4>") 'scroll-down-line)
(global-set-key (kbd "<mouse-5>") 'scroll-up-line)

;; ** Home & End keys:
(global-set-key (kbd "<home>") 'beginning-of-line)
(global-set-key (kbd "<select>") 'end-of-line)

;; ** Recentf keybinds:
(global-set-key "\C-x\ \C-r" 'counsel-recentf)
(global-set-key (kbd "C-r") 'counsel-recentf)
(global-set-key (kbd "s-r") 'counsel-recentf)

;; ** Bookmarks:
(global-unset-key (kbd "C-j"))
(global-set-key (kbd "C-j b") 'counsel-bookmark)
(global-set-key (kbd "C-j s") 'my-create-bm)

;;***************************************************
;; Sidebar
;;***************************************************
(add-to-list 'load-path "~/.emacs.d/lisp/sidebar.el/")
(require 'sidebar)
(global-set-key (kbd "H-j C-f") 'sidebar-open)
(global-set-key (kbd "C-x a") 'sidebar-buffers-open)

;;********
;; org: **
;;********
;; (use-package init-org :defer t)

;; (use-package org :straight t)
;; (define-key org-mode-map (kbd "C-e") nil)
;; (define-key org-mode-map (kbd "C-j") nil)
;; (define-key org-mode-map (kbd "M-s") nil)

;; (setq org-support-shift-select t)

;; (use-package org-superstar :straight t) 
;;     (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))

;; (add-hook 'org-mode-hook (lambda ()
;; "Beautify Org Checkbox Symbol"
;;     (push '("[ ]" .  "☐") prettify-symbols-alist)
;;     (push '("[X]" . "☑" ) prettify-symbols-alist)
;;     (push '("[-]" . "❍" ) prettify-symbols-alist)
;;     (prettify-symbols-mode)))

;; ****************
;; ** Clipboard  **
;; ****************

(cua-mode t)
(cua-selection-mode t)

(clipmon-mode 1)
 
(use-package easy-kill
    :straight t
    :defer t
    :config
    (global-set-key [remap kill-ring-save] #'easy-kill)
    (global-set-key [remap cua-copy-region] #'easy-kill)
    (global-set-key [remap mark-sexp] #'easy-mark))

(setq x-select-enable-clipboard t)
(xclip-mode 1)
(setq save-interprogram-paste-before-kill t)
(setq gui-select-enable-primary t)

;; **************************************************
;; ** which-key:
;; **************************************************
(use-package which-key
     :straight t
     :defer t
     :diminish which-key-mode
     ;;:bind (:map help-map ("C-h" . which-key-C-h-dispatch))
     :hook (after-init . which-key-mode))
(which-key-mode)

;;**************************************************
;;* Improved keyboard quit function:
;;**************************************************
(defun my-escape ()
   "Quit in current context.
    When there is an active minibuffer exit that.
    Ignore quit command for keyboard macros. Otherwise
    behave like `keyboad-quit'."
    (interactive)
    (if-let ((minibuffer (active-minibuffer-window)))
        (with-current-buffer (window-buffer minibuffer)
          (minibuffer-keyboard-quit))
      ;; ignore quit for macros
      (unless (or defining-kbd-macro executing-kbd-macro)
        (keyboard-quit))))

(global-set-key [remap keyboard-quit] #'my-escape)

;; #################################################################

;; *****************************************************************
(global-linum-mode 1)
(setq display-line-numbers-width-start 1)
(setq display-line-numbers 'relative)

(fset 'yes-or-no-p 'y-or-n-p)
  (setq visible-bell t)
  (desktop-save-mode 0)

(save-place-mode +1)

(use-package idle-highlight-mode :straight t :defer t)

;; **************************************************************
;; RYO mode:
;; **************************************************************

;;(load "~/dotfiles/emacs-term/functions.el")
;;(load "~/dotfiles/emacs-term/lisp/init-keybindings.el")

(use-package init-my-ryo )

(global-set-key (kbd "C-SPC") 'ryo-toggle)
(global-set-key (kbd "C-@") 'ryo-toggle)

(defun ryo-on()
  (interactive)
  (ryo-modal-mode 1)
  (message "ryo on")
  (set-background-color "#111144")
  (set-cursor-color "red"))

(defun ryo-off()
  (interactive)
  (ryo-modal-mode -1)
  (message "ryo off")
  (set-background-color "#000000")
  (set-cursor-color "green"))

(defun ryo-toggle()
  (interactive)
  (if (bound-and-true-p ryo-modal-mode)
      (ryo-off)
    (ryo-on)
    ))

(defun my-buffer-recentf()
 (interactive)
 (ryo-off)
 ;;(setq ivy-use-virtual-buffers t)
 (counsel-buffer-or-recentf)
 (recentf-save-list/silent))

(ryo-modal-keys
 ("j"
  (("b" my-buffer-recentf)
   ("g" goto-line)
   ("k" kill-buffer)
   ("<right>"  centaur-tabs-forward)
   ("<left>"  centaur-tabs-backward)
   ("s" my-create-bm))))

(ryo-modal-keys

   ("1" delete-other-windows)
   ("2" split-window-below)
   ("3" split-window-right)
   ;("4" switch-window)
   ;("t" transpose-frame)
   
   (";" comment-line)
   
   ("," ryo-modal-repeat)
   ("q" save-buffers-kill-terminal)
   ("a" mark-whole-buffer)
   ("s" save-buffer)
   ("k" kill-buffer)
   ("p" package-list-packages)
   ("J" python-nav-forward-defun)
   ("K" python-nav-backward-defun)
   ("y" end-of-line)
   ("c" easy-kill)
   ("v" counsel-yank-pop)
   ("x" kill-region)
   ("g" my-escape)
   ("u" beginning-of-line)
   ("r" counsel-linux-app)
   ;;("f" swiper-all)
   ;;("S" counsel-grep-or-swiper)
   ("R" anzu-query-replace)
   ("+" text-scale-adjust 2)
   ("-" text-scale-adjust 2)
   )
;; *******************************************************************
;; ** auto-sudoedit:
;; *******************************************************************
(auto-sudoedit-mode 1)
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
;; *******************************************************************
;; ** Themeing:
;; *******************************************************************
(setq custom-theme-directory "~/.emacs.d/themes")

(use-package abyss-theme :straight t
     :config (load-theme 'abyss t))

(global-hl-line-mode +1) 
(set-face-background 'region "DarkOrchid1")
(set-face-foreground 'font-lock-comment-face  "BlueViolet")
(set-face-background 'font-lock-comment-face  "gray10")
(set-face-attribute 'font-lock-comment-face nil :bold t)
(set-background-color "#000000")

(use-package color :straight t)

(set-face-background 'mode-line "#101011")

(defun minibuffer-bg ()
  (set (make-local-variable 'face-remapping-alist)
       '((default :background "#232959"))))
    (add-hook 'minibuffer-setup-hook 'minibuffer-bg)
;; *******************************************************************

(use-package undo-fu-session
  :straight t
  :config
  (setq undo-fu-session-incompatible-files '("COMMIT_EDITMSG$" "git-rebase-todo$")))

(global-undo-fu-session-mode)

(normal-erase-is-backspace-mode t)

(global-set-key "\C-d" 'delete-backward-char)

(global-set-key (kbd "C-d") 'delete-backward-char)

(set-background-color "#000000")
;; ******************************************************************

(menu-bar-mode +1)
