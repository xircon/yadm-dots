;;; init-keybindings.el --- My k/b customization file    -*- no-byte-compile: t -*-

;;(define-key key-translation-map [space] 'event-apply-hyper-modifier)

(require 'general)

(global-unset-key (kbd "M-l"))
(global-set-key (kbd "M-l") '(lambda()(interactive)
                               (modify-frame-parameters nil '((alpha . 100)))))

(global-set-key (kbd "<C-tab>")'nswbuff-switch-to-next-buffer)
(global-set-key (kbd "<C-S-iso-lefttab>") 'nswbuff-switch-to-previous-buffer)

;; --------------------------------------------------------------------------------------------------
;; =================================|>
;; 99999999999999999999 - Numeric
;; =================================|>
(global-set-key (kbd "H-1")'delete-other-windows)
(global-set-key (kbd "H-2")'split-window-below)
(global-set-key (kbd "H-3")'split-window-right)
(global-set-key (kbd "H-<tab>")'aw-flip-window)

;; Miscellaneous bindings:
(global-set-key (kbd "H-;")'comment-dwim-2)
(global-unset-key (kbd "C-SPC"))
(global-unset-key (kbd "C-@"))
(global-set-key (kbd "C-SPC") 'ryo-modal-mode)
(global-set-key (kbd "C-@") 'ryo-modal-mode)
;; --------------------------------------------------------------------------------------------------
;; =========|>
;; CUA keys
;; =========|>
(cua-mode t)
(cua-selection-mode t) ;; cua goodness without copy/paste ;; load windows-style keys using windows key instead of control.
(require 'win-win)

(global-set-key (kbd "H-a")'mark-whole-buffer)
(global-set-key (kbd "H-c")'easy-kill) ;; Copy
(global-set-key (kbd "H-e")'eval-region)
(global-set-key (kbd "H-k")'my-kill-buffer)
(global-set-key (kbd "H-v")'consult-yank-pop) ;; Paste
(global-set-key (kbd "H-x")'kill-region) ;; Cut
(global-set-key (kbd "H-z")'undo-tree-undo) ;; undo
;; --------------------------------------------------------------------------------------------------
;; ===============|>
;; AAAAAAAAAAAAAAA
;; ===============|>
(global-unset-key (kbd "C-a"))
(global-set-key (kbd "C-a")'mark-whole-buffer)

;; --------------------------------------------------------------------------------------------------
;; ===============|>
;; BBBBBBBBBBBBBBB
;; ===============|>
(global-unset-key (kbd "C-b"))

(general-define-key
 :prefix "C-b"
 "C-b" 'beginning-of-buffer
 "C-e" 'end-of-buffer
 "b" 'myswitchbuff
 "e" 'end-of-buffer
 "k" 'kill-this-buffer
 "j" 'bookmark-jump
 "s" 'bookmark-set)

(general-define-key
 :prefix "H-b"
"H-b" 'consult-bookmark
"a" 'create-abbrev
"b" 'beginning-of-buffer
"c" 'clone-indirect-buffer-other-window
"f" 'sidebar-open
"j" 'myswitchbuff
"m" 'helm-mini
"n" 'lunaryorn-new-buffer-frame
"s" 'sidebar-buffers-open)

(global-set-key (kbd "s-b")'myswitchbuff)


;; --------------------------------------------------------------------------------------------------
;; ===============|>
;; CCCCCCCCCCCCCCC
;; ===============|>
(global-unset-key (kbd "C-c"))
(global-set-key (kbd "C-c C-c")'easy-kill) ;; Copy

;;(global-set-key (kbd "C-c")'easy-kill)

;; --------------------------------------------------------------------------------------------------
;; ===============|>
;; DDDDDDDDDDDDDDD
;; ===============|>
;;(global-set-key (kbd "H-d")'duplicate-line)
;; --------------------------------------------------------------------------------------------------
;; ===============|>
;; EEEEEEEEEEEEEEE
;; ===============|>
(global-unset-key (kbd "M-e"))
(global-unset-key (kbd "s-e"))
(global-set-key (kbd "M-e")'end-of-line)

;; --------------------------------------------------------------------------------------------------
;; ===============|>
;; FFFFFFFFFFFFFFF
;; ===============|>
(global-unset-key (kbd "C-f"))
(global-set-key (kbd "C-f")'consult-line)
(global-set-key (kbd "H-f")'consult-line)

 (global-unset-key (kbd "M-f"))

;;(global-set-key (kbd "H-f") 'counsel-grep-or-swiper)

;; --------------------------------------------------------------------------------------------------
;; ===============|>
;; GGGGGGGGGGGGGGG
;; ===============|>
(general-define-key
 :prefix "H-g"
"1" 'delete-other-windows
"2" 'split-window-below
"3" 'split-window-right
"a" 'cfg_alacritty
;;"b" 'consult-bookmark
"c" 'cfg_cfgorg
"d" 'cfg_cfgtorg
"e" 'end-of-buffer
"f" 'my-recentf
"g" 'goto-line
"i" 'cfg_init
"k" 'cfg_keyb
"l" 'goto-line
"m" 'jump_messages
"r" 'cfg_ryo
"s" 'jump_scratch
"t" 'toggle-truncate-lines)
(global-set-key (kbd "H-g b")'consult-bookmark)
;; --------------------------------------------------------------------------------------------------
;; ===============|>
;; HHHHHHHHHHHHHHH
;; ===============|>
(global-set-key (kbd "H-h")'dired-hide-dotfiles-mode)
;; --------------------------------------------------------------------------------------------------
;; ===============|>
;; IIIIIIIIIIIIIII
;; ===============|>
 (global-set-key (kbd "H-i")'myprompt)

;; --------------------------------------------------------------------------------------------------
;; ===============|>
;; JJJJJJJJJJJJJJJ
;; ===============|>
;;; Support functions for C-j shortcuts:
(defun cfg_alacritty ()
  (interactive)
  (find-file "~/dotfiles/alacritty/alacritty.yml"))

(defun cfg_cfgorg ()
  (interactive)
  (find-file "~/.emacs.d/config.org")
  (ryo-off))

(defun cfg_esp ()
  (interactive)
  (find-file "~/.config/espanso/default.yml")
  (ryo-off))


(defun cfg_cfgkb ()
  (interactive)
  (find-file "~/.emacs.d/lisp/init-keybindings.el")
  (ryo-off))

(defun cfg_cfgtorg ()
  (interactive)
  (find-file "~/dotfiles/emacs.term/init.el")
  (ryo-off))

(defun cfg_init ()
  (interactive)
  (find-file "~/.emacs.d/init.el")
  (ryo-off))

(defun cfg_keyb ()
  (interactive)
  (find-file "~/.emacs.d/lisp/init-keybindings.el")
  (ryo-off))

(defun jump_messages ()
  (interactive)
  (switch-to-buffer "*Messages*"))

(defun jump_scratch ()
  (interactive)
  (switch-to-buffer "*scratch*"))

(defun cfg_ryo ()
  (interactive)
  (find-file "~/.emacs.d/lisp/init-my-ryo.el")
  (ryo-off))

(defun my_bookmark-jump ()
  (interactive)
  (call-interactively 'bookmark-jump)
  (ryo-off))


(global-unset-key (kbd "C-j"))

(general-define-key
 :prefix "C-j"
"a" 'cfg_alacritty
"b" 'myswitchbuff
"c" 'cfg_cfgorg
"d" 'cfg_cfgtorg
"e" 'end-of-buffer
"f" 'my-recentf
"g" 'goto-line
"i" 'cfg_init
"k" 'cfg_keyb
"l" 'goto-line
"m" 'jump_messages
"r" 'cfg_ryo
"s" 'jump_scratch
"t" 'toggle-truncate-lines)

(general-define-key
 :prefix "H-j"
"a" 'cfg_alacritty
"b" 'myswitchbuff
"c" 'cfg_cfgorg
"d" 'cfg_cfgtorg
"e" 'end-of-buffer
"f" 'my-recentf
"g" 'goto-line
"i" 'cfg_init
"H-j" 'bookmark-jump
"k" 'cfg_keyb
"l" 'goto-line
"m" 'jump_messages
"r" 'cfg_ryo
"s" 'jump_scratch
"t" 'toggle-truncate-lines)

(global-unset-key (kbd "M-e"))

(general-define-key
 "M-e" 'ivy-immediate-done)

;; --------------------------------------------------------------------------------------------------
;; ===============|>
;; KKKKKKKKKKKKKKK
;; ===============|>
;; --------------------------------------------------------------------------------------------------
;; ===============|>
;; LLLLLLLLLLLLLLL
;; ===============|>
(general-define-key
 :prefix "H-l"
";"'comment-line
"c"'copy-line
"d"'duplicate-line
"e"'kill-whole-line
"j"'goto-line
"m"'mc/edit-lines
"v"'visual-line-mode
"<up>"'move-line-up
"<down>"'move-line-down)
;;----------------------------------------------------------------------------
;; ===============|>
;; MMMMMMMMMMMMMMM
;; ===============|>
;; (global-set-key (kbd "H-m")'neotree-toggle)
;; --------------------------------------------------------------------------------------------------
;; ===============|>
;; NNNNNNNNNNNNNNN
;; ===============|>
(global-set-key (kbd "H-n")'neotree-toggle)
;; --------------------------------------------------------------------------------------------------
;; ===============|>
;; OOOOOOOOOOOOOOO
;; ===============|>
;; --------------------------------------------------------------------------------------------------
;; ===============|>
;; PPPPPPPPPPPPPP
;; ===============|>
(global-set-key (kbd "C-p")'check-parens)
(global-set-key (kbd "C-x p")'check-parens)

(global-set-key (kbd "H-p")'package-list-packages)
;; --------------------------------------------------------------------------------------------------
;; ===============|>
;; QQQQQQQQQQQQQQQ
;; ===============|>
(global-set-key (kbd "H-q")'quoted-insert) ;; Instead of C-q, now used to quit..
(global-unset-key (kbd "C-q"))
(global-set-key (kbd "C-q")'save-buffers-kill-terminal)
;; --------------------------------------------------------------------------------------------------
;; ===============|>
;; RRRRRRRRRRRRRRR
;; ===============|>
(global-set-key (kbd "C-r")'anzu-query-replace)
;;;(global-set-key (kbd "M-r")'anzu-query-replace)
(global-unset-key (kbd "M-r"))
(global-set-key (kbd "H-r")'query-replace)

(global-set-key (kbd "C-r") 'anzu-query-replace)
(global-set-key (kbd "s-r") 'my-recentf)


;; --------------------------------------------------------------------------------------------------
;; ===============|>
;; SSSSSSSSSSSSSSS
;; ===============|>
;;(global-set-key (kbd "H-s")'save-buffer)
(general-define-key
 :prefix "H-s"
 "a" 'cfg_alacritty
 ;;"s" 'sublima-scratch
 "p" 'previous-buffer
 "n" 'next-buffer)
;; --------------------------------------------------------------------------------------------------
;; ===============|>
;; TTTTTTTTTTTTTTT
;; ===============|>
(global-set-key (kbd "C-t")(lambda()(interactive)(vterm "/usr/bin/tmux")))
(global-set-key (kbd "H-t")'vterm-toggle)
;; --------------------------------------------------------------------------------------------------
;; ===============|>
;; UUUUUUUUUUUUUUU
;; ===============|>
(global-set-key (kbd "H-u")'undo-kill-buffer)
;; --------------------------------------------------------------------------------------------------
;; ===============|>
;; VVVVVVVVVVVVVVV
;; ===============|>
;; --------------------------------------------------------------------------------------------------
;; ===============|>
;; WWWWWWWWWWWWWWW
;; ===============|>
;; --------------------------------------------------------------------------------------------------
;; ===============|>
;; XXXXXXXXXXXXXXX
;; ===============|>
(global-unset-key (kbd "C-x C-x"))
(global-set-key (kbd "C-x C-x")'kill-region) ;; Cut
(global-set-key (kbd "C-x f") 'my-find-file)
(global-set-key (kbd "C-x C-f") 'my-find-file)
(global-set-key (kbd "C-x k") 'my-kill-buffer)
(global-set-key (kbd "C-x x") 'my-kill-buffer)
(global-set-key (kbd "C-x b") 'myswitchbuff)
(global-set-key (kbd "C-x C-b") 'myswitchbuff)

(global-set-key (kbd "M-x") 'amx)
;;(global-set-key (kbd "M-x") 'smex)

;; --------------------------------------------------------------------------------------------------
;; ===============|>
;; YYYYYYYYYYYYYYY
;; ===============|>
(global-unset-key (kbd "C-y"))
(global-set-key (kbd "C-y") 'undo)

(global-set-key (kbd "M-y") 'consult-yank-pop)

;; --------------------------------------------------------------------------------------------------
;; ===============|>
;; ZZZZZZZZZZZZZZZ
;; ===============|>
(key-chord-define-global "zz" 'ryo-modal-mode)
;; --------------------------------------------------------------------------------------------------
;;;(global-set-key (kbd "C-t")'multi-term)
;;(global-set-key (kbd "M-t")(lambda()(interactive)(vterm "/usr/bin/tmux")))
(global-set-key (kbd "M-t") 'aweshell-dedicated-toggle)

;; --------------------------------------------------------------------------------------------------

;; ============
;; Keychords
;; ============
;;(key-chord-define-global "bb" 'bm-menu)
;;(key-chord-define-global "zz" 'tabbar-backward)
;;(key-chord-define-global "xx" 'ryo-modal-mode)
;;(key-chord-define-global "mm"'menu-switch)

;; =============================================================================================
;; Use CUA mode to make life easier. We do _not__ use standard copy/paste etc. (see below).
;; =============================================================================================

;; Make control-z undo
(global-undo-tree-mode -1)
;(global-set-key (kbd "C-z") 'undo)


(use-package undo-fu)

(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-z")   'undo-fu-only-undo)
(global-set-key (kbd "C-S-z") 'undo-fu-only-redo)

;; https://github.com/redguardtoo/cliphist
(global-unset-key (kbd "C-v"))
(global-set-key (kbd "C-v")   'consult-yank-pop)

(global-unset-key (kbd "C-;"))
(global-set-key (kbd "C-;") 'comment-line)

;(define-key undo-tree-map (kbd "C-S-z") 'undo-tree-redo)
(define-key undo-tree-map (kbd "C-x u") 'undo)
(define-key undo-tree-map (kbd "C-x U") 'undo-tree-visualize)
(define-key undo-tree-map (kbd "M-z") 'undo-tree-visualize)

;; Make C-g quit undo tree
(define-key undo-tree-visualizer-mode-map (kbd "C-g") 'undo-tree-visualizer-quit)
(define-key undo-tree-visualizer-mode-map (kbd "<escape> <escape> <escape>") 'undo-tree-visualizer-quit)

;; Make right-click do something close to what people expect
(require 'mouse3)
(global-set-key (kbd "<mouse-3>") 'mouse3-popup-menu)
;; --------------------------------------------------------------------------------------------------
;; =====================|>
;; Shortcuts - Arrows
;; =====================|>

(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

(global-set-key (kbd "S-s-<up>") 'buf-move-up)
(global-set-key (kbd "S-s-<down>") 'buf-move-down)
(global-set-key (kbd "S-s-<left>") 'buf-move-left)
(global-set-key (kbd "S-s-<right>") 'buf-move-right)

(global-set-key (kbd "C-=") 'which-key-mode)

(provide 'init-keybindings)

;;; init-keybindings.el ends here
