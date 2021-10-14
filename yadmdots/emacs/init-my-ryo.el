;;; init-my-ryo.el --- My ryo customisation file    -*- no-byte-compile: t -*-
;;; Commentary:

;; Code:

;; =====================================================================
;; your modal mode https://github.com/Kungsgeten/ryo-modal ;; ;;
;; =====================================================================

;;(push '((nil . "ryo:.*:") . (nil . "")) which-key-replacement-alist)
(use-package init-python)

;;(defvar which-key-enable-extended-define-key t "No effect.") 

;; Support functions::

(defun ryo-on()				
  (interactive)
  (ryo-modal-mode 1)
  (message "ryo on")
  (set-background-color "#003131")
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
      (ryo-on)))

(defun my-save-buff()
  (interactive)
  (ryo-off)
  (setq lnum (line-number-at-pos))
;;  (check-parens)
  (goto-line lnum)
  (save-buffer)
  (message "%s" lnum))

(defun my-split-window-right()
  (interactive)
  (split-window-right)
  (other-window 1)
  (my-find-file)
  (ryo-off))

(defun my-split-window-below()
  (interactive)
  (split-window-below)
  (other-window 1)
  (my-find-file)
  (ryo-off))

(defun myswitchbuff()
     (interactive)
     (ryo-off)
     (consult-buffer)
     (recentf-save-list/silent))

(defun my-find-file ()
     (interactive)
     (ryo-off)
     (call-interactively 'find-file)
     (recentf-save-list/silent))

(defun my-recentf()
  (interactive)
  (ryo-off)
  (consult-buffer)
  (recentf-save-list/silent))

(defun my-duplicate-line()
  (interactive)
  (ryo-off)
  (duplicate-line))

(defun my-kill-whole-line()
  (interactive)
  (ryo-off)
  (kill-whole-line))

(defun my-copy-line()
  (interactive)
  (kill-ring-save (point-at-bol) (point-at-eol))
  (message "1 line copied"))

(global-set-key (kbd "C-7")'ryo-toggle)
(global-set-key (kbd "C-SPC")'ryo-toggle)
(global-set-key (kbd "S-SPC")'ryo-toggle)

;; START 

(use-package ryo-modal
  :commands ryo-modal-mode
  ;;; :bind ("<C-SPC>" . ryo-modal-mode)
  :init
  :config

(ryo-modal-keys
   ("1" delete-other-windows :then '(ryo-off) :exit t)
   ("2" my-split-window-below)
   ("3" my-split-window-right)
   ("4" switch-window) 
   (";" comment-dwim :then '(ryo-off) :exit t)
   ("," ryo-modal-repeat)
   ("<left>" left-word)
   ("<right>" right-word)

   ;; AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
   ;; A is for.........
   ("a a" mark-whole-buffer :then '(ryo-off) :exit t)

   ;; ********************************************* 
   
   ;; BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB
   ;; b is for buffer:
   ("b b" myswitchbuff)
   ("b r" run-buffer)
   ("B" balance :then '(ryo-off) :exit t)

   ;; CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
   ("c l" my-copy-line :then '(ryo-off) :exit t)
   ("c w" capitalize-word :then '(ryo-off) :exit t)
   

   ;; DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD
   ("d d" dired-sidebar-toggle-sidebar :then '(ryo-off) :exit t)
   ("d x" dired :then '(ryo-off) :exit t)
   
   ;; EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE
   
   ;; FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
   ;; Find & Replace
   ("f b" myswitchbuff)
   ("f f" my-find-file :then '(ryo-off) :exit t)
   ("f r" anzu-query-replace :then '(ryo-off) :exit t)

   ;; GGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGG
   ;; Go to:
   ("g b" beginning-of-buffer :then '(ryo-off) :exit t)
   ("g e" end-of-buffer :then '(ryo-off) :exit t)
   ("g g" goto-line :then '(ryo-off) :exit t)


   ;; HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH
   ("h k" describe-key :then '(ryo-off) :exit t)

   
   ;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
   
   ;; JJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJ
   ;; J is for Jump:
   ("j j" consult-buffer :then '(ryo-off) :exit t :name "Buffer")

   ;; KKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKK
   ("k b" kill-buffer :then '(ryo-off) :exit t)
   ("K"   kill-whole-line)

   ;; LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL
   ;; l is for line:
   ("l b" beginning-of-line  :then '(ryo-off) :exit t :name "Line begin")
   ("l e" end-of-line        :then '(ryo-off) :exit t :name "Line end")
   ("l d" my-kill-whole-line :then '(ryo-off) :exit t :name "Kill line")
   ("l l" my-duplicate-line  :then '(ryo-off) :exit t :name "Dup line")

   
   ;; MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
   ;; Mark
   ("m a" mark-whole-buffer :then '(ryo-off) :exit t)
   ("m l" expand-line-mark-line :then '(ryo-off) :exit t)
   ("m w" mark-word :then '(ryo-off) :exit t)

   ;; NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN
   
   ;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
   ;;; O is for open:
   ("o b" sidebar-buffers-open :then '(ryo-off) :exit t :name "sidebar")
   ("o c" cfg_cfgorg :then '(ryo-off) :exit t :name "Config")
   ("o e" cfg_esp :then '(ryo-off) :exit t :name "Espanso")
   ("o k" cfg_cfgkb :then '(ryo-off) :exit t :name "Keys")
   ("o f" my-find-file :then '(ryo-off) :exit t :name "Open file")
   ("o r" cfg_ryo :then '(ryo-off) :exit t :name "RYO")
   ("o s" my_scratch :then '(ryo-off) :exit t :name "Scratch")

   ;; PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
   ("p p" list-packages :then '(ryo-off) :exit t :name "Pkg List")
   
   ;; QQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQ
   ("q"   save-buffers-kill-terminal)

   ;; RRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRR

   ;; SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
   ("s s"   my-save-buff)

   ;; TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
   ("t"   transpose-frame :then '(ryo-off) :exit t)

   ;; UUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUU
   ("u"   beginning-of-line)

   ;; VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV
   ("v"   consult-yank-pop)

   ;; WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW
   ("w"   whitespace-mode :then '(ryo-off) :exit t :name "Whitespace")

   ;; XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

   ;; YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY
   ("y"   end-of-line)

   ;; ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ

   ("<up>"  beginning-of-buffer :then '(ryo-off) :exit t :name "Top" ) 
   ("<down>"  end-of-buffer :then '(ryo-off) :exit t :name "Top" ) 
   ("+"   text-scale-adjust 2)
   ("-"   text-scale-adjust 2)))
;; *******************************************************************
;; *******************************************************************

(ryo-modal-key
 "SPC" '(("s" save-buffer)
         ;; ("g" magit-status)
         ("k" describe-key)
         ("m" menu-switch)
         ("j c" cfg_cfgorg)
         ("j k" cfg_cfgkb)
         ("v" consult-yank-pop)
	 ("z" undo-fu-only-undo)
         ("p" package-list-packages)
         ("t" straight-pull-all)
         ("SPC" ryo-toggle))
         :then '(ryo-off) :exit t)

;; --------------------------------------------------------------------------------------------------
(setq ryo-modal-cursor-color "red")

;; **|>**|>**|>**|>**|>**|>**|>**|>**|>**|>**|>**|>**|>**|>**|>**|>**|>**|>**|>**|>**|>**|>**|>**|>**|>
;; End Code:
(push '((nil . "ryo:.*:") . (nil . "")) which-key-replacement-alist)

(provide 'init-my-ryo)
;;; init-my-ryo.el ends here
