;;; Functions --- Summary
;;; Commentary
;; -%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%
(with-no-warnings
  (require 'cl-lib))

(defun lunaryorn-new-buffer-frame ()
  ;; -%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%
  "Create a new frame with a new empty buffer."
  (interactive)
  (let ((buffer (generate-new-buffer "untitled")))
    (set-buffer-major-mode buffer)
    (display-buffer buffer '(display-buffer-pop-up-frame . nil))))

;***********************************************
(defun screenshot-svg ()
  "Save a screenshot of the current frame as an SVG image.
Saves to a temp file and puts the filename in the kill ring."
  (interactive)
  (let* ((filename (make-temp-file "Emacs" nil ".svg"))
         (data (x-export-frames nil 'svg)))
    (with-temp-file filename
      (insert data))
    (kill-new filename)
    (message filename)))
;***********************************************

(defvar th-shell-popup-buffer nil)

(defun my_scratch()
  (interactive)
  (switch-to-buffer "*scratch*"))

(defun my_messages()
  (interactive)
  (switch-to-buffer "*Messages*"))

(defun my_config()
  (interactive)
  (find-file"~/.emacs.d/config.org"))

(defun my_ryo()
  (interactive)
  (find-file"~/.emacs.d/lisp/init-my-ryo.el"))


(defun th-shell-popup ()
  "Toggle a shell popup buffer with the current file's directory as cwd."
  (interactive)
  (unless (buffer-live-p th-shell-popup-buffer)
    (save-window-excursion (shell "*Popup Shell*"))
    (setq th-shell-popup-buffer (get-buffer "*Popup Shell*")))
  (let ((win (get-buffer-window th-shell-popup-buffer))
        (dir (file-name-directory (or (buffer-file-name)

                                      ;; dired
                                      dired-directory
                                      ;; use HOME
                                      "~/"))))
    (if win
        (quit-window nil win)
      (pop-to-buffer th-shell-popup-buffer nil t)
      (comint-send-string nil (concat "cd " dir "\n")))))

;;(global-set-key (kbd "<f11>") 'th-shell-popup)

;; -%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<

(defun do-org-show-all-inline-images ()
  (interactive)
  (org-display-inline-images t t))
(global-set-key (kbd "C-c C-x C v")
                'do-org-show-all-inline-images)

(add-hook 'text-mode-hook 'my-adoc-mode-hook)

;; -%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<

(defun my-adoc-mode-hook ()
  "Custom `text-mode' behaviours."
  (iimage-mode 1))

;; -%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<

(defun delete-char-or-kill-terminal-buffer (N &optional killflag)
  (interactive "p\nP")
  (if (string= (buffer-name) "*terminal*")
  (kill-buffer (current-buffer))
(delete-char N killflag)))
(global-set-key (kbd "C-d") 'delete-char-or-kill-terminal-buffer)

;; -%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<

(defun copy-line (arg)
    "Copy lines (as many as prefix argument) in the kill ring.
      Ease of use features:
      - Move to start of next line.
      - Appends the copy on sequential calls.
      - Use newline as last char even on the last line of the buffer.
      - If region is active, copy its lines."
    (interactive "p")
    (let ((beg (line-beginning-position))
          (end (line-end-position arg)))
      (when mark-active
        (if (> (point) (mark))
            (setq beg (save-excursion (goto-char (mark)) (line-beginning-position)))
          (setq end (save-excursion (goto-char (mark)) (line-end-position)))))
      (if (eq last-command 'copy-line)
          (kill-append (buffer-substring beg end) (< end beg))
        (kill-ring-save beg end)))
    (kill-append "\n" nil)
    (beginning-of-line (or (and arg (1+ arg)) 2))
    (if (and arg (not (= 1 arg))) (message "%d lines copied" arg)))

;; -%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<

;; Markdown Preview
(defun markdown-html (buffer)
  (princ (with-current-buffer buffer
           (format "<!DOCTYPE html><html><title>Impatient Markdown</title><xmp theme=\"united\" style=\"display:none;\"> %s  </xmp><script src=\"http://strapdownjs.com/v/0.2/strapdown.js\"></script></html>" (buffer-substring-no-properties (point-min) (point-max))))
         (current-buffer)))

;; -%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<-%<

(defun make-new-buffer ()
  "makes a new buffer, uniquely named"
  (interactive)
  (switch-to-buffer "New")
  (rename-uniquely))

;;**************************************************

(defun byte-compile-cfg()
(interactive)
;; ********************
;; Load Settings files:
;; ********************
(let ((myfiles '("init.el" "theme-changes.el" "load-pkg.el" "functions.el" "easy-menus.el" "shortcuts.el" "mybookmarks.el" "hyper-sc.el" "super-sc.el" "ctrl-sc.el" "hydra-conf.el" "keychords-sc.el")))
  (while myfiles
       (byte-compile-file (concat "/home/steve/.emacs.d/My-emacs/" (car myfiles)) (pop myfiles))))
)

;; ********************************************************************

(global-set-key (kbd "<f5>") 'lawlist-bookmark)

(defun lawlist-bookmark (choice)
  "Choices for directories and files."
  (interactive "c[D]ired | [v]ocab.org | [g]td.org | [d]iary.org | [n]otes.org")
  (cond
   ((eq choice ?D)
    (dired "/very/long/and/boring/path/which/make/me/use/tab/for/..."))
   ((eq choice ?v)
    (find-file "/Users/HOME/.0.data/vocab.org")
    (message "Opened:  %s" (buffer-name)))
   ((eq choice ?g)
    (find-file "/Users/HOME/.0.data/gtd.org")
    (message "Opened:  %s" (buffer-name)))
   ((eq choice ?d)
    (find-file "/Users/HOME/.0.data/diary.org")
    (message "Opened:  %s" (buffer-name)))
   ((eq choice ?n)
    (find-file "/Users/HOME/.0.data/notes.org")
    (message "Opened:  %s" (buffer-name)))
   (t (message "Quit"))))

;; **********************************************************************************************
;;(global-set-key [f7] 'swiper-all)

;;**************************************************

(use-package poporg
  :bind (("C-c /" . poporg-dwim)))

;; define function to display ansi colours for a buffer
;; http://stackoverflow.com/questions/23378271/how-do-i-display-ansi-color-codes-in-emacs-for-any-mode
(require 'ansi-color)
(defun display-ansi-colors ()
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))


;;**************************************************

(defun my-agenda ()
  "Open my google calendar agenda file. The agenda is displayed in the buffer *gcal*."
  (interactive)
  (run-gcal)
  ;; set name of calendar buffer and location of file containing my agenda
  (let ((tmp-buff-name "*gcal*") (cal-file (expand-file-name "~/.emacs.d/gcal")))
    ;; switch to calendar buffer
    (switch-to-buffer tmp-buff-name)
    ;; turn off read only to overwrite if buffer exists
    (read-only-mode -1)
    ;; clear buffer
    (erase-buffer)
    ;; insert agenda file
    (insert-file-contents cal-file)
    ;; turn on colours
    (display-ansi-colors)
    ;; turn on special mode
    (special-mode)
    ;; turn off line wrapping
    (visual-line-mode -1)))

;;**************************************************
;;**************************************************

(defun run-gcal ()
  "run a command on the current file and revert the buffer"
  (interactive)
  (shell-command
   (format "/usr/bin/gcalcli agenda > ~/.emacs.d/gcal")))
(global-set-key (kbd "C-S-e") 'call-something-on-current-buffers-file)

;;**************************************************

(defun run-scrftch ()
  "run a command on the current file and revert the buffer"
  (interactive)
  (shell-command
   (format "/usr/bin/screenfetch > ~/.emacs.d/scrftch")))
(global-set-key (kbd "C-S-e") 'call-something-on-current-buffers-file)


(defun my-scrftch ()
  "Open my google calendar agenda file. The agenda is displayed in the buffer *gcal*."
  (interactive)
  (run-scrftch)
  ;; set name of calendar buffer and location of file containing my agenda
  (let ((tmp-buff-name "*scrf*") (cal-file (expand-file-name "~/.emacs.d/scrftch")))
    ;; switch to calendar buffer
    (switch-to-buffer tmp-buff-name)
    ;; turn off read only to overwrite if buffer exists
    (read-only-mode -1)
    ;; clear buffer
    (erase-buffer)
    ;; insert agenda file
    (insert-file-contents cal-file)
    ;; turn on colours
    (display-ansi-colors)
    ;; turn on special mode
    (special-mode)
    ;; turn off line wrapping
    (visual-line-mode -1)))

;;**************************************************


;;**************************************************
;; http://emacs-fu.blogspot.com/
;;**************************************************

(defun ffr ()
  "Like `ido-find-file, but automatically edit the file with
root-privileges (using tramp/sudo), if the file is not writable by
user."
  (interactive)
  (let ((file (ido-read-file-name "Edit as root: ")))
    (unless (file-writable-p file)
      (setq file (concat "/sudo:root@localhost:" file)))
    (find-file file)))
;; or some other keybinding...
(global-set-key (kbd "C-x F") 'ffr)

;;***************************
;;Time stamp message buffer *
;;***************************
;; (defun sh/current-time-microseconds ()
;;   "Return the current time formatted to include microseconds."
;;   (let* ((nowtime (current-time))
;;          (now-ms (nth 2 nowtime)))
;;     (concat (format-time-string "[%Y-%m-%dT%T" nowtime) (format ".%d]" now-ms))))

;; (defun sh/ad-timestamp-message (FORMAT-STRING &rest args)
;;   "Advice to run before `message' that prepends a timestamp to each message.

;; Activate this advice with:
;; (advice-add 'message :before 'sh/ad-timestamp-message)"
;;        (unless (string-equal FORMAT-STRING "%s%s")
;;          (let ((deactivate-mark nil)
;;                (inhibit-read-only t))
;;            (with-current-buffer "*Messages*"
;;              (goto-char (point-max))
;;              (if (not (bolp))
;;                  (newline))
;;              (insert (sh/current-time-microseconds) " ")))))

;; (advice-add 'message :before 'sh/ad-timestamp-message)
;;**************************************************
(defun my-clone-and-open-file (filename)
  "Clone the current buffer writing it into FILENAME and open it"
  (interactive "FClone to file: ")
  (save-restriction
    (widen)
    (write-region (point-min) (point-max) filename nil nil nil 'confirm))
  (find-file filename))

;; **************************************************************************************************************

(defun backup-and-save ()
  (interactive)
  (setq filename (buffer-file-name)) ;; Store original filename
  (setq file-ext (file-name-extension(buffer-file-name))) ;; Ditto file extension
  (setq tmpfile "tmp-zzz.el") ;; Store tmpfile name
  (message (concat buffer-file-name " & " file-ext))
  ;; Only check elisp files:
  (when (string= file-ext "el")
    (check-parens)
    (eval-buffer) ;; <<<< THIS IS GIVING ME PROBLEMS!!!!!
    (widen)
    ;; Save a copy of the buffer to tmpfile
    (write-region (point-min) (point-max) tmpfile nil nil nil)
    ;; Byte compile a copy of the file:
    (setq byte-compile-error-on-warn 0) ;; Turn off compiler warnings
    (byte-compile-file tmpfile)
    (setq byte-compile-error-on-warn t) ;; Turn on compiler warnings
    )

  ;; Write a time and dated backup and then save file:
  (write-file (concat "~/.emacs_backups/" (file-name-nondirectory filename) (format-time-string "_%Y%m%d-%H:%M:%S")))
  (write-file filename)
  ;; Cleanup
  (delete-file "tmp-zzz.el")
  (delete-file "tmp-zzz.elc")
  (eval-buffer)
  )

;;(add-hook 'before-save-hook  'backup-and-save)

(defun run-current-file ()
  "Execute the current file.
For example, if the current buffer is x.py, then it'll call 「python x.py」 in a shell.
Output is printed to buffer “*xah-run output*”.

The file can be Emacs Lisp, PHP, Perl, Python, Ruby, JavaScript, TypeScript, golang, Bash, Ocaml, Visual Basic, TeX, Java, Clojure.
File suffix is used to determine what program to run.

If the file is modified or not saved, save it automatically before run.

URL `http://ergoemacs.org/emacs/elisp_run_current_file.html'
Version 2018-07-01"
  (interactive)
  (let (
        ($outputb "*run output*")
        (resize-mini-windows nil)
        ($suffix-map
         ;; (‹extension› . ‹shell program name›)
         `(
           ("php" . "php")
           ("pl" . "perl")
           ("py" . "python")
           ("py3" . ,(if (string-equal system-type "windows-nt") "c:/Python32/python.exe" "python3"))
           ("rb" . "ruby")
           ("go" . "go run")
           ("hs" . "runhaskell")
           ("js" . "node")
           ("mjs" . "node --experimental-modules ")
           ("ts" . "tsc") ; TypeScript
           ("tsx" . "tsc")
           ("sh" . "bash")
           ("clj" . "java -cp ~/apps/clojure-1.6.0/clojure-1.6.0.jar clojure.main")
           ("rkt" . "racket")
           ("ml" . "ocaml")
           ("vbs" . "cscript")
           ("tex" . "pdflatex")
           ("latex" . "pdflatex")
           ("java" . "javac")
           ("hy" . "hy")
           ;; ("pov" . "/usr/local/bin/povray +R2 +A0.1 +J1.2 +Am2 +Q9 +H480 +W640")
           ))
        $fname
        $fSuffix
        $prog-name
        $cmd-str)
    (when (not (buffer-file-name)) (save-buffer))
    (when (buffer-modified-p) (save-buffer))
    (setq $fname (buffer-file-name))
    (setq $fSuffix (file-name-extension $fname))
    (setq $prog-name (cdr (assoc $fSuffix $suffix-map)))
    (setq $cmd-str (concat $prog-name " \""   $fname "\""))
    (cond
     ((string-equal $fSuffix "el")
      (load $fname))
     ((or (string-equal $fSuffix "ts") (string-equal $fSuffix "tsx"))
      (if (fboundp 'ts-compile-file)
          (ts-compile-file current-prefix-arg)
        (if $prog-name
            (progn
              (message "Running")
              (shell-command $cmd-str $outputb ))
          (message "No recognized program file suffix for this file."))))
     ((string-equal $fSuffix "go")
      ;; (when (fboundp 'gofmt) (gofmt) )
      (shell-command $cmd-str $outputb ))
     ((string-equal $fSuffix "java")
      (progn
        (shell-command (format "java %s" (file-name-sans-extension (file-name-nondirectory $fname))) $outputb )))
     (t (if $prog-name
            (progn
              (message "Running")
              (shell-command $cmd-str $outputb ))
          (message "No recognized program file suffix for this file."))))))

(defun edit-with-sudo ()
  (interactive)
  (setq use-dialog-box nil)
  (setq filename (read-file-name "Enter file name (with path):"))
  (find-file (concat "/sudo::" filename)))

;;(require 'discover)
;;(global-discover-mode 1)

(defun run-buffer ()
  (interactive)
  (save-buffer)
  (shell-command (concat "python " buffer-file-name)))
(global-set-key (kbd "<f9>") 'run-buffer)


;(add-hook 'org-mode-hook (lambda () (linum-mode 0)))

(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
            '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

(defun gtl (n)
  (interactive "nHow many lines?: ")
  (forward-line (+ (line-number-at-pos) n)))

;;*******************
;;Copy Matching Lines
;;*******************
(defun copy-lines-matching-re (re)
  "find all lines matching the regexp RE in the current buffer
putting the matching lines in a buffer named *matching*"
  (interactive "sRegexp to match: ")
  (let ((result-buffer (get-buffer-create "*matching*")))
    (with-current-buffer result-buffer
      (erase-buffer))
    (save-match-data
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward re nil t)
          (princ (buffer-substring-no-properties (line-beginning-position)
                                                 (line-beginning-position 2))
                 result-buffer))))
    (pop-to-buffer result-buffer)))


(defun gtlu (n)
  (interactive "nHow many lines up?: ")
  (forward-line (+ (line-number-at-pos) (* n -1))))

(defun gtld (n)
  (interactive "cHow many lines down?: ")
  (forward-line (+ (line-number-at-pos) (char-to-string n))))

(defun jump-to-char (arg char)
  (interactive "p\ncJump to char: ")
  (forward-char)
  (let ((case-fold-search nil)); not sure if I want this
    (search-forward (char-to-string char) nil nil arg))
  (forward-char -1))

;; ********************************************************************
(defun duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (forward-line 1)
  (yank)
  )


(global-set-key (kbd "C-d") 'duplicate-line)
;; ********************************************************************
;; Reopen the last killed buffer
;; Source: https://stackoverflow.com/questions/10394213/emacs-reopen-previous-killed-buffer
(with-no-warnings
  (require 'cl-lib))
(require 'recentf)
(recentf-mode 1)
(defun undo-kill-buffer ()
  (interactive)
  (let ((active-files (loop for buf in (buffer-list)
                            when (buffer-file-name buf) collect it)))
    (loop for file in recentf-list
          unless (member file active-files) return (find-file file))))


;; **************************************************************************
;; SOURCE - https://coderwall.com/p/wsbdww/a-quick-easy-buffer-menu-for-emacs
;;***************************************************************************
;; used for quick menu switching between buffers
;;**********************************************
(defun menu-list-from-buffers ()
  (cons "PANE" (mapcar (lambda (e)  (list (buffer-name e) e)) (cl-remove-if (lambda (e)  (string-match "\*.*\*" (buffer-name e))  )(buffer-list))))
)

(defun menu-switch ()
  (interactive)
  (when (windowp (active-minibuffer-window))
    (abort-recursive-edit))
  (set-window-buffer nil (car (x-popup-menu t (list "Switch To Buffer" (menu-list-from-buffers)))))
  )
;; ********************************************************************
(defun sort-buffers ()
  "Put the buffer list in alphabetical order."
  (interactive)
  (dolist (buff (buffer-list-sorted)) (bury-buffer buff))
  (when (called-interactively-p (list-buffers))
    ))

(defun buffer-list-sorted ()
  (sort (buffer-list)
        (function
         (lambda
           (a b) (string<
                  (downcase (buffer-name a))
                  (downcase (buffer-name b))
                  )))))

(global-set-key "\M-b"    'sort-buffers)

;;**************
;;Writer Mode
;;**************
;;https://azer.bike/journal/ia-writer-mode-for-emacs
(defun writing-mode ()
  (interactive)
  (setq buffer-face-mode-face '(:family "dejavu sans mono" :height 150))
  (buffer-face-mode)
  (global-display-line-numbers-mode -1)
  (linum-mode 0)
  (writeroom-mode 1)
  (blink-cursor-mode)
  (visual-line-mode 1)
  (setq truncate-lines nil)
  (setq-default line-spacing 5)
  (setq global-hl-line-mode nil)
  )


;;  (call-process-shell-command "find ~/.emacs.d/My-emacs/backup/ -type f -name '*.zip' -mtime +7 -exec rm {} \\;" nil "*Shell Command*" t)

;; (call-process-shell-command  "cat ~/.emacs.d/init.el"  nil "*Shell Command Output*" t )

;; (if (y-or-n-p "Do it?")
;;     (progn
;;       ;; code to do something here
;;     )
;;   (progn
;;     ;; code if user answered no.
;;   )
;; )

;;************************
;;Format URL/Desc for org.
;;************************
(defun orgurl (x y)
  "Url + Desc in org format"
  (interactive "sUrl:
sDescription: ")
  (insert(concat "[[http://" x "][" y "]]"))
  )

(defun ffof (x y)
  "Url + Desc in org format"
  (interactive "sUrl:
sDescription: ")
  (insert(concat "[[elisp:(find-file-other-frame \"" x "\")][" y "]]"))
  )


(defun kill-shell-buffers ()
  (interactive)
  (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t)))
    (kill-matching-buffers "Shell")))

(defun my-quit-function()
  (interactive)
  (recentf-save-list)
  (save-buffers-kill-terminal))

;; **********************************************************************************************

(defun timestamp ()
   (interactive)
   (format-time-string "%Y-%m-%d-%H:%M:%S"))

(defun back-it-up ()
  (interactive)
  (defvar backup-prog)
  (defvar backup-loc)
  (defvar backup-files)
  (defvar zip-it)
  (setq backup-prog "zip -j ")
  (setq backup-loc (concat "~/.emacs.d/My-emacs/backup/backup-" (timestamp) ".zip"))
  (setq backup-files " ~/.emacs.d/My-emacs/*.el&")
  (setq zip-it (concat backup-prog backup-loc backup-files))
  ;;(shell-command zip-it)
  (call-process-shell-command zip-it nil "*Shell Command*" t))

(defun my-cfg ()
  (interactive)
  (flyspell-mode 0)
  (back-it-up)
  (find-file "~/.emacs.d/init.el")
  (find-file "~/.emacs.d/custom-post.el")
  (find-file "~/.emacs.d/lisp/init-fira-ligatures.el")
  (find-file "~/.emacs.d/My-emacs/hyper-sc.el")
  (find-file "~/.emacs.d/My-emacs/functions.el")
  (find-file "~/.emacs.d/My-emacs/keychords-sc.el")
  (find-file "/home/steve/.config/autostart-scripts/plasma-xcape.sh")
  (flyspell-mode 1)
  (switch-to-buffer "init.el"))

(defvar me/bookmarks
  '(("github" . "https://github.com")
    ("Manjaro Forum" . "https://forum.manjaro.org")
    ("IMDB" . " https://www.imdb.com")
    ))

(defun me/raven-bookmarks ()
  (raven-source "Bookmarks"
                me/bookmarks
                '(browse-url)))

(defun me/raven-browser-actions ()
  (raven-source
   "Other"
   '((ddg . "Search DuckDuckGo")
     (browse . "Browse URL"))
   (list (lambda (a)
           (cond ((eq a 'ddg)
                  (browse-url (concat "https://duckduckgo.com/?q="
                                      (raven-input))))
                 ((eq a 'browse)
                  (browse-url (raven-input))))))))

(defun me/browser ()
  (interactive)
  (raven (list (me/raven-bookmarks)
               (me/raven-browser-actions))))


(defun move-line-down ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines 1))
    (forward-line)
    (move-to-column col)))

(defun move-line-up ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines -1))
    (forward-line -1)
    (move-to-column col)))

;; --------------------------------------------------------------------------------------------------
(defvar bm-commands
  '("Bookmark Actions:"
    (""
     ("Toggle"          "bm-toggle")
     ("Next"            "bm-next")
     ("Previous"        "bm-previous")
     ("Annotate"        "bm-bookmark-annotate")
     ("Show Annotation" "bm-bookmark-show-annotation")
     ("Show all"        "bm-show")
     )))

(defun bm-menu ()
  "Run a Bookmark command from bm-commands(keychord bb):"
  (interactive)
  (let ((cmd
         (car (x-popup-menu
               (list (selected-window))
               bm-commands))))
    (funcall (intern cmd))))


;; **********************************************************************************************
;; ==================
;; kill-ring popup ;;
;; ==================
(require 'popup)
(require 'pos-tip)
(require 'popup-kill-ring)
;;(global-set-key "\M-y" 'popup-kill-ring)

(defun revert-all-no-confirm ()
  "Revert all file buffers, without confirmation.
Buffers visiting files that no longer exist are ignored.
Files that are not readable (including do not exist) are ignored.
Other errors while reverting a buffer are reported only as messages."
  (interactive)
  (let (file)
    (dolist (buf  (buffer-list))
      (setq file  (buffer-file-name buf))
      (when (and file  (file-readable-p file)) ;; and
        (with-current-buffer buf
          (with-demoted-errors "Error: %S" (revert-buffer t t)))))))

(revert-all-no-confirm)
;; --------------------------------------------------------------------------------------------------

(defun region-convert (begin end callback &rest args)
  "Convert string in region(BEGIN to END) by `CALLBACK' function call with additional arguments `ARGS'."
  (interactive "r\na")
  (let ((region-string (buffer-substring-no-properties begin end))
        result)
    (setq result (apply callback region-string args))
    (if (null result)
        (error "Convert Error")
      (save-excursion
        (goto-char end)
        (delete-region begin end)
        (insert result)
        (set-mark begin)))))

(provide 'region-convert)
;;; region-convert.el ends here
;; --------------------------------------------------------------------------------------------------
(require 'backup-each-save)
(add-hook 'after-save-hook 'backup-each-save)
(defun backup-each-save-filter (filename)
  (let ((ignored-filenames
         '("^/tmp" "semantic.cache$" "filesets-cache$" "treemacs-persist-$"  "\\.emacs-places$"
           "\\.recentf$" "\\recentf$" ".newsrc\\(\\.eld\\)?"))
        (matched-ignored-filename nil))
    (mapc
     (lambda (x)
       (when (string-match x filename)
         (setq matched-ignored-filename t)))
     ignored-filenames)
    (not matched-ignored-filename)))
(setq backup-each-save-filter-function 'backup-each-save-filter)
;;; Saved to ~/.backups/

;; ===|> ===|> ===|> ===|> ===|> ===|> ===|> ===|> ===|> ===|> ===|> ===|> ===|> ===|> ===|> ===|> ===|> ===|> ===|>



;; ===|> ===|> ===|> ===|> ===|> ===|> ===|> ===|> ===|> ===|> ===|> ===|> ===|> ===|> ===|> ===|> ===|> ===|> ===|>
(defun timed-updater ()
  (interactive)
  (setq counter 1)
  (setq filename "~/.timed-runner")
  (setq fexist (f-exists? filename)) ;; True/False

  ;; Create file if it doesn't exist (for future use), will also run payload:
  (if (not fexist)
      (f-write-text "%Y000" 'utf-8 filename))

  ;; Read file contents and convert to number, create comparison variable:
  (setq lastrun (string-to-number (f-read-text filename 'utf-8)))
  (setq yearday (string-to-number (format-time-string "%Y%j")))

  ;; Calculate days from file creation:
  (setq days (- yearday  lastrun))

  (if (> days counter)
      (progn
        ;; Payload:
        (shell-command "~/dotfiles/scripts/mybackup")
        (shell-command "touch ~/.mybackup/date_run")
        ;;(all-the-icons-install-fonts)
        ;; Reset flag file:
        (f-write-text (format-time-string "%Y%j") 'utf-8 filename)))
  )

(defun opacity-modify (&optional dec)
"modify the transparency of the emacs frame; if DEC is t, decrease the transparency, otherwise increase it in 10%-steps"
  (let* ((alpha-or-nil (frame-parameter nil 'alpha)) ; nil before setting
         (oldalpha (if alpha-or-nil alpha-or-nil 100))
         (newalpha (if dec (- oldalpha 10) (+ oldalpha 10))))
    (when (and (>= newalpha frame-alpha-lower-limit) (<= newalpha 100))
      (modify-frame-parameters nil (list (cons 'alpha newalpha))))))

;; C-8 will increase opacity  decrease transparency
;; C-9 will decrease opacity  increase transparency
;; C-0 will returns the state to normal
(global-set-key (kbd "C-8") '(lambda()(interactive)(opacity-modify)))
(global-set-key (kbd "C-9") '(lambda()(interactive)(opacity-modify t)))
(global-set-key (kbd "C-0") '(lambda()(interactive)
                               (modify-frame-parameters nil `((alpha . 100)))))

;; ===|> ===|> ===|> ===|> ===|> ===|> ===|> ===|> ===|> ===|> ===|> ===|> ===|> ===|> ===|> ===|> ===|> ===|> ===|>
;;; End functions!!
;; ===|> ===|> ===|> ===|> ===|> ===|> ===|> ===|> ===|> ===|> ===|> ===|> ===|> ===|> ===|> ===|> ===|> ===|> ===|>
