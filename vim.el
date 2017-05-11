;; -*- lexical-binding: t -*-

;; Utility predicates

(defun vim-alnum-p (c)
  (string-match "[a-zA-Z_0-9]" (char-to-string c)))

(defun vim-punct-p (c)
  (string-match "[!-/:-@\[-^`{-~]" (char-to-string c)))

(defun vim-at-bol ()
  (= (point) (line-beginning-position)))

(defun vim-at-eol ()
  (= (point) (line-end-position)))

;; Global variables
;(defvar vim-goal-column 0)
(defvar vim-prefix-arg-motion nil)
(defvar vim-last-search-motion-command nil)
(defvar vim-last-search-opposite-motion-command nil)
(defvar vim-last-prefix-arg nil)
(defvar vim-last-repeatable-command nil)
(defvar vim-insert-map (make-sparse-keymap))
(defvar vim-normal-map (make-sparse-keymap))

;; Utility procedures
(defun vim-move-to-indent ()
  (goto-char (line-beginning-position))
  (skip-chars-forward " \t"))

(defun vim-forward-to-char (c)
  ;; look for c in range (point+1, eol)
  ;; throws error if not found
  (let ((saved-point (point))) ; lest we fail
    (when (and (/= (point) (point-max)) (/= (char-after) 10))
      (forward-char))
    (while (and (/= (point) (point-max)) (/= (char-after) 10) (/= (char-after) c))
      (forward-char))
    (unless (= (char-after) c)
      (goto-char saved-point)
      (error "vim-forward-to-char: '%c' not found" c)))
  t)

(defun vim-backward-till-char (c)
  ;; look for c in range (bol, point)
  ;; throws error if not found
  (let ((saved-point (point)))
    (while (and (/= (point) (point-min)) (/= (char-before) 10) (/= (char-before) c))
      (backward-char))
    (unless (= (char-before) c)
      (goto-char saved-point)
      (error "vim-backward-to-char: '%c' not found" c)))
  t)

(defun vim-search-forward (regexp)
  (save-excursion
    (forward-char)
    ;; this sets point at end of match
    (search-forward-regexp regexp))
  ;; so go to beginning of match
  (goto-char (match-beginning 0)))

(defun vim-search-backward (regexp)
  (search-backward-regexp regexp))

;; Goal column related routines

(defun vim-set-goal-column (col)
  (setq temporary-goal-column
	(cond
	 ((consp temporary-goal-column)
	  (cons col (cdr temporary-goal-column)))
	 ((integerp temporary-goal-column)
	  temporary-goal-column)
	 ((floatp temporary-goal-column)
	  (truncate temporary-goal-column)))))

(defun vim-update-goal-column ()
  (vim-set-goal-column (current-column)))

(defun vim-move-to-goal-column ()
  (let ((n
	 (if (consp temporary-goal-column)
	     (+ (car temporary-goal-column) (cdr temporary-goal-column))
	   temporary-goal-column)))
    (move-to-column
     (if (integerp n) n (truncate n)))))

;; Motions

(defun vim-motion-up (&optional n)
  (unless n (setq n 1))
  (let ((saved-point (point)))
    (forward-line (- n))
    (vim-move-to-goal-column)
    (< (point) saved-point)))

(put 'vim-motion-up 'linewise t)
(put 'vim-motion-up 'preserve-goal-column t)

(defun vim-motion-down (&optional n)
  (unless n (setq n 1))
  (let ((saved-point (point)))
    (forward-line n)
    (vim-move-to-goal-column)
    (> (point) saved-point)))

(put 'vim-motion-down 'linewise t)
(put 'vim-motion-down 'preserve-goal-column t)

(defun vim-motion-left (&optional n)
  (unless n (setq n 1))
  (let* ((bol (line-beginning-position))
	 (dest (max bol (- (point) n))))
    (if (< dest (point))
	(progn
	  (goto-char dest)
	  t)
      nil)))

(defun vim-motion-right (&optional n)
  (unless n (setq n 1))
  (let* ((adjusted-eol (line-end-position))
	 (dest (min adjusted-eol (+ (point) n))))
    (if (> dest (point))
	(progn
	  (goto-char dest)
	  t)
      nil)))

(defun vim-motion-indent (n)
  ;; `n` is ignored
  (vim-move-to-indent)
  t)

(defun vim-motion-bol (&optional n)
  ;; `n` is ignored
  (beginning-of-line)
  (vim-update-goal-column)
  t)

(defun vim-motion-eol (&optional n)
  (unless n (setq n 1))
  (goto-char (line-end-position n))
  (vim-set-goal-column most-positive-fixnum)
  t)

(put 'vim-motion-eol 'preserve-goal-column t)

(defun vim-motion-forward-word (&optional n)
  (unless n (setq n 1))
  (dotimes (i n)
    (let ((c (char-after)))
      (cond
       ((vim-alnum-p c) (skip-chars-forward "a-zA-Z_0-9"))
       ((vim-punct-p c) (skip-chars-forward "!-/:-@[-^`{-~"))
       ((= c 10) (forward-char)))
      (skip-chars-forward " \t")))
  t)

(defun vim-motion-forward-big-word (n)
  (unless n (setq n 1))
  (dotimes (i n)
    (let ((c (char-after)))
      (cond
       ((or (vim-alnum-p c) (vim-punct-p c)) (skip-chars-forward "a-zA-Z_0-9!-/:-@[-^`{-~"))
       ((= c 10) (forward-char)))
      (skip-chars-forward " \t")))
  t)

(defun vim-motion-end-of-word (n)
  (unless n (setq n 1))
  (dotimes (i n)
    (skip-chars-forward " \t")
    (let ((c (char-after)))
      (cond
       ((vim-alnum-p c) (skip-chars-forward "a-zA-Z_0-9"))
       ((vim-punct-p c) (skip-chars-forward "!-/:-@[-^`{-~"))
       ((= c 10) (forward-char)))))
  t)

(defun vim-motion-end-of-big-word (n)
  (unless n (setq n 1))
  (dotimes (i n)
    (skip-chars-forward " \t")
    (let ((c (char-after)))
      (cond
       ((or (vim-alnum-p c) (vim-punct-p c)) (skip-chars-forward "a-zA-Z_0-9!-/:-@[-^`{-~"))
       ((= c 10) (forward-char)))))
  t)

(defun vim-motion-backward-word (n)
  (unless n (setq n 1))
  (dotimes (i n)
    (skip-chars-backward " \t")
    (let ((c (char-before)))
      (cond
       ((vim-alnum-p c) (skip-chars-backward "a-zA-Z_0-9"))
       ((vim-punct-p c) (skip-chars-backward "!-/:-@[-^`{-~"))
       ((= c 10) (backward-char)))))
  t)

(defun vim-motion-backward-big-word (n)
  (unless n (setq n 1))
  (dotimes (i n)
    (skip-chars-backward " \t")
    (let ((c (char-before)))
      (cond
       ((or (vim-alnum-p c) (vim-punct-p c)) (skip-chars-backward "a-zA-Z_0-9!-/:-@[-^`{-~"))
       ((= c 10) (backward-char)))))
  t)

(defun vim-motion-forward-paragraph (&optional n)
  (forward-paragraph n)
  t)

(defun vim-motion-backward-paragraph (&optional n)
  (backward-paragraph n)
  t)

(defun vim-motion-forward-sentence (&optional n)
  (forward-sentence n)
  t)

(defun vim-motion-backward-sentence (&optional n)
  (backward-sentence n)
  t)

(defun vim-motion-forward-to-char (&optional n)
  (unless n (setq n 1))
  (let ((c (read-char)))
    (dotimes (i n)
      (vim-forward-to-char c)))
  t)

(defun vim-motion-backward-till-char (&optional n)
  (unless n (setq n 1))
  (let ((c (read-char)))
    (dotimes (i n)
      (vim-backward-till-char c)))
  t)

(put 'vim-motion-forward-to-char 'inclusive t)

(defun vim-motion-forward-till-char (&optional n)
  (vim-motion-forward-to-char n)
  (backward-char)
  t)

(defun vim-motion-backward-to-char (&optional n)
  (vim-motion-backward-till-char n)
  (backward-char)
  t)

(put 'vim-motion-forward-till-char 'inclusive t)

(defun vim-motion-search-forward (&optional n)
  (let ((input-regexp (read-from-minibuffer "/")))
    (setq vim-last-search-motion-command
	  (lambda () (vim-search-forward input-regexp))
	  vim-last-search-opposite-motion-command
	  (lambda () (vim-search-backward input-regexp)))
    (dotimes (i (or n 1)) (funcall vim-last-search-motion-command)))
  t)

(defun vim-motion-search-backward (&optional n)
  (let ((input-regexp (read-from-minibuffer "?")))
    (setq vim-last-search-motion-command
	  (lambda () (vim-search-backward input-regexp))
	  vim-last-search-opposite-motion-command
	  (lambda () (vim-search-forward input-regexp)))
    (dotimes (i (or n 1)) (funcall vim-last-search-motion-command)))
  t)

(defun vim-motion-search-next (&optional n)
  (unless vim-last-search-motion-command
    (error "no previous regular expression"))
  (dotimes (i (or n 1)) (funcall vim-last-search-motion-command))
  t)

(defun vim-motion-search-previous (&optional n)
  (unless vim-last-search-opposite-motion-command
    (error "no previous regular expression"))
  (dotimes (i (or n 1)) (funcall vim-last-search-opposite-motion-command))
  t)

(defun vim-motion-goto-line (&optional n)
  ;;(message "vim-motion-goto-line: %S" n)
  ;; stop at the first non-blank character on that line
  (if n
      (goto-line n)
    (goto-char (point-max)))
  (vim-move-to-indent)
  t)

(put 'vim-motion-goto-line 'linewise t)

(defun vim-motion-first-line (n)
  (goto-char (point-min))
  (vim-move-to-indent)
  t)

(put 'vim-motion-first-line 'linewise t)

(defun vim-motion-column (&optional n)
  (let ((col (1- (or n 1))))
    (move-to-column col)
    (vim-set-goal-column col))
  t)

(defun vim-motion-current-line (&optional n)
  t)

(put 'vim-motion-current-line 'linewise t)

(defun vim-motion-window-start (n)
  ;; `n` is ignored
  (goto-char (window-start))
  (vim-move-to-indent)
  t)

(defun vim-motion-window-end (n)
  ;; `n` is ignored
  (goto-char (window-start))
  (forward-line (1- (window-body-height)))
  (vim-move-to-indent)
  t)

(defun vim-motion-window-middle (n)
  ;; `n` is ignored
  (goto-char (window-start))
  (let* ((end (min (window-end) (point-max)))
	 (lines (count-lines (point) end)))
    (forward-line (/ lines 2)))
  (vim-move-to-indent)
  t)

(defun vim-motion-forward-char (n)
  (if (< (point) (point-max))
      (progn
	(forward-char n)
	t)
    nil))

(defun vim-motion-backward-char (n)
  (if (> (point) (point-min))
      (progn
	(backward-char n)
	t)
    nil))

;; Editing commands

(defun vim-erase-word ()
  (interactive)
  (let ((c (char-before)))
    (if (= c 10) ; at beginning of line
	(delete-char -1)
      (let ((p (point)))
	;; First, skip through any whitespace
	(when (string-match "[ \t]" (char-to-string c))
	  (skip-chars-backward " \t")
	  (setq c (char-before)))
	(cond
	 ((vim-alnum-p c)
	  (progn
	    (skip-chars-backward "a-zA-Z_0-9")
	    ))
	 ((vim-punct-p c)
	  (progn
	    (skip-chars-backward "!-/:-@[-^`{-~")
	    )))
	(kill-region (point) p)))))

(defun vim-erase-line ()
  (interactive)
  (if (= (char-before) 10)
      ;; at beginning of line
      (delete-char -1)
    ;; not at beginning of line
    (let ((c (char-before)) (p (point)))
      (if
	  ;; current line consists solely of spaces and tabs
	  (progn
	    (skip-chars-backward " \t")
	    (= (line-beginning-position) (point)))
	  ;; then
	  ;; already at bol, do nothing
	  nil
	;; else
	(progn
	  (goto-char (line-beginning-position))
	  (skip-chars-forward " \t")))
      (kill-region (point) p))))

(defun vim-open-line ()
  (interactive)
  (goto-char (line-end-position))
  (insert "\n")
  (indent-for-tab-command)
  (vim-insert-mode))

(defun vim-open-line-before ()
  (interactive)
  (goto-char (line-beginning-position))
  (save-excursion (insert "\n"))
  (indent-for-tab-command)
  (vim-insert-mode))

(defun vim-operator-change (begin end)
  (goto-char begin)
  (vim-operator-delete begin end)
  (vim-insert-mode))

(defun vim-operator-delete (begin end)
  (kill-region begin end))

;; enter normal mode
(defun vim-escape ()
  (interactive)
  (setq cursor-type 'box)
  (use-local-map vim-normal-map)
  (vim-update-goal-column))

(defun vim-insert-mode ()
  (interactive)
  (setq cursor-type 'bar)
  (use-local-map vim-insert-map))

(defun vim-insert (arg)
  (interactive "P")
  (vim-insert-mode))

(defun vim-insert-indent ()
  (interactive)
  (vim-move-to-indent)
  (vim-insert-mode))

(defun vim-append-eol ()
  (interactive)
  (goto-char (line-end-position))
  (vim-insert-mode))

(define-key vim-insert-map (kbd "C-u") 'vim-erase-line)
(define-key vim-insert-map (kbd "C-w") 'vim-erase-word)
;; breaks M- keys
(define-key vim-insert-map (kbd "ESC") 'vim-escape)

(defun vim-combined-prefix-arg (arg)
  (if arg
      (if vim-prefix-arg-motion
	  (* arg vim-prefix-arg-motion)
	arg)
    vim-prefix-arg-motion))

(defun vim-compose-command (action motion)
  (if action
      ;; action is non-nil
      (if (get motion 'linewise)
	  ;; line-wise motion
	  (lambda (arg)
	    (interactive "P")
	    ;;(message (format "line-wise motion, prefix arg is %d" arg))
	    (setq vim-last-prefix-arg (vim-combined-prefix-arg arg)
		  vim-prefix-arg-motion nil
		  vim-last-repeatable-command
		  (lambda (arg)
		    (let ((saved-point (point))
			  (saved-bol (line-beginning-position))
			  (saved-eol (line-end-position)))
		      (when (funcall motion arg)
			(if (<= saved-point (point))
			    ;; not moved, or moved forward
			    (progn
			      (end-of-line)
			      (unless (= (point) (point-max))
				(forward-char))
			      (funcall action saved-bol (point)))
			  ;; moved backward
			  (progn
			    (beginning-of-line)
			    (unless (= saved-eol (point-max))
			      (setq saved-eol (1+ saved-eol)))
			    (funcall action (point) saved-eol)))
			(vim-move-to-indent)
			(vim-update-goal-column)))))
	    (funcall vim-last-repeatable-command vim-last-prefix-arg))
	;; character-wise motion
	(if (get motion 'inclusive)
	    ;; inclusive motion
	    (lambda (arg)
	      (interactive "P")
	      (unless arg (setq arg 1))
	      ;;(message "inclusive char-wise motion, prefix arg is %d" arg)
	      (setq vim-last-prefix-arg (vim-combined-prefix-arg arg)
		    vim-prefix-arg-motion nil
		    vim-last-repeatable-command
		    (lambda (arg)
		      (let ((saved-point (point)))
			(when (funcall motion arg)
			  (unless (= (point) (point-max))
			    (forward-char))
			  (funcall action saved-point (point))
			  (vim-update-goal-column)))))
	      (funcall vim-last-repeatable-command vim-last-prefix-arg))
	  ;; exclusive motion
	  (lambda (arg)
	    (interactive "P")
	    (unless arg (setq arg 1))
	    ;;(message "exclusive char-wise motion, prefix arg is %d" arg)
	    (setq vim-last-prefix-arg (vim-combined-prefix-arg arg)
		  vim-prefix-arg-motion nil
		  vim-last-repeatable-command
		  (lambda (arg)
		    (let ((saved-point (point)))
		      (when (funcall motion arg)
			(funcall action saved-point (point))
			(vim-update-goal-column)))))
	    (funcall vim-last-repeatable-command vim-last-prefix-arg))))
    ;; action is nil
    (let ((update-goal-column-if-necessary
	   (if (get motion 'preserve-goal-column)
	       (lambda ())
	     (lambda () (vim-update-goal-column)))))
      (lambda (arg)
	(interactive "P")
	(when (funcall motion arg)
	  (funcall update-goal-column-if-necessary))))))

(defun vim-repeat-last-command (arg)
  (interactive "P")
  (when vim-last-repeatable-command
    (setq vim-last-prefix-arg (or arg vim-last-prefix-arg))
    (funcall vim-last-repeatable-command vim-last-prefix-arg)))

(defun vim-digit-argument-2 (map count)
  (lambda ()
    (interactive)
    (prefix-command-preserve-state)
    (setq prefix-arg (+ (* prefix-arg 10) count))
    (set-transient-map map)))

(defun vim-digit-argument (map count)
  (lambda ()
    (interactive)
    (prefix-command-preserve-state)
    (setq prefix-arg count)
    (let ((map-copy (copy-keymap map)))
      (define-key map-copy [?0] (vim-digit-argument-2 map-copy 0))
      (define-key map-copy [?1] (vim-digit-argument-2 map-copy 1))
      (define-key map-copy [?2] (vim-digit-argument-2 map-copy 2))
      (define-key map-copy [?3] (vim-digit-argument-2 map-copy 3))
      (define-key map-copy [?4] (vim-digit-argument-2 map-copy 4))
      (define-key map-copy [?5] (vim-digit-argument-2 map-copy 5))
      (define-key map-copy [?6] (vim-digit-argument-2 map-copy 6))
      (define-key map-copy [?7] (vim-digit-argument-2 map-copy 7))
      (define-key map-copy [?8] (vim-digit-argument-2 map-copy 8))
      (define-key map-copy [?9] (vim-digit-argument-2 map-copy 9))
      (set-transient-map map-copy)
      )))

(defun vim-digit-argument-motion-2 (map count)
  (lambda ()
    (interactive)
    (prefix-command-preserve-state)
    (setq vim-prefix-arg-motion (+ (* vim-prefix-arg-motion 10) count))
    (set-transient-map map)))

(defun vim-digit-argument-motion (map count)
  (lambda ()
    (interactive)
    (prefix-command-preserve-state)
    (setq vim-prefix-arg-motion count)
    (let ((map-copy (copy-keymap map)))
      (define-key map-copy [?0] (vim-digit-argument-motion-2 map-copy 0))
      (define-key map-copy [?1] (vim-digit-argument-motion-2 map-copy 1))
      (define-key map-copy [?2] (vim-digit-argument-motion-2 map-copy 2))
      (define-key map-copy [?3] (vim-digit-argument-motion-2 map-copy 3))
      (define-key map-copy [?4] (vim-digit-argument-motion-2 map-copy 4))
      (define-key map-copy [?5] (vim-digit-argument-motion-2 map-copy 5))
      (define-key map-copy [?6] (vim-digit-argument-motion-2 map-copy 6))
      (define-key map-copy [?7] (vim-digit-argument-motion-2 map-copy 7))
      (define-key map-copy [?8] (vim-digit-argument-motion-2 map-copy 8))
      (define-key map-copy [?9] (vim-digit-argument-motion-2 map-copy 9))
      (set-transient-map map-copy)
    )))

(defun vim-define-operator (op-key op-fn)
  (let ((op-map (if op-key (make-sparse-keymap) vim-normal-map)))
    (dolist
	(pair
	 '(
	   ("". vim-motion-backward-char)
	   (" " . vim-motion-forward-char)
	   ("$" . vim-motion-eol)
	   ("(" . vim-motion-backward-sentence)
	   (")" . vim-motion-forward-sentence)
	   ("/" . vim-motion-search-forward)
	   ("B" . vim-motion-backward-big-word)
	   ("E" . vim-motion-end-of-big-word)
	   ("F" . vim-motion-backward-to-char)
	   ("G" . vim-motion-goto-line)
	   ("H" . vim-motion-window-start)
	   ("L" . vim-motion-window-end)
	   ("M" . vim-motion-window-middle)
	   ("W" . vim-motion-forward-big-word)
	   ("0" . vim-motion-bol)
	   ("T" . vim-motion-backward-till-char)
	   ("?" . vim-motion-search-backward)
	   ("N" . vim-motion-search-previous)
	   ("^" . vim-motion-indent)
	   ("b" . vim-motion-backward-word)
	   ("e" . vim-motion-end-of-word)
	   ("f" . vim-motion-forward-to-char)
	   ("gg". vim-motion-first-line)
	   ("h" . vim-motion-left)
	   ("j" . vim-motion-down)
	   ("k" . vim-motion-up)
	   ("l" . vim-motion-right)
	   ("n" . vim-motion-search-next)
	   ("t" . vim-motion-forward-till-char)
	   ("w" . vim-motion-forward-word)
	   ("{" . vim-motion-backward-paragraph)
	   ("|" . vim-motion-column)
	   ("}" . vim-motion-forward-paragraph)
	   ))
      (let ((motion-key (car pair)) (motion-fn (cdr pair)))
	(define-key op-map motion-key (vim-compose-command op-fn motion-fn))))
    (when op-key
      (define-key op-map op-key (vim-compose-command op-fn 'vim-motion-current-line)))
    (if op-key
	(progn
	  (define-key op-map "1" (vim-digit-argument-motion op-map 1))
	  (define-key op-map "2" (vim-digit-argument-motion op-map 2))
	  (define-key op-map "3" (vim-digit-argument-motion op-map 3))
	  (define-key op-map "4" (vim-digit-argument-motion op-map 4))
	  (define-key op-map "5" (vim-digit-argument-motion op-map 5))
	  (define-key op-map "6" (vim-digit-argument-motion op-map 6))
	  (define-key op-map "7" (vim-digit-argument-motion op-map 7))
	  (define-key op-map "8" (vim-digit-argument-motion op-map 8))
	  (define-key op-map "9" (vim-digit-argument-motion op-map 9))
	  )
      (progn
	(define-key op-map "1" (vim-digit-argument op-map 1))
	(define-key op-map "2" (vim-digit-argument op-map 2))
	(define-key op-map "3" (vim-digit-argument op-map 3))
	(define-key op-map "4" (vim-digit-argument op-map 4))
	(define-key op-map "5" (vim-digit-argument op-map 5))
	(define-key op-map "6" (vim-digit-argument op-map 6))
	(define-key op-map "7" (vim-digit-argument op-map 7))
	(define-key op-map "8" (vim-digit-argument op-map 8))
	(define-key op-map "9" (vim-digit-argument op-map 9))
	))
    (when op-key
      (define-key vim-normal-map op-key op-map))
    ))

(vim-define-operator nil nil)
(vim-define-operator "c" 'vim-operator-change)
(vim-define-operator "d" 'vim-operator-delete)

;; Bind keys to commands
(define-key vim-normal-map "A" 'vim-append-eol)
(define-key vim-normal-map "I" 'vim-insert-indent)
(define-key vim-normal-map "O" 'vim-open-line-before)
(define-key vim-normal-map "P" 'yank)
(define-key vim-normal-map "i" 'vim-insert)
(define-key vim-normal-map "o" 'vim-open-line)
(define-key vim-normal-map "." 'vim-repeat-last-command)

;; Alias commands
(define-key vim-normal-map "C" (lookup-key vim-normal-map "c$"))
(define-key vim-normal-map "D" (lookup-key vim-normal-map "d$"))
(define-key vim-normal-map "S" (lookup-key vim-normal-map "cc"))
(define-key vim-normal-map "X" (lookup-key vim-normal-map "dh"))
(define-key vim-normal-map "x" (lookup-key vim-normal-map "dl"))

;;;###autoload
(defun vim-mode ()
  (interactive)
  (use-local-map vim-normal-map))
