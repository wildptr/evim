;; -*- lexical-binding: t -*-

(defun vim-alnum-p (c)
  (string-match "[a-zA-Z_0-9]" (char-to-string c)))

(defun vim-punct-p (c)
  (string-match "[!-/:-@\[-^`{-~]" (char-to-string c)))

(defun vim-at-bol ()
  (= (point) (line-beginning-position)))

(defun vim-at-eol ()
  (= (point) (line-end-position)))

(defvar vim-goal-column 0)

(defun vim-update-goal-column ()
  (setq vim-goal-column (current-column)))

(defun vim-move-to-goal-column ()
  (move-to-column vim-goal-column)
  (when (and (= (point) (line-end-position)) (> (point) (line-beginning-position)))
    (backward-char)))

;; FIXME cursor should always stay on last character when last motion is '$'
(defun vim-motion-up (&optional n)
  (unless n (setq n 1))
  (forward-line (- n))
  (vim-move-to-goal-column))

(put 'vim-motion-up 'linewise t)

(defun vim-motion-down (&optional n)
  (unless n (setq n 1))
  (forward-line n)
  (vim-move-to-goal-column))

(defun vim-motion-left (&optional n)
  (unless n (setq n 1))
  (let* ((bol (line-beginning-position))
	 (dest (max bol (- (point) n))))
    (when (< dest (point))
      (goto-char dest)
      (vim-update-goal-column))))

(defun vim-motion-right (&optional n)
  (unless n (setq n 1))
  (let* ((eol (line-end-position))
	 (dest (min eol (+ (point) n))))
    (when (> dest (point))
      (goto-char dest)
      (vim-update-goal-column))))

(put 'vim-motion-down 'linewise t)

(defun vim-motion-indent (&optional n)
  ;; `n` is ignored
  (goto-char (line-beginning-position))
  (skip-chars-forward " \t")
  (vim-update-goal-column))

(defun vim-motion-bol (&optional n)
  ;; `n` is ignored
  (beginning-of-line)
  (vim-update-goal-column))

(defun vim-motion-eol (&optional n)
  (unless n (setq n 1))
  (goto-char (line-end-position n))
  (unless (= (point) (line-beginning-position))
    (backward-char))
  (setq vim-goal-column most-positive-fixnum))

(put 'vim-motion-eol 'inclusive t)

(defun vim-motion-forward-word (&optional n)
  (unless n (setq n 1))
  (dotimes (i n)
    (let ((c (char-after)))
      (cond
       ((vim-alnum-p c) (skip-chars-forward "a-zA-Z_0-9"))
       ((vim-punct-p c) (skip-chars-forward "!-/:-@[-^`{-~"))
       )
      ;; FIXME
      (skip-chars-forward " \t\n")))
  (vim-update-goal-column))

(defun vim-motion-forward-paragraph (&optional n)
  (forward-paragraph n)
  (vim-update-goal-column))

(defun vim-motion-backward-paragraph (&optional n)
  (backward-paragraph n)
  (vim-update-goal-column))
    
(defun vim-motion-forward-sentence (&optional n)
  (forward-sentence n)
  (vim-update-goal-column))

(defun vim-motion-backward-sentence (&optional n)
  (backward-sentence n)
  (vim-update-goal-column))

(defun vim-forward-to-char (c)
  ;; look for c in range (point+1, eol)
  ;; throws error if not found
  (when (and (/= (point) (point-max)) (/= (char-after) 10))
    (forward-char))
  (while (and (/= (point) (point-max)) (/= (char-after) 10) (/= (char-after) c))
    (forward-char))
  (unless (= (char-after) c)
    (error "vim-forward-to-char: '%c' not found" c)))

(defun vim-backward-till-char (c)
  ;; look for c in range (bol, point)
  ;; throws error if not found
  (while (and (/= (point) (point-min)) (/= (char-before) 10) (/= (char-before) c))
    (backward-char))
  (unless (= (char-before) c)
    (error "vim-backward-to-char: '%c' not found" c)))

(defun vim-motion-forward-to-char (&optional n)
  (unless n (setq n 1))
  (let ((c (read-char)))
    (dotimes (i n)
      (vim-forward-to-char c)))
  (vim-update-goal-column))

(defun vim-motion-backward-till-char (&optional n)
  (unless n (setq n 1))
  (let ((c (read-char)))
    (dotimes (i n)
      (vim-backward-till-char c)))
  (vim-update-goal-column))

(put 'vim-motion-forward-to-char 'inclusive t)

(defun vim-motion-forward-till-char (&optional n)
  (vim-motion-forward-to-char n)
  (backward-char)
  (vim-update-goal-column))

(defun vim-motion-backward-to-char (&optional n)
  (vim-motion-backward-till-char n)
  (backward-char)
  (vim-update-goal-column))

(put 'vim-motion-forward-till-char 'inclusive t)

(defvar vim-prefix-arg-motion nil)
(defvar vim-last-search-motion-command)
(defvar vim-last-search-opposite-motion-command)
(defvar vim-last-repeatable-command)

(defun vim-search-forward (regexp)
  (forward-char)
  ;; this sets point at end of match
  (search-forward-regexp regexp)
  ;; so go to beginning of match
  (goto-char (match-beginning 0)))

(defun vim-search-backward (regexp)
  (search-backward-regexp regexp))

(defun vim-motion-search-forward (&optional n)
  (let ((input-regexp (read-from-minibuffer "/")))
    (setq vim-last-search-motion-command
	  (lambda () (vim-search-forward input-regexp))
	  vim-last-search-opposite-motion-command
	  (lambda () (vim-search-backward input-regexp)))
    (dotimes (i (or n 1)) (funcall vim-last-search-motion-command)))
  (vim-update-goal-column))

(defun vim-motion-search-backward (&optional n)
  (let ((input-regexp (read-from-minibuffer "?")))
    (setq vim-last-search-motion-command
	  (lambda () (vim-search-backward input-regexp))
	  vim-last-search-opposite-motion-command
	  (lambda () (vim-search-forward input-regexp)))
    (dotimes (i (or n 1)) (funcall vim-last-search-motion-command)))
  (vim-update-goal-column))

(defun vim-motion-search-next (&optional n)
  (unless vim-last-search-motion-command
    (error "no previous regular expression"))
  (dotimes (i (or n 1)) (funcall vim-last-search-motion-command))
  (vim-update-goal-column))

(defun vim-motion-search-previous (&optional n)
  (unless vim-last-search-opposite-motion-command
    (error "no previous regular expression"))
  (dotimes (i (or n 1)) (funcall vim-last-search-opposite-motion-command))
  (vim-update-goal-column))

(defun vim-motion-goto-line (&optional n)
  ;;(message "vim-motion-goto-line: %S" n)
  ;; stop at the first non-blank character on that line
  (if n
      (goto-line n)
    (goto-char (point-max)))
  (vim-motion-indent)) ; goal column already updated

(put 'vim-motion-goto-line 'linewise t)

(defun vim-motion-column (&optional n)
  (let ((col (1- (or n 1))))
    (move-to-column col)
    (setq vim-goal-column col)))

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

(defun vim-change (begin end)
  (goto-char begin)
  (vim-delete begin end)
  (vim-insert-mode))

(defun vim-delete (begin end)
  (kill-region begin end))

(defun vim-delete-line ()
  (interactive)
  (vim-delete (line-beginning-position) (+ (line-end-position) 1)))

;; enter normal mode
(defun vim-escape ()
  (interactive)
  (setq cursor-type 'box)
  (use-local-map vim-normal-map))

(defun vim-insert-mode ()
  (interactive)
  (setq cursor-type 'bar)
  (use-local-map vim-insert-map))

(defun vim-insert (arg)
  (interactive "P")
  (vim-insert-mode)
  )

(defun vim-insert-indent ()
  (interactive)
  (goto-char (vim-motion-indent))
  (vim-insert-mode))

(defun vim-append-eol ()
  (interactive)
  (goto-char (line-end-position))
  (vim-insert-mode))

(defvar vim-insert-map (make-sparse-keymap))
(define-key vim-insert-map (kbd "C-u") 'vim-erase-line)
(define-key vim-insert-map (kbd "C-w") 'vim-erase-word)
;; breaks M- keys
(define-key vim-insert-map (kbd "ESC") 'vim-escape)

(defun vim-repeat-last-command (arg)
  (interactive "P")
  (when vim-last-repeatable-command
    (dotimes (i (or arg 1))
      (funcall vim-last-repeatable-command))))

(defun vim-compose-command (action motion)
  (if action
      ;; action is non-nil
      (if (get motion 'linewise)
	  ;; line-wise motion
	  (lambda (arg)
	    (interactive "P")
	    (unless arg (setq arg 1))
	    ;;(message (format "line-wise motion, prefix arg is %d" arg))
	    (setq vim-last-repeatable-command
		  (let ((saved-arg vim-prefix-arg-motion))
		    (lambda ()
		      (let ((saved-point (point))
			    (saved-bol (line-beginning-position))
			    (saved-eol (line-end-position)))
			(funcall motion saved-arg)
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
			      (forward-char))
			    (funcall action (point) saved-eol)))))))
	    (dotimes (i arg) (funcall vim-last-repeatable-command))
	    (setq vim-prefix-arg-motion nil))
	;; character-wise motion
	(if (get motion 'inclusive)
	    ;; inclusive motion
	    (lambda (arg)
	      (interactive "P")
	      (unless arg (setq arg 1))
	      ;;(message "inclusive char-wise motion, prefix arg is %d" arg)
	      (setq vim-last-repeatable-command
		    (let ((saved-arg vim-prefix-arg-motion))
		      (lambda ()
			(let ((saved-point (point)))
			  (funcall motion saved-arg)
			  (unless (= (point) (point-max))
			    (forward-char))
			  (funcall action saved-point (point))))))
	      (dotimes (i arg) (funcall vim-last-repeatable-command))
	      (setq vim-prefix-arg-motion nil))
	  ;; exclusive motion
	  (lambda (arg)
	    (interactive "P")
	    (unless arg (setq arg 1))
	    ;;(message "exclusive char-wise motion, prefix arg is %d" arg)
	    (setq vim-last-repeatable-command
		  (let ((saved-arg vim-prefix-arg-motion))
		    (lambda ()
		      (let ((saved-point (point)))
			(funcall motion saved-arg)
			(funcall action saved-point (point))))))
	    (dotimes (i arg) (funcall vim-last-repeatable-command))
	    (setq vim-prefix-arg-motion nil))))
    ;; action is nil
    (lambda (arg)
      (interactive "P")
      (funcall motion arg))))

(defvar vim-normal-map (make-sparse-keymap))
(define-key vim-normal-map "A" 'vim-append-eol)
(define-key vim-normal-map "I" 'vim-insert-indent)
(define-key vim-normal-map "O" 'vim-open-line-before)
(define-key vim-normal-map "P" 'yank) ; ***
(define-key vim-normal-map "i" 'vim-insert)
(define-key vim-normal-map "o" 'vim-open-line)
(define-key vim-normal-map "." 'vim-repeat-last-command)

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

(defun vim-digit-argument-motion-with-map (map count)
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

(defun vim-define-operation (op-key op-fn)
  (let ((op-map (if op-key (make-sparse-keymap) vim-normal-map)))
    (dolist
	(pair
	 '(
	   ("$" . vim-motion-eol)
	   ("(" . vim-motion-backward-sentence)
	   (")" . vim-motion-forward-sentence)
	   ("/" . vim-motion-search-forward)
	   ("F" . vim-motion-backward-to-char)
	   ("G" . vim-motion-goto-line)
	   ("0" . vim-motion-bol)
	   ("T" . vim-motion-backward-till-char)
	   ("?" . vim-motion-search-backward)
	   ("N" . vim-motion-search-previous)
	   ("^" . vim-motion-indent)
	   ("f" . vim-motion-forward-to-char)
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
    (if op-key
	(progn
	  (define-key op-map "1" (vim-digit-argument-motion-with-map op-map 1))
	  (define-key op-map "2" (vim-digit-argument-motion-with-map op-map 2))
	  (define-key op-map "3" (vim-digit-argument-motion-with-map op-map 3))
	  (define-key op-map "4" (vim-digit-argument-motion-with-map op-map 4))
	  (define-key op-map "5" (vim-digit-argument-motion-with-map op-map 5))
	  (define-key op-map "6" (vim-digit-argument-motion-with-map op-map 6))
	  (define-key op-map "7" (vim-digit-argument-motion-with-map op-map 7))
	  (define-key op-map "8" (vim-digit-argument-motion-with-map op-map 8))
	  (define-key op-map "9" (vim-digit-argument-motion-with-map op-map 9))
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

(vim-define-operation nil nil)
(vim-define-operation "c" 'vim-change)
(vim-define-operation "d" 'vim-delete)

;;;###autoload
(defun vim-mode ()
  (interactive)
  (use-local-map vim-normal-map))
