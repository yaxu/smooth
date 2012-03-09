;; smooth.el - (c) alex@slab.org, 20012, based heavily on...
;; hsc3.el - (c) rohan drape, 2006-2008

;; notes from hsc3:
;; This mode is implemented as a derivation of `haskell' mode,
;; indentation and font locking is courtesy that mode.  The
;; inter-process communication is courtesy `comint'.  The symbol at
;; point acquisition is courtesy `thingatpt'.  The directory search
;; facilities are courtesy `find-lisp'.

(require 'scheme)
(require 'comint)
(require 'thingatpt)
(require 'find-lisp)

(defun smooth-highlight-tail-make-new-overlay (start-point
                                            end-point
                                            )
  "Make new highlight in the current point."
  (let* ((point-face-bgcolor-hex nil))
    ;; remove any highlight-tail's overlays at point
    (let ((overlays-at-start-point (highlight-tail-overlays-at start-point))
          highlight-tail-overlay)
      (mapcar '(lambda (overlay)
                 (when (highlight-tail-overlay-get overlay 'highlight-tail)
                   (setq highlight-tail-overlay overlay)))
              overlays-at-start-point)
      (when highlight-tail-overlay
        (add-to-list 'highlight-tail-deleted-overlays-list
                     highlight-tail-overlay)
        (remhash highlight-tail-overlay highlight-tail-overlays-hash)
        (highlight-tail-delete-overlay highlight-tail-overlay)))
    ;; do we need to fade out to default color or any other
    (setq point-face-bgcolor-hex (highlight-tail-get-bgcolor-hex start-point))
    ;; add the overlay with good ending color
    (let (highlight-tail-overlay)
      (if (> (length highlight-tail-deleted-overlays-list) 0)
          ;; get overlay from list of deleted overlays
          (progn
            (setq highlight-tail-overlay
                  (highlight-tail-move-overlay
                   (car highlight-tail-deleted-overlays-list)
                   start-point end-point
                   (current-buffer))) ; Xemacs needs it (or will highlight in
                                        ; other buffer)
            (setq highlight-tail-deleted-overlays-list
                  (cdr highlight-tail-deleted-overlays-list)))
        ;; make new overlay
        (setq highlight-tail-overlay
              (highlight-tail-make-overlay start-point end-point))
        (highlight-tail-overlay-put
         highlight-tail-overlay 'evaporate t)
        (highlight-tail-overlay-put
         highlight-tail-overlay 'highlight-tail t))
      (puthash highlight-tail-overlay
               (list point-face-bgcolor-hex
                     1)             ; first step in fading-out
               highlight-tail-overlays-hash)
      (highlight-tail-overlay-put
       highlight-tail-overlay 'face
       (intern
        (concat "highlight-tail-face-"
                (format "%s" point-face-bgcolor-hex)
                "-1")))))) ; first step in fading out


(defvar smooth-buffer
  "*smooth*"
  "*The name of the smooth process buffer (default=*smooth*).")

(defvar smooth-interpreter
  "ghci"
  "*The haskell interpeter to use (default=ghci).")

(defvar smooth-interpreter-arguments
  (list "-XOverloadedStrings"
        )
  "*Arguments to the haskell interpreter (default=none).")

(defvar smooth-run-control
  "~/.smooth.hs"
  "*Run control file (default=~/.smooth.hs)")

(defvar smooth-modules
  (list "import Control.Concurrent"
        "import Control.Monad"
        "import Data.List"
        "import Control.Applicative"
        "import Parse"
        "import Pattern"
        "import Stream"
        "import Dirt"
        )
  "*List of modules (possibly qualified) to bring into interpreter context.")

(defvar smooth-literate-p
  t
  "*Flag to indicate if we are in literate mode (default=t).")

(make-variable-buffer-local 'smooth-literate-p)

(defun smooth-unlit (s)
  "Remove bird literate marks"
  (replace-regexp-in-string "^> " "" s))

(defun smooth-intersperse (e l)
  (if (null l)
      '()
    (cons e (cons (car l) (smooth-intersperse e (cdr l))))))

(defun smooth-write-default-run-control ()
  "Write default run control file if no file exists."
  (if (not (file-exists-p smooth-run-control))
      (with-temp-file
          smooth-run-control
        (mapc
         (lambda (s)
           (insert (concat s "\n")))
         smooth-modules))))

(defun smooth-start-haskell ()
  "Start haskell."
  (interactive)
  (if (comint-check-proc smooth-buffer)
      (error "A smooth process is already running")
    (apply
     'make-comint
     "smooth"
     smooth-interpreter
     nil
     smooth-interpreter-arguments)
    (smooth-see-output))
  (smooth-write-default-run-control)
  (smooth-send-string (concat ":l " smooth-run-control))
  (smooth-send-string ":set prompt \"smooth> \""))

(defun smooth-see-output ()
  "Show haskell output."
  (interactive)
  (when (comint-check-proc smooth-buffer)
    (delete-other-windows)
    (split-window-vertically)
    (with-current-buffer smooth-buffer
      (let ((window (display-buffer (current-buffer))))
	(goto-char (point-max))
	(save-selected-window
	  (set-window-point window (point-max)))))))

(defun smooth-quit-haskell ()
  "Quit haskell."
  (interactive)
  (kill-buffer smooth-buffer)
  (delete-other-windows))

(defun smooth-help ()
  "Lookup up the name at point in the Help files."
  (interactive)
  (mapc (lambda (filename)
	  (find-file-other-window filename))
	(find-lisp-find-files smooth-help-directory
			      (concat "^"
				      (thing-at-point 'symbol)
				      "\\.help\\.lhs"))))

(defun chunk-string (n s)
  "Split a string into chunks of 'n' characters."
  (let* ((l (length s))
         (m (min l n))
         (c (substring s 0 m)))
    (if (<= l n)
        (list c)
      (cons c (chunk-string n (substring s n))))))

(defun smooth-send-string (s)
  (if (comint-check-proc smooth-buffer)
      (let ((cs (chunk-string 64 (concat s "\n"))))
        (mapcar (lambda (c) (comint-send-string smooth-buffer c)) cs))
    (error "no smooth process running?")))

(defun smooth-transform-and-store (f s)
  "Transform example text into compilable form."
  (with-temp-file f
    (mapc (lambda (module)
	    (insert (concat module "\n")))
	  smooth-modules)
    (insert "main = do\n")
    (insert (if smooth-literate-p (smooth-unlit s) s))))

(defun smooth-run-line ()
  "Send the current line to the interpreter."
  (interactive)
  (let* ((s (buffer-substring (line-beginning-position)
			      (line-end-position)))
	 (s* (if smooth-literate-p
		 (smooth-unlit s)
	       s)))
    (smooth-send-string s*))
  (smooth-highlight-tail-make-new-overlay (point-at-bol) (point-at-eol))
  (next-line)
  )

(defun smooth-run-multiple-lines ()
  "Send the current region to the interpreter as a single line."
  (interactive)
  (save-excursion
   (mark-paragraph)
   (let* ((s (buffer-substring-no-properties (region-beginning)
                                             (region-end)))
          (s* (if smooth-literate-p
                  (smooth-unlit s)
                s)))
     (smooth-send-string ":{")
     (smooth-send-string s*)
     (smooth-send-string ":}")
     (mark-paragraph)
     (smooth-highlight-tail-make-new-overlay (mark) (point))
     )
    ;(smooth-send-string (replace-regexp-in-string "\n" " " s*))
   )
  )

(defun smooth-run-region ()
  "Place the region in a do block and compile."
  (interactive)
  (smooth-transform-and-store
   "/tmp/smooth.hs"
   (buffer-substring-no-properties (region-beginning) (region-end)))
  (smooth-send-string ":load \"/tmp/smooth.hs\"")
  (smooth-send-string "main"))

(defun smooth-load-buffer ()
  "Load the current buffer."
  (interactive)
  (save-buffer)
  (smooth-send-string (format ":load \"%s\"" buffer-file-name)))

(defun smooth-run-main ()
  "Run current main."
  (interactive)
  (smooth-send-string "main"))

(defun smooth-interrupt-haskell ()
  (interactive)
  (if (comint-check-proc smooth-buffer)
      (with-current-buffer smooth-buffer
	(interrupt-process (get-buffer-process (current-buffer))))
    (error "no smooth process running?")))

(defvar smooth-mode-map nil
  "Smooth keymap.")

(defun smooth-mode-keybindings (map)
  "Haskell Smooth keybindings."
  (define-key map [?\C-c ?\C-s] 'smooth-start-haskell)
  (define-key map [?\C-c ?\C-v] 'smooth-see-output)
  (define-key map [?\C-c ?\C-q] 'smooth-quit-haskell)
  (define-key map [?\C-c ?\C-c] 'smooth-run-line)
  (define-key map [?\C-c ?\C-e] 'smooth-run-multiple-lines)
  (define-key map [?\C-c ?\C-r] 'smooth-run-region)
  (define-key map [?\C-c ?\C-l] 'smooth-load-buffer)
  (define-key map [?\C-c ?\C-i] 'smooth-interrupt-haskell)
  (define-key map [?\C-c ?\C-m] 'smooth-run-main)
  (define-key map [?\C-c ?\C-h] 'smooth-help))

(defun turn-on-smooth-keybindings ()
  "Haskell Smooth keybindings in the local map."
  (local-set-key [?\C-c ?\C-s] 'smooth-start-haskell)
  (local-set-key [?\C-c ?\C-v] 'smooth-see-output)
  (local-set-key [?\C-c ?\C-q] 'smooth-quit-haskell)
  (local-set-key [?\C-c ?\C-c] 'smooth-run-line)
  (local-set-key [?\C-c ?\C-e] 'smooth-run-multiple-lines)
  (local-set-key [?\C-c ?\C-r] 'smooth-run-region)
  (local-set-key [?\C-c ?\C-l] 'smooth-load-buffer)
  (local-set-key [?\C-c ?\C-i] 'smooth-interrupt-haskell)
  (local-set-key [?\C-c ?\C-m] 'smooth-run-main)
  (local-set-key [?\C-c ?\C-h] 'smooth-help))

(defun smooth-mode-menu (map)
  "Haskell Smooth menu."
  (define-key map [menu-bar smooth]
    (cons "Haskell-Smooth" (make-sparse-keymap "Haskell-Smooth")))
  (define-key map [menu-bar smooth help]
    (cons "Help" (make-sparse-keymap "Help")))
  (define-key map [menu-bar smooth expression]
    (cons "Expression" (make-sparse-keymap "Expression")))
  (define-key map [menu-bar smooth expression load-buffer]
    '("Load buffer" . smooth-load-buffer))
  (define-key map [menu-bar smooth expression run-main]
    '("Run main" . smooth-run-main))
  (define-key map [menu-bar smooth expression run-region]
    '("Run region" . smooth-run-region))
  (define-key map [menu-bar smooth expression run-multiple-lines]
    '("Run multiple lines" . smooth-run-multiple-lines))
  (define-key map [menu-bar smooth expression run-line]
    '("Run line" . smooth-run-line))
  (define-key map [menu-bar smooth haskell]
    (cons "Haskell" (make-sparse-keymap "Haskell")))
  (define-key map [menu-bar smooth haskell quit-haskell]
    '("Quit haskell" . smooth-quit-haskell))
  (define-key map [menu-bar smooth haskell see-output]
    '("See output" . smooth-see-output))
  (define-key map [menu-bar smooth haskell start-haskell]
    '("Start haskell" . smooth-start-haskell)))

(if smooth-mode-map
    ()
  (let ((map (make-sparse-keymap "Haskell-Smooth")))
    (smooth-mode-keybindings map)
    (smooth-mode-menu map)
    (setq smooth-mode-map map)))

(define-derived-mode
  literate-smooth-mode
  smooth-mode
  "Literate Haskell Smooth"
  "Major mode for interacting with an inferior haskell process."
  (setq smooth-literate-p t)
  (setq haskell-literate 'bird)
  (turn-on-font-lock))

(add-to-list 'auto-mode-alist '("\\.lsmooth$" . literate-smooth-mode))

(define-derived-mode
  smooth-mode
  haskell-mode
  "Haskell Smooth"
  "Major mode for interacting with an inferior haskell process."
  (setq smooth-literate-p nil)
  (turn-on-font-lock))

(add-to-list 'auto-mode-alist '("\\.smooth$" . smooth-mode))

(provide 'smooth)
