;; More visual feedback (portion in buffer)

(defun ml-buffer-status ()
  (concat (let ((f (buffer-file-name)))
            (if (and f (file-executable-p f)) "x" ""))
          (cdr (assoc (cons buffer-read-only (buffer-modified-p))
                      '(((nil . nil) . "-")
                        ((nil . t  ) . "+")
                        ((t   . nil) . "R")
                        ((t   . t  ) . "R+"))))))

;; (defun ml-buffer-position (&optional l)
;;   (let* ((l (or l 10))
;;          (x (line-number-at-pos)) (n (line-number-at-pos (point-max)))
;;          (p (floor (/ (* l (+ x (/ n 2 l))) n))))
;;     (format "[%s#%s]" (make-string (max 0 (1- p)) ?.) (make-string (min (1- l) (- l p)) ?.))))

(defun ml-left-right (fmt-left fmt-right)
  (let* ((left  (format-mode-line fmt-left))
         (right (format-mode-line fmt-right))
         (w (window-width)) (wl (length left)) (wr (length right)))
    (cond
     ;; Window has enough space
     ((<= (+ wl wr) w)
      (format "%s%s%s" left (make-string (- w wl wr) ? ) right))

     ;; If window width is too small, right is completely hidden.
     ((<= w (+ wl 2))
      (format "%s" (substring left 0 w)))

     ;; If window width is a bit small, prioritize left.
     (t (format "%s  %s" (substring left 0 wl) (substring right (- wr (- w wl 2)))))))

  ;; ;; If window width is too small, cut each string equally
  ;; (let* ((wex (- (+ wl wr) w))
  ;;        (wl1 (- wl (floor wex 2)))
  ;;        (wr1 (- wr (ceiling wex 2))))
  ;;   (format "%s%s" (substring left 0 wl1) (substring right (- wr wr1))))
  )

(defun ml-buffer-region-size ()
  (if (not (region-active-p))
      ""
    (let* ((b (region-beginning)) (e (region-end))
           (lb (line-number-at-pos b)) (le (line-number-at-pos e))
           (lines (- le lb))
           (chars (case evil-visual-selection
                    (char (- (bufferpos-to-filepos e 'exact)
                             (bufferpos-to-filepos b 'exact)
                             (- lines 1)))
                    (line (- (line-end-position
                              (- le (line-number-at-pos) -1))
                             (line-beginning-position
                              (- lb (line-number-at-pos) -1))
                             lines))
                    (t    "?"))))
      (format "R%s(%s)" lines chars))))

;; (setq-default mode-line-format
;;               '(:eval (ml-left-right
;;                        '("%e"
;;                          " " (:eval (ml-buffer-status))
;;                          " " (:eval mode-line-buffer-identification)
;;                          ;; "   %2l:%2c "
;;                          "   L%2l/" (:eval (format "%s" (line-number-at-pos (point-max))))
;;                          "  C%2c "
;;                          " " (:eval (ml-buffer-region-size))
;;                          ;; " " (:eval (format "(%s/%s)" (line-number-at-pos) (line-number-at-pos (point-max)))) ""
;;                          ;; " " (:eval (ml-buffer-position 10)) ""
;;                          )
;;                        '("" mode-line-modes)
;;                        ;; (:eval (ml-eyebrowse))
;;                        )))

(defun set-mode-line-format (fmt)
  ;; Update mode-line-format in all bufs (+ setq-default)
  (setq-default mode-line-format fmt)
  (dolist (b (buffer-list))
    (with-current-buffer b
      (setq mode-line-format fmt))))


(set-mode-line-format

 ;; Remove unnecessary elements from default. And some beautifying.
 ;; (not a "from scratch" approach)

 '(:eval (ml-left-right
          '("%e"
            ;; mode-line-front-space mode-line-mule-info
            " "
            (:eval (ml-buffer-status))
            ;; mode-line-modified
            ;; mode-line-remote
            " "
            ;; mode-line-frame-identification
            mode-line-buffer-identification
            ;; "   " mode-line-position
            "     L%2l/" (:eval (format "%s" (line-number-at-pos (point-max))))
            "  C%2c "
            " " (:eval (ml-buffer-region-size)))
          '((vc-mode vc-mode)
            "  " mode-line-modes mode-line-misc-info
            mode-line-client)))
 )
