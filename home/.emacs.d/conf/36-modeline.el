;; -*- lexical-binding: t; -*-

;; Customization of mode-line, header-line, tab-line, tab-bar

(defun ml-left-right (fmt-left fmt-right)
  "Insert space in between so that \"$left   $right\" fits the window."
  (let* ((left  (format-mode-line fmt-left))
         (right (format-mode-line fmt-right))
         (wl (string-width left)) (wr (string-width right)) ;; don't use "length" here, for multibyte chars
         (w (window-width)))
    (cond
     ;; Window has enough space
     ((<= (+ wl wr) w)
      (format "%s%s%s" left (make-string (- w wl wr) ? ) right))

     ;; If window width is too small, right is completely hidden.
     ((<= w (+ wl 2))
      (format "%s" (substring left 0 w)))

     ;; If window width is a bit small, prioritize left.
     (t (format "%s  %s" (substring left 0 wl) (substring right (- wr (- w wl 2))))))))

(defun ml-buffer-status (&optional buf)
  (concat (let ((f (buffer-file-name buf)))
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

;; Tab-bar tabs switcher
(dolist (h '(window-selection-change-functions window-state-change-hook))
  (add-hook h 'ml-update-tab-bar-switcher))
(defun ml-update-tab-bar-switcher (&rest _)
  (setq ml-tab-bar-switcher
        (let ((n (length (tab-bar-tabs))))
          (if (< 6 n)
              (format "%s tabs" n)
            `(:eval ,(loop for i below n
                           concat (propertize (format "%s " i)
                                              'face (if (= i (tab-bar--current-tab-index))
                                                        '(:inverse-video t) '())
                                              'help-echo (format "Select window config %s" i)
                                              'keymap `(keymap
                                                        (tab-line keymap
                                                                  (mouse-1 . (lambda () (interactive) (message "%s" ,(1+ i)) (tab-bar-select-tab ,(1+ i))))
                                                                  (mouse-4 . tab-bar-switch-to-prev-tab)
                                                                  (mouse-5 . tab-bar-switch-to-next-tab))))))))))

;; (defun ml-update-major-mode-name ()
;;   (message "yes %s" (buffer-name))
;;   ;; (make-local-variable 'ml-major-mode-name)
;;   ;; (unless (local-variable-p 'ml-major-mode-name) (make-local-variable 'ml-major-mode-name))
;;   (if (boundp 'ml-major-mode-name)
;;       (setq-local ml-major-mode-name (capitalize (string-remove-suffix "-mode" (symbol-name major-mode))))
;;     (defvar-local ml-major-mode-name (capitalize (string-remove-suffix "-mode" (symbol-name major-mode)))))
;;   (message "%s" ml-major-mode-name))
;; (dolist (h '(change-major-mode-hook find-file-hook))
;;   (add-hook h 'ml-update-major-mode-name))
;; (dolist (b (buffer-list)) (with-current-buffer b (ml-update-major-mode-name)))
;; (message "mm: %s" (buffer-list))

(let ((ht (make-hash-table :test 'equal)))
  (defun ml-shorten-path (path)
    (or (gethash path ht)
        (let* ((l (split-string (string-remove-suffix "/" (abbreviate-file-name path)) "/" nil))
               (abbr (string-join (append (mapcar (lambda (s)
                                                    (substring s 0 (min 1 (length s))))
                                                  (butlast l))
                                          (last l))
                                  "/")))
          (puthash path abbr ht)
          abbr))))

;; For fun
(let* ((ls '("<*_*>"    "<')><"    "Zzz.."   "*(oo)*"   "@('_')@"   "<'^_)~"
             "$1000"    "<%%%|=="  "%+$#!?"))
       (lf (mapcan (lambda (x) `((:foreground ,x)
                                 (:background ,x :foreground "black")))
                   '("white" "red" "green" "cyan" "magenta" "yellow")))
       (ht (make-hash-table :test 'eq)))
  (defun ml-easter-egg ()
    (or (gethash (current-buffer) ht)
        (let* ((s (buffer-name))
               (h (loop for i below (length s) by 4 sum (elt s i)))
               (res (propertize (nth (mod h (length ls)) ls)
                                'face (nth (mod h (length lf)) lf))))
          (puthash (current-buffer) res ht)
          res))))

(defun set-mode-line-format (fmt &optional var)
  (setq var (or var 'mode-line-format))
  ;; Update mode-line-format in all bufs (+ setq-default)
  (set-default var fmt)
  (dolist (b (buffer-list))
    (with-current-buffer b
      (set var fmt))))
(defun set-header-line-format (fmt) (set-mode-line-format fmt 'header-line-format))
(defun set-tab-line-format    (fmt) (set-mode-line-format fmt 'tab-line-format))

(set-header-line-format nil)
;; (set-header-line-format
;;  '((:eval (if (eq ml-selected-window (selected-window))
;;               `(:propertize ,mode-line-format face (:foreground "blue"))
;;             `(:propertize ,mode-line-format face mode-line-inactive)
;;             ))))
;; (defun ml-record-window () (setq ml-selected-window (selected-window)))
;; (add-hook 'buffer-list-update-hook 'ml-record-window)

(set-mode-line-format
 '(:eval (ml-left-right
          '((:eval ml-tab-bar-switcher)
            "  "
            (:eval (ml-buffer-status))
            " "
            mode-line-buffer-identification
            " [" (:eval (ml-shorten-path default-directory)) "]"
            "     L%2l/" (:eval (format "%s" (line-number-at-pos (point-max))))
            "  C%2c "
            " " (:eval (ml-buffer-region-size)))
          '((vc-mode vc-mode)
            ;; " [" ml-major-mode-name "]  "
            " "
            mode-line-modes
            " " ;; (:eval (ml-easter-egg))
            mode-line-client))))

;; (set-mode-line-format

;;  ;; Remove unnecessary elements from default. And some beautifying.
;;  ;; (not a "from scratch" approach)

;;  '(:eval (ml-left-right
;;           '("%e"
;;             ;; mode-line-front-space mode-line-mule-info
;;             " "
;;             (:eval (ml-buffer-status))
;;             ;; mode-line-modified
;;             ;; mode-line-remote
;;             " "
;;             ;; mode-line-frame-identification
;;             mode-line-buffer-identification
;;             ;; "   " mode-line-position
;;             "     L%2l/" (:eval (format "%s" (line-number-at-pos (point-max))))
;;             "  C%2c "
;;             " " (:eval (ml-buffer-region-size)))
;;           '((vc-mode vc-mode)
;;             "  " mode-line-modes mode-line-misc-info
;;             mode-line-client))))

;; (require 'tab-line)
;; (set-mode-line-format
;;  '(" Buffers:  "
;;    (:eval
;;     ;; (format "%s" (mapcar (lambda (b)
;;     ;;                        (let ((s (buffer-name b)))
;;     ;;                          (substring s 0 (min 5 (length s)))))
;;     ;;                      (buffer-list)))
;;     (string-join
;;      (sort (mapcar 'buffer-name (funcall tab-line-tabs-function))
;;            'string<)
;;      "  "))))

(progn ;; Tabs (emacs 27)
  (setq tab-bar-close-button-show t
        tab-bar-new-button-show nil
        tab-bar-tab-name-truncated-max 24
        tab-bar-tab-name-ellipsis ".."
        tab-bar-separator " "
        tab-bar-show nil
        tab-bar-new-tab-choice "*scratch*")
  (fset tab-bar-tab-name-function
        (lambda ()
          (let ((tab-name (tab-bar-tab-name-truncated)))
            (format " %s" tab-name))))
  (defun add-tab-scroll-bindings (lis) ;; lis = (keymap x1 x2 ...)
    `(keymap (mouse-4 . tab-previous)
             (mouse-5 . tab-next)
             ,@(cdr lis)))
  (advice-add 'tab-bar-make-keymap-1 :filter-return 'add-tab-scroll-bindings))

(when t ;; tab-line
  ;; (global-tab-line-mode 1)
  ;; (require 'cl)
  (require 'tab-line) ;; necessary
  ;; (global-tab-line-mode 1)
  (setq tab-line-separator "  "
        tab-line-close-button-show nil
        tab-line-tab-name-truncated-max 14
        tab-line-tab-name-ellipsis "…"
        tab-line-switch-cycling t
        tab-line-tabs-function
        (lambda ()
          (sort (tab-line-tabs-window-buffers)
                (lambda (b1 b2) (string< (buffer-name b1) (buffer-name b2))))))

  (defun tl-buffer-status (&optional buf)
    (with-current-buffer buf
      (cl-flet ((true (x) (not (not x))))
        (cdr (assoc (cons (true buffer-read-only) (true (and (buffer-file-name)
                                                             (buffer-modified-p))))
                    `(((nil . nil) . "")
                      ((nil . t  ) . "+")
                      ((t   . nil) . "R")
                      ((t   . t  ) . "R+")))))))
 
  (defun tab-line-tab-name-truncated-buffer2 (buffer &optional _buffers)
    "Generate tab name from BUFFER.
Truncate it to the length specified by `tab-line-tab-name-truncated-max'.
Append ellipsis `tab-line-tab-name-ellipsis' in this case."
    (let* ((tab-name (buffer-name buffer))
           (str (concat (tl-buffer-status buffer) tab-name)))
      (if (< (length str) tab-line-tab-name-truncated-max)
          str
        (propertize (truncate-string-to-width
                     str tab-line-tab-name-truncated-max nil nil
                     tab-line-tab-name-ellipsis)
                    'help-echo tab-name))))
  (setf tab-line-tab-name-function 'tab-line-tab-name-truncated-buffer2)

  (defun tab-line-select-tab (&optional e)
    "Switch to the selected tab.
This command maintains the original order of prev/next buffers.
So for example, switching to a previous tab is equivalent to
using the `previous-buffer' command."
    (interactive "e")
    (let* ((posnp (event-start e))
           (tab (get-pos-property (length tab-line-separator) 'tab (car (posn-string posnp))))
           (buffer (if (bufferp tab) tab (cdr (assq 'buffer tab)))))
      (if buffer
          (tab-line-select-tab-buffer buffer (posn-window posnp))
        (let ((select (cdr (assq 'select tab))))
          (when (functionp select)
            (with-selected-window (posn-window posnp)
              (funcall select)
              (force-mode-line-update)))))))

  (defun tab-line-close-tab (&optional mouse-event)
    "Close the selected tab.
Usually is invoked by clicking on the close button on the right side
of the tab.  This command buries the buffer, so it goes out of sight
from the tab line."
    (interactive (list last-nonmenu-event))
    (let* ((posnp (and (listp mouse-event) (event-start mouse-event)))
           (window (and posnp (posn-window posnp)))
           (tab (get-pos-property (length tab-line-separator) 'tab (car (posn-string posnp))))
           (buffer (if (bufferp tab) tab (cdr (assq 'buffer tab))))
           (close-function (unless (bufferp tab) (cdr (assq 'close tab)))))
      (with-selected-window (or window (selected-window))
        (cond
         ((functionp close-function)
          (funcall close-function))
         ((eq tab-line-close-tab-function 'kill-buffer)
          (kill-buffer buffer))
         ((eq tab-line-close-tab-function 'bury-buffer)
          (if (eq buffer (current-buffer))
              (bury-buffer)
            (set-window-prev-buffers nil (assq-delete-all buffer (window-prev-buffers)))
            (set-window-next-buffers nil (delq buffer (window-next-buffers)))))
         ((functionp tab-line-close-tab-function)
          (funcall tab-line-close-tab-function tab)))
        (force-mode-line-update))))

  ;; (with-sparse-keymap)
  (global-set-key [tab-line mouse-4] 'tab-line-switch-to-prev-tab)
  (global-set-key [tab-line mouse-5] 'tab-line-switch-to-next-tab)
  ;; (setq tab-line-tab-bar-keymap )
  (let ((m (make-sparse-keymap)))
    (define-key m [tab-line mouse-1] 'tab-line-close-tab)
    (set-tab-line-format
     `((:eval ml-tab-bar-switcher)
       (:eval (mapcar (lambda(x)(propertize x 'display nil)) (tab-line-format)))
       ;; (:eval (propertize " x "
       ;;                    'help-echo "Detach buffer from window"
       ;;                    'mouse-face '(:inverse-video t)
       ;;                    'keymap ',m))
       )))
  )
