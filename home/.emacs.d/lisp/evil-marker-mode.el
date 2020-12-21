(defun mark-points (points)
  (interactive)
  (dolist (p points)
    (let ((ov (make-overlay p (+ 1 p))))
      (overlay-put ov 'category 'jmp)
      (overlay-put ov 'face 'evil-snipe-matches-face))))

(defun unmark-points (&optional force)
  (interactive)
  (unless (and (not force) (eq this-command last-command))
    (remove-hook 'pre-command-hook 'unmark-points)
    (setq mark-last-info nil)
    (remove-overlays 0 10000 'category 'jmp)))

(defun get-delim-points (from to mover)
  (let (res)
    (save-excursion (goto-char from) (while (<= from (point) to) (funcall mover) (push (point) res)))
    (save-excursion (goto-char to)   (while (<= from (point) to) (funcall mover) (push (point) res)))
    (nreverse res)))

(setq mark-last-info -1)
(defun call-mark (&rest as) ;; only update when moving to another line
  (when (and (called-interactively-p 'any)
             (memq this-command marked-commands)
             (or (not mark-last-info)
                 (not (= mark-last-info (line-number-at-pos)))))
    ;; (message "Updating")
    (when (and mark-last-info (not (= mark-last-info (line-number-at-pos)))) (unmark-points t))
    (mark-points (get-delim-points (line-beginning-position) (line-end-position) this-command))
    (setq mark-last-info (line-number-at-pos))
    (add-hook 'pre-command-hook 'unmark-points)))

(setq enabled-marks nil)
(defun add-marker-advice (movefun)
  (advice-add movefun :after 'call-mark)
  (push movefun enabled-marks))

(defun remove-marker-advice ()
  (dolist (x enabled-marks) (advice-remove x 'call-mark))
  (setq enabled-marks nil))

(setq marked-commands '(evil-forward-word-begin
                        evil-forward-word-end
                        evil-backward-word-begin
                        evil-forward-WORD-begin
                        evil-forward-WORD-end
                        evil-backward-WORD-begin))

(mapc 'add-marker-advice marked-commands)
(remove-marker-advice)

(define-minor-mode evil-marker-mode
  "evil-marker minor mode."
  :lighter " marker"
  :group 'evil-marker
  (if evil-marker-mode
      (mapc 'add-marker-advice marked-commands)
    (remove-marker-advice)))

(defun turn-on-evil-marker-mode () (message "yes"))

;; (defun hookmanip (&rest as) (remove-hook 'pre-command-hook 'unmark-points))

;; (advice-add 'evil-forward-word-begin :before 'hookmanip)
;; (advice-remove 'evil-forward-word-begin 'hookmanip)
;; (advice-add 'evil-forward-word-begin :after 'mark-efwb)
;; (advice-remove 'evil-forward-word-begin 'mark-efwb)

;; (global-set-key (kbd "<f7>") 'evil-marker-mode)
;; (global-set-key (kbd "<f8>") 'unmark-points)

(provide 'evil-marker-mode)
