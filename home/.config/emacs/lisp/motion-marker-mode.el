;; -*- lexical-binding: t; -*-

;; Copyright (C) 2020-  someone

;; Author: someone <email>
;; Keywords: lisp
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Inspired by evil-snipe, highlights points when you use motion functions where cursor will go.
;; Usage:
;;   (require 'motion-marker)
;;   (motion-marker-mode 1)

;;; Code:

(defgroup  motion-marker nil
  "Group f or motion-marker")
(defface   motion-marker-face
  '((t (:inherit region)))
  "Face for motion-marker mode marks"
  :group 'motion-marker)
(defcustom motion-marker-commands
  '(forward-word
    backward-word
    evil-forward-word-begin
    evil-forward-word-end
    evil-backward-word-begin
    evil-forward-WORD-begin
    evil-forward-WORD-end
    evil-backward-WORD-begin)
  "Mark when these commands are used."
  :group 'motion-marker :type '(list symbol))
(defcustom motion-marker-max-line-length 200
  "Don't mark on lines longer than this value."
  :group 'motion-marker :type 'integer)


;; ====== Internal vars ======

(defconst moma-overlay-category  'motion-marker-overlay)
(defvar   moma-last-line         -1)


;; ====== Helper functions ======

(defun moma-mark-points (points)
  (dolist (p points)
    (let ((ov (make-overlay p (+ 1 p))))
      (overlay-put ov 'category moma-overlay-category)
      (overlay-put ov 'face 'motion-marker-face))))
(defun moma-unmark ()
  (remove-overlays (point-min) (point-max) 'category moma-overlay-category))
(defun moma-get-points-to-mark (from to mover)
  "Get points where cursor comes when MOVER is used consequtively, between FROM and TO."
  (let* ((low  (min from to))
         (high (max from to))
         (res  nil))
    ;; (message "%s %s %s" low high moma-last-line)
    (when (< (- high low) motion-marker-max-line-length)
      (dolist (x (list low high))
        (save-excursion
          (goto-char x)
          (funcall mover)
          (while (< low (point) high) (push (point) res) (funcall mover))))
      (nreverse res))))


;; ====== Advice functions ======

(defun moma-mark-advice (&rest _args) ;; only update when moving to another line
  (when (and (called-interactively-p 'any)
             (memq this-command motion-marker-commands)
             ;; only when no mark or moved to a different line
             (or (not moma-last-line)
                 (not (= moma-last-line (line-number-at-pos)))))
    ;; Unmark when moved to a different line
    (unless (equal moma-last-line (line-number-at-pos)) (moma-unmark))
    (setq moma-last-line (line-number-at-pos))
    (moma-mark-points (moma-get-points-to-mark (line-beginning-position) (line-end-position) this-command))
    ;; May remove marks on next command
    (add-hook 'pre-command-hook 'moma-unmark-advice)))
(defun moma-unmark-advice ()
  (unless (eq this-command last-command)
    (remove-hook 'pre-command-hook 'moma-unmark-advice)
    (setq moma-last-line nil)
    (moma-unmark)))


;; ====== Define minor mode ======

(define-minor-mode motion-marker-mode
  "motion-marker minor mode."
  :lighter " motionmark"
  :group 'motion-marker
  (cond
   (motion-marker-mode (dolist (f motion-marker-commands)
                         (advice-add f :after 'moma-mark-advice)))
   (t                  (dolist (f motion-marker-commands)
                         (advice-remove f :after 'moma-mark-advice)))))

(provide 'motion-marker-mode)

;;; motion-marker-mode.el ends here
