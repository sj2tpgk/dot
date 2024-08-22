;;; startup-profiler.el --- emacs startup profiler   -*- lexical-binding: t; -*-

;; Author: Nic Ferrier <nferrier@ferrier.me.uk>
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

;; A simple emacs startup profiler

;;; Code:

(defvar stpr/alist)
(defvar stpr/start-time)
(defvar stpr/total-time)

(defun stpr/start-profiling ()
  (setq stpr/alist       nil
        stpr/start-time  (current-time)
        stpr/total-time  nil))

(defun stpr/finish-profiling ()
  (setq stpr/total-time (float-time (time-since stpr/start-time))))

(defun stpr/profiled-load-file (file)
    "Loads file FILE and records the time required."
    (let* ((time-start    (current-time))
           (--            (load-file file))
           (time-used     (time-since time-start))
           (time-used-sec (float-time time-used)))
      (push (cons file time-used-sec) stpr/alist)))

(defun stpr/show-result ()
    "Print profiling result."
    (let ((total stpr/total-time)
          (sum   0))
      (message         "+---------+---------+------------------------------------")
      (message         "|  Total  |  Time   |    %%    |  Name")
      (message         "|---------+---------+------------------------------------")
      (mapc (lambda (entry)
              (setq sum (+ sum (cdr entry)))
              (message "|  %.05s  |  %.05s  |  %4.1f %% | %s"
                       sum
                       (cdr entry)
                       (* 100 (/ (cdr entry) total))
                       (car entry)
                       ))
            (reverse stpr/alist))
      (message         "|  %.05s  |  %.05s  |  %4.1f %% |  (Others)"
                       total
                       (- total sum)
                       (* 100 (/ (- total sum) total)))
      (message         "|---------+---------+------------------------------------")
      (message         "|  %.05s  |         |         |  Total" total)
      (message         "+---------+---------+------------------------------------")))


(provide 'startup-profiler)
;;; startup-profiler.el ends here
;
;
;
;
;
;(progn ;; Measure startup time (simple profiler) {{{
;  (defun my-profile-init ()
;    (setq my-profile-alist '())
;    (setq my-profile-start-time (current-time))
;    (setq my-profile-total-time nil))
;
;  (my-profile-init)
;
;  (defun my-profile-pretty-print ()
;    "Print profiling result."
;    (let ((total my-profile-total-time)
;          (sum   0))
;      (message         "+---------+---------+------------------------------------")
;      (message         "|  Total  |  Time   |    %%    |  Name")
;      (message         "|---------+---------+------------------------------------")
;      (mapc (lambda (entry)
;              (setq sum (+ sum (cdr entry)))
;              (message "|  %.05s  |  %.05s  |  %4.1f %% | %s"
;                       sum
;                       (cdr entry)
;                       (* 100 (/ (cdr entry) total))
;                       (car entry)
;                       ))
;            (reverse my-profile-alist))
;      (message         "|  %.05s  |  %.05s  |  %4.1f %% |  (Others)"
;                       total
;                       (- total sum)
;                       (* 100 (/ (- total sum) total)))
;      (message         "|---------+---------+------------------------------------")
;      (message         "|  %.05s  |         |         |  Total" total)
;      (message         "+---------+---------+------------------------------------")))
;
;  (defun my-profile-finish ()
;    (setq my-profile-total-time (float-time (time-since my-profile-start-time))))
;
;  (defun my-profiled-load-file (file)
;    "Loads file FILE and records the time required."
;    (let* ((time-start    (current-time))
;           (--            (load-file file))
;           (time-used     (time-since time-start))
;           (time-used-sec (float-time time-used)))
;      (push (cons file time-used-sec) my-profile-alist)))
;  ) ;; }}}
