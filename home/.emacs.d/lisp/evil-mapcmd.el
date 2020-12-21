;;; evil-mapcmd.el --- vim-like mapping commands     -*- lexical-binding: t; -*-

;; Author: sj2tpgk <github.fQ1wY8Wc.a87EgLB0@gmail.com>
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

;; TODO: handle undefined keymap (automatically delay until some hook?)

;;; Commentary:

;; nmap, imap, vmap etc.
;; (nmap "SPC w" 'save-some-buffer)

;;; Code:

(defvar evmc/delayed-maps nil)

(defun evmc/run-delayed-maps ()
  (let ((run-map (lambda (x) (apply (car x) (cdr x)))))
    ;; reverse is important; some unmappings must be done before mappings
    (mapc run-map (reverse evmc/delayed-maps)))
  (remove-hook 'evil-mode-hook 'evmc/run-delayed-maps))

(defun evmc/delay-map (func args)
  (if (and (boundp 'evil-mode) evil-mode)
      nil
    (push (cons func args) evmc/delayed-maps)
    t))

(defun evmc/map-helper (states &rest args)
  (let ((loop (lambda (state-list keymap specs)
                (while specs
                  (let ((key (kbd (pop specs))) (func (pop specs)))
                    (dolist (s state-list)
                      (evil-define-key* s keymap key func)))))))
    (let ((state-list (if (listp states) states (list states))))
      (if (and args (keymapp (car args)))
          (funcall loop state-list (car args) (cdr args))
        (funcall loop state-list 'global args)))))

(defmacro evmc/define-mapper (name states)
  `(defun ,name (&rest args)
                                        ; (message (format "Mapping %s %s %s" ',name ',states args))
     (or (evmc/delay-map (quote ,name) args)
         (apply 'evmc/map-helper ,states args))))

(defmacro evmc/define-unmapper (name map)
  `(defun ,name (key)
                                        ; (message (format "Mapping %s %s %s" ',name ',map key))
     (or (evmc/delay-map ',name (list key))
         (unbind-key (kbd key) ,map))))

(evmc/define-mapper nmap  'normal)
(evmc/define-mapper vmap  'visual)
(evmc/define-mapper nvmap '(normal visual))
(evmc/define-mapper imap  'insert)
(evmc/define-mapper nimap '(normal insert))
(evmc/define-mapper mmap  'motion)
(evmc/define-mapper omap  'operator)

(evmc/define-unmapper nunmap evil-normal-state-map)
(evmc/define-unmapper vunmap evil-visual-state-map)
(evmc/define-unmapper ounmap evil-operator-state-map)

(with-eval-after-load 'evil (add-hook 'evil-mode-hook 'evmc/run-delayed-maps))

(provide 'evil-mapcmd)
;;; test.el ends here
