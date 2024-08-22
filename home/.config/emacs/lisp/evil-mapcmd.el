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

;; Testing: guess undefined keymaps and hook.
;; Testing: remove unbind-key dependency
;; Todo: key-chord support
;; Todo: non-evil support

(defcustom evmc/guess-hook-for-keymaps t "When keymap is not defined yet, automatically guess the right hook?")

(defvar evmc/delayed-thunks nil)

(defun evmc/run-delayed-thunks ()
  ;; reverse is important; some unmappings must be done before mappings
  (mapc 'funcall (reverse evmc/delayed-thunks)))

(defun evmc/add-one-time-hook (hook func) ;; Add hook that runs only once.
  (let (f)
    (setq f (lambda (&rest args)
              (apply func args)
              (remove-hook hook f)))
    (add-hook hook f)))

(defun evmc/setup (states map &rest binds) ;; Care whether keymap is defined, evil is loaded etc.
  ;; &rest is necessary to evaluate ' in (xmap "a" 'command)
  ;; (message "evmc/setup: %s" (list states map binds))
  (let* ((map1 (and map ;; xxx-xxx:  -->  xxx-xxx-map
                    (let ((str (symbol-name map)))
                      (if (string-suffix-p ":" str)
                          (intern (format "%s-map" (substring str 0 -1)))
                        map))))
         (thunk (lambda ()
                  (let ((f (lambda () (evmc/do-binding states map1 binds))))
                    (if (and map1 (not (boundp map1)))
                        (if evmc/guess-hook-for-keymaps
                            (evmc/add-one-time-hook (evmc/guess-hook map1) f)
                          (error "Keymap %s is not defined.\nConsider setting `evmc/guess-hook-for-keymaps' to t." map1))
                      (funcall f)))))
         (is-evil-loaded (bound-and-true-p evil-mode)))
    (if is-evil-loaded
        (funcall thunk)
      (push thunk evmc/delayed-thunks))))

(defun evmc/do-binding (states map binds) ;; Actually calls evil-define-key*
  ;; (message "%s" (list states map binds))
  (let ((map-value (if map (symbol-value map) 'global)))
    (while binds
      (let* ((key0 (pop binds)) (key (if (stringp key0) (kbd key0) key0))
             (func (pop binds)))
        (dolist (s states)
          (if (eq s 'none)
              (define-key map-value key func)
            (evil-define-key* s map-value key func)))))))

(defun evmc/guess-hook (map) ;; Guess appropriate hook which will create the keymap
  (let ((s (symbol-name map)))
    (if (string-suffix-p "-mode-map" s)
        (intern (concat (string-remove-suffix "map" s) "hook"))
      (error "Can't guess hook from keymap name: %s" map))))

(defmacro evmc/define-mapper (name states)
  ;; (evmc/define-mapper nmap (normal))
  ;; ==>
  ;; (defmacro nmap (map &rest binds)
  ;;   (if (symbolp map)
  ;;       `(evmc/setup '(normal) ',map ,@binds)
  ;;     `(evmc/setup '(normal) nil ',map ,@binds)))
  `(defmacro ,name (map &rest binds)
     ,(format "Keymapping function for states: %1$s
Example: (%2$s \"a\" 'cmd1)
         (%2$s emacs-lisp-mode-map \"b\" 'cmd2)
         (%2$s lua-mode-map \"c\" 'cmd3)
If key is a string, automatically wrapped with (kbd ...).
If the keymap is not defined yet and `evmc/guess-hook-for-keymaps' is non-nil,
automatically add code to an appropriate hook (in this example, lua-mode-hook).
"
              (string-join (mapcar 'symbol-name states) ", ")
              name)
     (let ((states2 ',states))
       (if (symbolp map)
           `(evmc/setup ',states2 ',map ,@binds)
         `(evmc/setup ',states2 nil ',map ,@binds)))))
(defmacro evmc/define-unmapper (name map)
  `(defun ,name (key)
     ;; (message (format "Mapping %s %s %s" ',name ',map key))
     (if (bound-and-true-p evil-mode)
         ;; (unbind-key (kbd key) ,map)
         (define-key ,map (kbd key) nil)
       (let ((s (list 'lambda nil ;; expect dynamic scope
                      (list 'define-key ,map (kbd key) nil))))
         (push s evmc/delayed-thunks)))))

(evmc/define-mapper emap  (none))
(evmc/define-mapper nmap  (normal))
(evmc/define-mapper vmap  (visual))
(evmc/define-mapper nvmap (normal visual))
(evmc/define-mapper imap  (insert))
(evmc/define-mapper nimap (normal insert))
(evmc/define-mapper mmap  (motion))
(evmc/define-mapper omap  (operator))

;; (insert (format "%s" (evmc/define-unmapper nunmap evil-normal-state-map)))
(evmc/define-unmapper nunmap evil-normal-state-map)
(evmc/define-unmapper vunmap evil-visual-state-map)
(evmc/define-unmapper ounmap evil-operator-state-map)

(with-eval-after-load 'evil (add-hook 'evil-mode-hook 'evmc/run-delayed-thunks))

(provide 'evil-mapcmd)
;;; evil-mapcmd.el ends here
