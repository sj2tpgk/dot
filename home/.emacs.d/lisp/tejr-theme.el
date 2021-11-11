;; -*- lexical-binding: t; -*-

;; todo: ability to depend on other theme.

(defconst tejr-theme-name 'tejr)

(setq tejr-rules
      `(
        (default :fg default :bg default)
        (match   :fg k :bg w)
        (region  match)

        (font-lock-comment-face         default :fg b :-b)
        (font-lock-constant-face        default :fg c :-b)
        (font-lock-function-name-face   default :fg y :-b)
        (font-lock-keyword-face         default :fg g :-b)
        (font-lock-string-face          default :fg g :-b)
        (font-lock-type-face            default :fg r :-b)
        (font-lock-variable-name-face   default :fg c :-b)
        (font-lock-warning-face         default :fg w :bg r)

        (font-lock-builtin-face         default :fg r :-b)
        ))

(global-set-key (kbd "<f7>") 'tejr-reload-theme-test)
(defun tejr-reload-theme-test ()
  (interactive) (eval-buffer) (apply 'custom-theme-set-faces tejr-theme-name (mapcar 'tejr-parse tejr-rules)))

(defun tejr-getcolor (name)
  (cdr (assoc name '((k . "#38464F") (r . "#FF6263") (g . "#8CD854") (y . "#F4C52C")
                     (b . "#43C5F1") (m . "#B373E5") (c . "#62D8C5") (w . "#D6DFD8")))))

(defun tejr-parse (rule)
  (let* ((name (car rule))
         (inherits nil)
         (attrs (tejr-mapcan
                 (lambda (x prev next)
                   (cond ((memq prev '(':fg ':bg ':b)))
                         ((eq x :fg)      `(:foreground ,(tejr-getcolor next)))
                         ((eq x :bg)      `(:background ,(tejr-getcolor next)))
                         ((eq x :b)       `(:weight bold))
                         ((eq x :-b)      `(:weight normal))
                         (t               (push x inherits) nil)))
                 (cdr rule)))
         (inherit-clause (if inherits `(:inherit ,inherits) nil)))
    `(,name ( (t ,@inherit-clause ,@attrs) ) )
    ))

(defun tejr-mapcan (proc lis)
  (let ((res nil) (l lis) (prev nil) (next (cadr lis)))
    (while l
      (push (funcall proc (car l) prev next) res)
      (setq prev (car l) l (cdr l) next (cadr l)))
    (apply 'append (nreverse res))))

(defun tejr-create-theme (theme-name docstring specs-for-custom-set)
  ;; Declare theme
  (eval `(deftheme ,theme-name ,docstring))

  ;; <<< IMPORTANT: first applied rule has more priority. >>>
  ;; Apply custom rules (must be applied BEFORE rules from default theme)
  (apply 'custom-theme-set-faces theme-name specs-for-custom-set)

  ;; Finally provide the theme
  (provide-theme theme-name))

(tejr-create-theme tejr-theme-name "tejr theme." ())
;; (tejr-create-theme tejr-theme-name "tejr theme." '((default ((t :foreground "unspecified-fg" :background "unspecified-bg")))))

(setq sdt/theme-custom-rules1
      `(
        (default                          (:g8 :gg) :fg fore :bg back)
        (match                            (:g8 :gg) :fg k :bg w)
        (region                           (:g8 :gg) :inherit match)
        (show-paren-match                 (:g8 :gg) :inherit match :b)
        (tooltip                          (:g8 :gg) :inherit match)

        (mode-line                        (:g8 :gg) :bg k :u)
        (mode-line-inactive               (:g8 :gg) :inherit mode-line :-u)
        (header-line                      (:g8 :gg) :inherit mode-line)

        (font-lock-comment-face           (:g8 :gg) :fg b)
        (font-lock-constant-face          (:g8 :gg) :inherit default)
        (font-lock-function-name-face     (:g8 :gg) :inherit default)
        (font-lock-keyword-face           (:g8 :gg) :inherit default)
        (font-lock-string-face            (:g8 :gg) :inherit default)
        (font-lock-type-face              (:g8 :gg) :inherit default)
        (font-lock-variable-name-face     (:g8 :gg) :inherit default)

        (font-lock-builtin-face           (:g8 :gg) :inherit default)
        (font-lock-warning-face           (:g8 :gg) :inherit default)

        (shadow (:g8 :gg) :inherit default :b)

        ;; (company-preview                  (:g8 :gg) :inherit default :fg g)
        ;; (company-preview-common           (:g8 :gg) :inherit default :fg r)
        (company-tooltip                  (:g8 :gg) :inherit tooltip)
        (linu)
        ;; (company-tooltip-selection        (:g8 :gg) :inherit match)
        ;; (company-tooltip-common           (:g8 :gg) :inherit company-tooltip)
        ;; (company-tooltip-common-selection (:g8 :gg) :inherit company-tooltip-common-selection)
        ;; (company-tooltip-annotation       (:g8 :gg) :fg r)
        ;; (company-tooltip-annotation-selection (:g8 :gg) :fg r)


        ))

(setq custom--inhibit-theme-enable nil)
;; (global-set-key (kbd "<f8>") (lambda()(interactive)(save-buffer)(eval-buffer)(apply 'sdt/modify sdt/theme-custom-rules)))

(setq sdt/theme-custom-rules
      `(
        (default                          :gg       :fg fore :bg back)
        (match                            (:g8 :gg) :fg y :bg k :b)
        ;; (mode-line                        (:g8 :gg) :fg b   :bg k :-b :u)
        (header-line                      (:g8 :gg) :fg w  :bg k :-b :-u)
        ;; (header-line-inactive             (:g8 :gg) :inherit header-line :fg w :bg w)
        (mode-line                        (:g8 :gg) :fg b  :bg k :-b :u)
        (mode-line-inactive               (:g8 :gg) :inherit mode-line :fg w)
        (region                           (:g8 :gg) :fg k  :bg b)
        (show-paren-match                 (:g8 :gg) :fg k  :bg b)
        (tooltip                          (:g8 :gg) :fg b   :bg k :b)

        (font-lock-comment-face           (:g8 :gg) :fg b)
        (font-lock-constant-face          (:g8 :gg) :fg c :b)
        (font-lock-function-name-face     (:g8 :gg) :fg y :b)
        (font-lock-keyword-face           (:g8 :gg) :fg g :b)
        (font-lock-string-face            (:g8 :gg) :fg g)
        (font-lock-type-face              (:g8 :gg) :fg b :b)
        (font-lock-variable-name-face     (:g8 :gg) :fg c :b)

        ;; (font-lock-builtin-face           (:g8 :gg) :inherit font-lock-function-name-face :-b)
        (font-lock-builtin-face           (:g8 :gg) :fg r :-b)
        (font-lock-warning-face           (:g8 :gg) :bg r)

        (lisp-extra-font-lock-backquote             (:g8 :gg) :fg k :bg m)
        (lisp-extra-font-lock-special-variable-name (:g8 :gg) :fg k :bg m)

        (button                           (:g8 :gg) :fg b)
        (highlight                        (:g8 :gg) :fg k :bg w :b)
        (isearch                          (:g8 :gg) :inherit region :bg y)
        (lazy-highlight                   (:g8 :gg) :inherit region)
        (mode-line-buffer-id              (:g8 :gg) :b)
        (shadow                           (:g8 :gg) :fg y)
        (trailing-whitespace              (:g8 :gg) :inherit default :fg r :u)
        (vertical-border                  (:g8 :gg) :inherit mode-line-inactive :-u)

        (link                             (:g8 :gg) :fg c :b :u)
        (link-visited                     :g8 :fg m :b :u :gg :fg "violet" :b :u)
        (info-menu-star                   (:g8 :gg) :fg r)

        (org-document-info-keyword        (:g8 :gg) :fg y) ;; #+title etc.
        (org-document-info                (:g8 :gg) :fg w) ;; value of #+title etc.
        (org-code                         (:g8 :gg) :fg y) ;; ~code~
        (org-verbatim                     (:g8 :gg) :fg y) ;; =verbatim=
        (org-macro                        (:g8 :gg) :fg y) ;; macro invocation
        (org-tag                          (:g8 :gg) :-b)
        (org-table                        (:g8 :gg) :fg c)

        (sh-quoted-exec                   (:g8 :gg) :inherit font-lock-function-name-face :-b)

        (tab-bar                          (:g8 :gg) :inherit mode-line :fg w :-u)
        (tab-bar-tab                      (:g8 :gg) :inherit tab-bar :bg g :fg k :b :-u)
        (tab-bar-tab-inactive             (:g8 :gg) :inherit tab-bar :fg w :b)

        (tab-line                         (:g8 :gg) :fg b :bg k :-b :-u)
        (tab-line-tab                     (:g8 :gg) :inherit tab-line :box nil)
        (tab-line-tab-inactive            (:g8 :gg) :inherit tab-line)
        (tab-line-tab-current             (:g8 :gg) :inherit tab-line :invert)
        (tab-line-highlight               (:g8 :gg) :inherit tab-line :invert :b)

        ;; ---- Faces from packages ----

        ;; avy
        (avy-lead-face                    (:g8 :gg) :inherit isearch)
        ,@(mapcar (lambda (x) `(,x (:g8 :gg) :inherit avy-lead-face))
                  '(avy-lead-face-0 avy-lead-face-1 avy-lead-face-2))

        (company-preview                  (:g8 :gg) :inherit default :fg g)
        (company-preview-common           (:g8 :gg) :inherit default :fg r)
        (company-tooltip                  (:g8 :gg) :inherit tooltip)
        (company-tooltip-selection        (:g8 :gg) :inherit match)
        (company-tooltip-common           (:g8 :gg) :inherit company-tooltip)
        (company-tooltip-common-selection (:g8 :gg) :inherit company-tooltip-common-selection)
        (company-tooltip-annotation       (:g8 :gg) :fg r)
        (company-tooltip-annotation-selection (:g8 :gg) :fg r)

        (compilation-info                 (:g8 :gg) :fg g)
        (compilation-line-number          (:g8 :gg) :fg c)

        (eyebrowse-mode-line-inactive     (:g8 :gg) :inherit mode-line :u)
        (eyebrowse-mode-line-active       (:g8 :gg) :inherit eyebrowse-mode-line-inactive :fg k :bg b :b :-u)
        (eyebrowse-mode-line-separator    (:g8 :gg) :inherit eyebrowse-mode-line-inactive)
        (eyebrowse-mode-line-delimiters   (:g8 :gg) :inherit eyebrowse-mode-line-inactive)

        (highlight-symbol-face            (:g8 :gg) :u)
        (ivy-highlight-face               (:g8 :gg) :bg k :b)
        (ivy-current-match                (:g8 :gg) :inherit match)
        (ivy-minibuffer-match-face-1      (:g8 :gg) :inherit tooltip)
        (ivy-minibuffer-match-face-2      (:g8 :gg) :inherit ivy-minibuffer-match-face-1)
        (ivy-minibuffer-match-face-3      (:g8 :gg) :inherit ivy-minibuffer-match-face-1)
        (ivy-minibuffer-match-face-4      (:g8 :gg) :inherit ivy-minibuffer-match-face-1)

        (org-document-title (:g8 :gg) :fg cb :b)
        (org-level-1        (:g8 :gg) :fg cb :height 140 :b :u)
        (org-level-2        (:g8 :gg) :fg cb :b)
        (org-level-3        (:g8 :gg) :fg bb)
        (org-level-4        (:g8 :gg) :fg mb)
        (org-link           (:g8 :gg) :fg bb)
        (org-todo           (:g8 :gg) :fg rb)

        ;; emacs-refactor etc.
        (popup-face                       (:g8 :gg) :inherit tooltip)
        (popup-menu-selection-face        (:g8 :gg) :inherit match :u)
        (popup-summary-face               (:g8 :gg) :fg r :bg k)
        (popup-menu-summary-face          (:g8 :gg) :inherit popup-summary-face)
        (popup-tip-face                   (:g8 :gg) :fg w :bg k)

        (rainbow-delimiters-base-error-face (:g8 :gg) :fg k :bg m :b)

        (selectrum-completion-annotation  (:g8 :gg) :fg r)
        (selectrum-completion-docsig      (:g8 :gg) :fg g)
        (selectrum-current-candidate      (:g8 :gg) :fg y :bg k)
        (selectrum-primary-highlight      (:g8 :gg) :inherit selectrum-secondary-highlight)
        (selectrum-secondary-highlight    (:g8 :gg) :b)

        (swiper-line-face                 (:g8 :gg) :b :u)
        (swiper-match-face-1              (:g8 :gg) :inherit region)
        (swiper-match-face-2              (:g8 :gg) :inherit swiper-match-face-1)
        (swiper-match-face-3              (:g8 :gg) :inherit swiper-match-face-1)
        (swiper-match-face-4              (:g8 :gg) :inherit swiper-match-face-1)
        (swiper-background-match-face-1   (:g8 :gg) :inherit region)
        (swiper-background-match-face-2   (:g8 :gg) :inherit swiper-background-match-face-1)
        (swiper-background-match-face-3   (:g8 :gg) :inherit swiper-background-match-face-1)
        (swiper-background-match-face-4   (:g8 :gg) :inherit swiper-background-match-face-1)
        ))

;; Rule parser
;; Configurable parts
(setq sdt/displays '((:g8 . ((type tty) (class color) (min-colors 8) (background dark)))
                     (:gg . ((type gtk)))
                     (:gt . t))

      sdt/colors   '(
                     ;; (k  . ((:g8 . "white")   (:gg . "#000000")))
                     ;; (kb . ((:g8 . "white")   (:gg . "#18262f")))
                     ;; (r  . ((:g8 . "red")     (:gg . "#df4243")))
                     ;; (rb . ((:g8 . "red")     (:gg . "#df4243")))
                     ;; (g  . ((:g8 . "green")   (:gg . "#6cb834")))
                     ;; (gb . ((:g8 . "green")   (:gg . "#6cb834")))
                     ;; (y  . ((:g8 . "yellow")  (:gg . "#d4a50c")))
                     ;; (yb . ((:g8 . "yellow")  (:gg . "#d4a50c")))
                     ;; (b  . ((:g8 . "blue")    (:gg . "#23a5d1")))
                     ;; (bb . ((:g8 . "blue")    (:gg . "#23a5d1")))
                     ;; (m  . ((:g8 . "magenta") (:gg . "#9353c5")))
                     ;; (mb . ((:g8 . "magenta") (:gg . "#9353c5")))
                     ;; (c  . ((:g8 . "cyan")    (:gg . "#42bba0")))
                     ;; (cb . ((:g8 . "cyan")    (:gg . "#42bba0")))
                     ;; (w  . ((:g8 . "black")   (:gg . "#868f98")))
                     ;; (wb . ((:g8 . "black")   (:gg . "#969FA8")))
                     ;; (fore . ((:g8 . "black") (:gg . "#000000")))
                     ;; (back . ((:g8 . "white") (:gg . "#eeeeee")))

                     (k  . ((:g8 . "black")   (:gg . "#18262F")))
                     (kb . ((:g8 . "black")   (:gg . "#38464F")))
                     (r  . ((:g8 . "red")     (:gg . "#FF6263")))
                     (rb . ((:g8 . "red")     (:gg . "#FF6263")))
                     (g  . ((:g8 . "green")   (:gg . "#8CD854")))
                     (gb . ((:g8 . "green")   (:gg . "#8CD854")))
                     (y  . ((:g8 . "yellow")  (:gg . "#F4C52C")))
                     (yb . ((:g8 . "yellow")  (:gg . "#F4C52C")))
                     (b  . ((:g8 . "blue")    (:gg . "#43C5F1")))
                     (bb . ((:g8 . "blue")    (:gg . "#43C5F1")))
                     (m  . ((:g8 . "magenta") (:gg . "#B373E5")))
                     (mb . ((:g8 . "magenta") (:gg . "#B373E5")))
                     (c  . ((:g8 . "cyan")    (:gg . "#62DBC0")))
                     (cb . ((:g8 . "cyan")    (:gg . "#62DBC0")))
                     (w  . ((:g8 . "white")   (:gg . "#A6AFB8")))
                     ;; (w  . ((:g8 . "white")   (:gg . "#B6BFC8")))
                     (wb . ((:g8 . "white")   (:gg . "#B6BFC8")))
                     (fore . ((:g8 . "unspecified-fg") (:gg . "#dddddd")))
                     (back . ((:g8 . "unspecified-bg") (:gg . "#10141a")))
                     )

      ;; sdt/colors   '((k  . ((:g8 . "black")   (:gg . "#18262F")))
      ;;                (kb . ((:g8 . "black")   (:gg . "#38464F")))
      ;;                (r  . ((:g8 . "red")     (:gg . "#EF5253")))
      ;;                (rb . ((:g8 . "red")     (:gg . "#FF6263")))
      ;;                (g  . ((:g8 . "green")   (:gg . "#7CC844")))
      ;;                (gb . ((:g8 . "green")   (:gg . "#8CD854")))
      ;;                (y  . ((:g8 . "yellow")  (:gg . "#E4B51C")))
      ;;                (yb . ((:g8 . "yellow")  (:gg . "#F4C52C")))
      ;;                (b  . ((:g8 . "blue")    (:gg . "#33B5E1")))
      ;;                (bb . ((:g8 . "blue")    (:gg . "#43C5F1")))
      ;;                (m  . ((:g8 . "magenta") (:gg . "#A363D5")))
      ;;                (mb . ((:g8 . "magenta") (:gg . "#B373E5")))
      ;;                (c  . ((:g8 . "cyan")    (:gg . "#52CBB0")))
      ;;                (cb . ((:g8 . "cyan")    (:gg . "#62DBC0")))
      ;;                (w  . ((:g8 . "white")   (:gg . "#A6AFB8")))
      ;;                (wb . ((:g8 . "white")   (:gg . "#B6BFC8")))
      ;;                (fore . ((:g8 . "unspecified-fg") (:gg . "#dddddd")))
      ;;                (back . ((:g8 . "unspecified-bg") (:gg . "#10141a"))))
      )

;; Non-configurable parts
(setq sdt/rule-keywords `((:fg      2 ,(lambda (disp key val)
                                         `(:foreground ,(sdt/colorize val disp))))
                          (:bg      2 ,(lambda (disp key val)
                                         `(:background ,(sdt/colorize val disp))))
                          (:b       1 ,(lambda (disp key) '(:weight bold)))
                          (:height      2 ,(lambda (disp key val) `(:height ,val)))
                          (:-b      1 ,(lambda (disp key) '(:weight normal)))
                          (:u       1 ,(lambda (disp key) '(:underline t)))
                          (:-u      1 ,(lambda (disp key) '(:underline nil)))
                          (:box     2 ,(lambda (disp key val) val))
                          (:invert  1 ,(lambda (disp key) '(:inverse-video t)))
                          (:inherit 2 ,(lambda (disp key val) `(:inherit ,val)))))

(defun sdt/colorize (color &optional display)
  (or (cdr (assq display
                 (cdr (assq color sdt/colors))))
      color))

(defun sdt/parse-spec (disp spec)
  ;; DISP = :g8 :gg :gt etc.
  ;; SPEC = face specification (:fg color :bg color :b :-b :u :-u :inherit face)
  ;; return value = parsed-spec
  (let* ((res  nil))

    (while spec
      (let* ((key   (car spec))
             (rule  (assq key sdt/rule-keywords))
             (func  (caddr rule))
             (nargs (cadr rule))
             (args  nil))
        (dotimes (i nargs) (push (pop spec) args))
        (push (apply func disp (nreverse args)) res)))

    (apply 'append (nreverse res))
    ))

(defun sdt/parse-rule (rule)
  "RULE = (face [disp spec ...] ...)
FACE = name of face
DISP = type of display (:g8 :gg :gt etc. or a list of them)
SPEC = face specification (:fg color :bg color :b :u :inherit face)"
  (let* ((face (car rule))
         (rest (cdr rule))

         list1 result)

    (let (d l)
      (while rest
        (setq d (pop rest) l nil)
        (while (and rest (not (or (assq (car rest) sdt/displays)
                                  (and (listp (car rest))
                                       (assq (caar rest) sdt/displays)))))
          (push (pop rest) l))
        (let ((revl (nreverse l)))
          (dolist (d1 (if (listp d) d (list d)))
            (push (cons d1 revl) list1)))))

    (dolist (x list1)
      (let* ((disp (car x))
             (spec (cdr x))
             (parsed-disp (cdr (assq disp sdt/displays)))
             (parsed-spec (sdt/parse-spec disp spec)))
        (push `(,parsed-disp ,parsed-spec) result)))

    `(,face ( ,@result ))
    ;; (face ( ( ((type gtk)) spec ... )
    ;;         ( (...) spec2 ...       ) ) )
    ))

(defun sdt/modify (&rest specs)
  (when custom--inhibit-theme-enable
    (when (yes-or-no-p "Unset custom--inhibit-theme-enable ?")
      (setq custom--inhibit-theme-enable nil)))
  (apply 'custom-theme-set-faces sdt/theme-name (mapcar 'sdt/parse-rule specs)))

