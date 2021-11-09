;; -*- lexical-binding: t; -*-

;; todo: ability to depend on other theme.

(defconst sdt-theme-name 'simple-dark)

(setq custom--inhibit-theme-enable nil)
;; (global-set-key (kbd "<f8>") (lambda()(interactive)(save-buffer)(eval-buffer)(apply 'sdt-modify sdt-theme-custom-rules)))
(global-set-key (kbd "<f7>") (lambda()(interactive)(save-buffer)(eval-buffer)(sdt-reload)))

(setq sdt-rules
      `(
        (default                          :gg       :fg fore :bg back)
        (match                            (:gf :gg) :fg y :bg "#445" :b (:g8) :fg y :bg k :b)
        (header-line                      (:gf :g8 :gg) :fg w  :bg k :-b :-u)
        (mode-line                        (:gf :g8 :gg) :fg b  :bg k :-b :u)
        (mode-line-inactive               (:gf :g8 :gg) :inherit mode-line :fg w)
        (region                           (:gf :g8 :gg) :fg k  :bg b)
        (show-paren-match                 (:gf :g8 :gg) :fg k  :bg b)
        (tooltip                          (:gf :gg) :fg b :bg "#334" :b (:g8) :fg b  :bg k :b)

        (font-lock-comment-face           (:gf :g8 :gg) :fg b)
        (font-lock-constant-face          (:gf :g8 :gg) :fg c :b)
        (font-lock-function-name-face     (:gf :g8 :gg) :fg y :b)
        (font-lock-keyword-face           (:gf :g8 :gg) :fg g :b)
        (font-lock-string-face            (:gf :g8 :gg) :fg g)
        (font-lock-type-face              (:gf :g8 :gg) :fg b :b)
        (font-lock-variable-name-face     (:gf :g8 :gg) :fg c :b)

        (font-lock-builtin-face           (:gf :g8 :gg) :fg r :-b)
        (font-lock-warning-face           (:gf :g8 :gg) :bg r)

        (lisp-extra-font-lock-backquote             (:gf :g8 :gg) :fg k :bg m)
        (lisp-extra-font-lock-special-variable-name (:gf :g8 :gg) :fg k :bg m)

        (button                           (:gf :g8 :gg) :fg b)
        (highlight                        (:gf :g8 :gg) :fg k :bg w :b)
        (isearch                          (:gf :g8 :gg) :inherit region :bg y)
        (lazy-highlight                   (:gf :g8 :gg) :inherit region)
        (mode-line-buffer-id              (:gf :g8 :gg) :b)
        (shadow                           (:gf :g8 :gg) :fg y)
        (trailing-whitespace              (:gf :g8 :gg) :inherit default :fg r :u)
        (vertical-border                  (:gf :g8 :gg) :inherit mode-line-inactive :-u)

        (line-number                      (:gf :gg) :fg "#999" :gt :inherit shadow)
        (line-number-current-line         (:gf :g8 :gg) :fg w :b :inherit line-number)

        (link                             (:gf :g8 :gg) :fg c :b :u)
        (link-visited                     :g8 :fg m :b :u :gg :fg "violet" :b :u)
        (info-menu-star                   (:gf :g8 :gg) :fg r)

        (org-document-info-keyword        (:gf :g8 :gg) :fg y) ;; #+title etc.
        (org-document-info                (:gf :g8 :gg) :fg w) ;; value of #+title etc.
        (org-code                         (:gf :g8 :gg) :fg y) ;; ~code~
        (org-verbatim                     (:gf :g8 :gg) :fg y) ;; =verbatim=
        (org-macro                        (:gf :g8 :gg) :fg y) ;; macro invocation
        (org-tag                          (:gf :g8 :gg) :-b)
        (org-table                        (:gf :g8 :gg) :fg c)

        (sh-quoted-exec                   (:gf :g8 :gg) :inherit font-lock-function-name-face :-b)

        (tab-bar                          (:gf :g8 :gg) :inherit mode-line :fg w :-u)
        (tab-bar-tab                      (:gf :g8 :gg) :inherit tab-bar :bg g :fg k :b :-u)
        (tab-bar-tab-inactive             (:gf :g8 :gg) :inherit tab-bar :fg w :b)

        (tab-line                         (:gf :g8 :gg) :fg b :bg k :-b :-u)
        (tab-line-tab                     (:gf :g8 :gg) :inherit tab-line :box nil)
        (tab-line-tab-inactive            (:gf :g8 :gg) :inherit tab-line)
        (tab-line-tab-current             (:gf :g8 :gg) :inherit tab-line :invert)
        (tab-line-highlight               (:gf :g8 :gg) :inherit tab-line :invert :b)

        ;; ---- Faces from packages ----

        ;; avy
        (avy-lead-face                    (:gf :g8 :gg) :inherit isearch)
        ,@(mapcar (lambda (x) `(,x (:gf :g8 :gg) :inherit avy-lead-face))
                  '(avy-lead-face-0 avy-lead-face-1 avy-lead-face-2))

        (company-preview                  (:gf :g8 :gg) :inherit default :fg g)
        (company-preview-common           (:gf :g8 :gg) :inherit default :fg r)
        (company-tooltip                  (:gf :g8 :gg) :inherit tooltip)
        (company-tooltip-selection        (:gf :g8 :gg) :inherit match)
        (company-tooltip-common           (:gf :g8 :gg) :inherit company-tooltip)
        (company-tooltip-common-selection (:gf :g8 :gg) :inherit company-tooltip-common-selection)
        (company-tooltip-annotation       (:gf :g8 :gg) :fg r)
        (company-tooltip-annotation-selection (:gf :g8 :gg) :fg r)

        (compilation-info                 (:gf :g8 :gg) :fg g)
        (compilation-line-number          (:gf :g8 :gg) :fg c)

        (eyebrowse-mode-line-inactive     (:gf :g8 :gg) :inherit mode-line :u)
        (eyebrowse-mode-line-active       (:gf :g8 :gg) :inherit eyebrowse-mode-line-inactive :fg k :bg b :b :-u)
        (eyebrowse-mode-line-separator    (:gf :g8 :gg) :inherit eyebrowse-mode-line-inactive)
        (eyebrowse-mode-line-delimiters   (:gf :g8 :gg) :inherit eyebrowse-mode-line-inactive)

        (highlight-symbol-face            (:gf :g8 :gg) :u)
        (ivy-highlight-face               (:gf :g8 :gg) :bg k :b)
        (ivy-current-match                (:gf :g8 :gg) :inherit match)
        (ivy-minibuffer-match-face-1      (:gf :g8 :gg) :inherit tooltip)
        (ivy-minibuffer-match-face-2      (:gf :g8 :gg) :inherit ivy-minibuffer-match-face-1)
        (ivy-minibuffer-match-face-3      (:gf :g8 :gg) :inherit ivy-minibuffer-match-face-1)
        (ivy-minibuffer-match-face-4      (:gf :g8 :gg) :inherit ivy-minibuffer-match-face-1)

        (org-document-title (:gf :g8 :gg) :fg c :b)
        (org-level-1        (:gf :g8 :gg) :fg c :height 140 :b :u)
        (org-level-2        (:gf :g8 :gg) :fg c :b)
        (org-level-3        (:gf :g8 :gg) :fg b)
        (org-level-4        (:gf :g8 :gg) :fg m)
        (org-link           (:gf :g8 :gg) :fg b)
        (org-todo           (:gf :g8 :gg) :fg r)

        ;; emacs-refactor etc.
        (popup-face                       (:gf :g8 :gg) :inherit tooltip)
        (popup-menu-selection-face        (:gf :g8 :gg) :inherit match :u)
        (popup-summary-face               (:gf :g8 :gg) :fg r :bg k)
        (popup-menu-summary-face          (:gf :g8 :gg) :inherit popup-summary-face)
        (popup-tip-face                   (:gf :g8 :gg) :fg w :bg k)

        (rainbow-delimiters-base-error-face (:gf :g8 :gg) :fg k :bg m :b)

        (selectrum-completion-annotation  (:gf :g8 :gg) :fg r)
        (selectrum-completion-docsig      (:gf :g8 :gg) :fg g)
        (selectrum-current-candidate      (:gf :g8 :gg) :fg y :bg k)
        (selectrum-primary-highlight      (:gf :g8 :gg) :inherit selectrum-secondary-highlight)
        (selectrum-secondary-highlight    (:gf :g8 :gg) :b)

        (swiper-line-face                 (:gf :g8 :gg) :b :u)
        (swiper-match-face-1              (:gf :g8 :gg) :inherit region)
        (swiper-match-face-2              (:gf :g8 :gg) :inherit swiper-match-face-1)
        (swiper-match-face-3              (:gf :g8 :gg) :inherit swiper-match-face-1)
        (swiper-match-face-4              (:gf :g8 :gg) :inherit swiper-match-face-1)
        (swiper-background-match-face-1   (:gf :g8 :gg) :inherit region)
        (swiper-background-match-face-2   (:gf :g8 :gg) :inherit swiper-background-match-face-1)
        (swiper-background-match-face-3   (:gf :g8 :gg) :inherit swiper-background-match-face-1)
        (swiper-background-match-face-4   (:gf :g8 :gg) :inherit swiper-background-match-face-1)
        ))

;; (tty-display-color-cells)
;; Rule parser
;; Configurable parts
(setq sdt-displays '((:g8 . ((type tty) (class color) (min-colors 8) (background dark)))
                     (:gf . ((type tty) (min-colors 16777216)))
                     (:gg . ((type gtk)))
                     (:gt . t))

      sdt-colors   '(
                     (k  . ((:g8 . "black")   (:gf . "#18262F") (:gg . "#18262F")))
                     (r  . ((:g8 . "red")     (:gf . "#FF6263") (:gg . "#FF6263")))
                     (g  . ((:g8 . "green")   (:gf . "#8CD854") (:gg . "#8CD854")))
                     (y  . ((:g8 . "yellow")  (:gf . "#F4C52C") (:gg . "#F4C52C")))
                     (b  . ((:g8 . "blue")    (:gf . "#43C5F1") (:gg . "#43C5F1")))
                     (m  . ((:g8 . "magenta") (:gf . "#B373E5") (:gg . "#B373E5")))
                     (c  . ((:g8 . "cyan")    (:gf . "#62DBC0") (:gg . "#62DBC0")))
                     (w  . ((:g8 . "white")   (:gf . "#A6AFB8") (:gg . "#A6AFB8")))
                     (fore . ((:g8 . "unspecified-fg") (:gf . "unspecified-fg") (:gg . "#dddddd")))
                     (back . ((:g8 . "unspecified-bg") (:gf . "unspecified-bg") (:gg . "#10141a")))
                     )
      )

;; Non-configurable parts
(setq sdt-rule-keywords `((:fg      2 ,(lambda (disp key val)
                                         `(:foreground ,(sdt-colorize val disp))))
                          (:bg      2 ,(lambda (disp key val)
                                         `(:background ,(sdt-colorize val disp))))
                          (:b       1 ,(lambda (disp key) '(:weight bold)))
                          (:height      2 ,(lambda (disp key val) `(:height ,val)))
                          (:-b      1 ,(lambda (disp key) '(:weight normal)))
                          (:u       1 ,(lambda (disp key) '(:underline t)))
                          (:-u      1 ,(lambda (disp key) '(:underline nil)))
                          (:box     2 ,(lambda (disp key val) val))
                          (:invert  1 ,(lambda (disp key) '(:inverse-video t)))
                          (:inherit 2 ,(lambda (disp key val) `(:inherit ,val)))))

(defun sdt-colorize (color &optional display)
  (or (cdr (assq display
                 (cdr (assq color sdt-colors))))
      color))

(defun sdt-parse-spec (disp spec)
  ;; DISP = :g8 :gg :gt etc.
  ;; SPEC = face specification (:fg color :bg color :b :-b :u :-u :inherit face)
  ;; return value = parsed-spec
  (let* ((res  nil))

    (while spec
      (let* ((key   (car spec))
             (rule  (assq key sdt-rule-keywords))
             (func  (caddr rule))
             (nargs (cadr rule))
             (args  nil))
        (dotimes (i nargs) (push (pop spec) args))
        (push (apply func disp (nreverse args)) res)))

    (apply 'append (nreverse res))
    ))

(defun sdt-parse-rule (rule)
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
        (while (and rest (not (or (assq (car rest) sdt-displays)
                                  (and (listp (car rest))
                                       (assq (caar rest) sdt-displays)))))
          (push (pop rest) l))
        (let ((revl (nreverse l)))
          (dolist (d1 (if (listp d) d (list d)))
            (push (cons d1 revl) list1)))))

    (dolist (x list1)
      (let* ((disp (car x))
             (spec (cdr x))
             (parsed-disp (cdr (assq disp sdt-displays)))
             (parsed-spec (sdt-parse-spec disp spec)))
        (push `(,parsed-disp ,parsed-spec) result)))

    `(,face ( ,@result ))
    ;; (face ( ( ((type gtk)) spec ... )
    ;;         ( (...) spec2 ...       ) ) )
    ))

(defun sdt-create-theme (theme-name docstring rules-for-custom-set)
  ;; create theme
  ;; (docstring  "Dark theme based on default 8-color terminal theme with base16-solarflare color palette.")

  ;; Declare theme
  (eval `(deftheme ,theme-name ,docstring))

  ;; <<< IMPORTANT: first applied rule has more priority. >>>

  ;; Apply custom rules (must be applied BEFORE rules from default theme)
  (apply 'custom-theme-set-faces theme-name rules-for-custom-set)

  ;; Finally provide the theme
  (provide-theme theme-name))

(defun sdt-reload ()
  (interactive)
  ;; (when custom--inhibit-theme-enable
  ;;   (when (yes-or-no-p "Unset custom--inhibit-theme-enable ?")
  ;;     (setq custom--inhibit-theme-enable nil)))
  (sdt-create-theme sdt-theme-name
                    "Dark theme with mostly 8 color + base16-solarflare palette."(mapcar 'sdt-parse-rule sdt-rules)))

(sdt-reload)
