;; TODO: more display support (8-color term, 256-color term, graphical etc.)
;; Custom rules (to be parsed to sexp in the form of arguments to custom-theme-set-faces)
(setq sdt/theme-custom-rules
      `(
        (default                          :gg       :fg fore :bg back)
        (match                            (:g8 :gg) :fg y :bg k :b)
        ;; (mode-line                        (:g8 :gg) :fg b   :bg k :-b :u)
        (mode-line                        (:g8 :gg) :fg b  :bg k :-b :u)
        (mode-line-inactive               (:g8 :gg) :inherit mode-line :fg w  :bg k :u)
        (region                           (:g8 :gg) :fg k  :bg w)
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

        (button                           (:g8 :gg) :fg b)
        (isearch                          (:g8 :gg) :inherit region :bg y)
        (lazy-highlight                   (:g8 :gg) :inherit region)
        (mode-line-buffer-id              (:g8 :gg) :b)
        (shadow                           (:g8 :gg) :fg y)
        (trailing-whitespace              (:g8 :gg) :inherit default :fg r :u)
        (vertical-border                  (:g8 :gg) :inherit mode-line-inactive :-u)

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
        ;; (tab-line                         (:g8 :gg) :fg k :bg b :b :-u)

        ;; Faces from packages

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

        (lisp-extra-font-lock-backquote   (:g8 :gg) :inherit default)

        (org-level-1     (:g8 :gg) :fg cb :height 140 :b :u)
        (org-level-2     (:g8 :gg) :fg bb :b)
        (org-level-3     (:g8 :gg) :fg bb)
        (org-level-4     (:g8 :gg) :fg mb)
        (org-link        (:g8 :gg) :fg bb)
        (org-todo        (:g8 :gg) :fg rb)

        ;; emacs-refactor etc.
        (popup-face                       (:g8 :gg) :inherit tooltip)
        (popup-menu-selection-face        (:g8 :gg) :inherit match :u)
        (popup-summary-face               (:g8 :gg) :fg r :bg k)
        (popup-menu-summary-face          (:g8 :gg) :inherit popup-summary-face)
        (popup-tip-face                   (:g8 :gg) :fg w :bg k)

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
                          (:invert  1 ,(lambda (disp key) '(:inverse-video t)))
                          (:inherit 2 ,(lambda (disp key val) `(:inherit ,val)))))

(defun sdt/colorize (color &optional display)
  (cdr (assq display
             (cdr (assq color sdt/colors)))))

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


;; Create theme
(let ((theme-name 'simple-dark)
      (docstring  "Dark theme based on default 8-color terminal theme with base16-solarflare color palette."))

  ;; Declare theme
  (eval `(deftheme ,theme-name ,docstring))

  ;; <<< IMPORTANT: first applied rule has more priority. >>>

  ;; Apply custom rules (must be applied BEFORE rules from default theme)
  (apply 'custom-theme-set-faces
         theme-name
         (mapcar 'sdt/parse-rule sdt/theme-custom-rules))

  ;; Inherit from default theme
  (custom-theme-set-faces
   theme-name
   ;; '(default ((t (:family "default" :foundry "default" :width normal :height 1 :weight normal :slant normal :underline nil :overline nil :strike-through nil :box nil :inverse-video nil :foreground "unspecified-fg" :background "unspecified-bg" :stipple nil :inherit nil))))
   '(default (( ((type tty))
                (:family "default" :foundry "default" :width normal :height 1 :weight normal :slant normal :underline nil :overline nil :strike-through nil :box nil :inverse-video nil :foreground "unspecified-fg" :background "unspecified-bg" :stipple nil :inherit nil))))
   '(cursor ((((background light)) (:background "black")) (((background dark)) (:background "white"))))
   '(fixed-pitch ((t (:family "Monospace"))))
   '(variable-pitch ((((type w32)) (:foundry "outline" :family "Arial")) (t (:family "Sans Serif"))))
   '(escape-glyph ((((background dark)) (:foreground "cyan")) (((type pc)) (:foreground "magenta")) (t (:foreground "brown"))))
   '(homoglyph ((((background dark)) (:foreground "cyan")) (((type pc)) (:foreground "magenta")) (t (:foreground "brown"))))
   '(minibuffer-prompt ((((background dark)) (:foreground "cyan")) (((type pc)) (:foreground "magenta")) (t (:foreground "medium blue"))))
   '(highlight ((((class color) (min-colors 88) (background light)) (:background "darkseagreen2")) (((class color) (min-colors 88) (background dark)) (:background "darkolivegreen")) (((class color) (min-colors 16) (background light)) (:background "darkseagreen2")) (((class color) (min-colors 16) (background dark)) (:background "darkolivegreen")) (((class color) (min-colors 8)) (:foreground "black" :background "green")) (t (:inverse-video t))))
   '(region ((((class color) (min-colors 88) (background dark))
              (:background "blue3"))

             (((class color) (min-colors 88) (background light) (type gtk))
              (:background "gtk_selection_bg_color" :distant-foreground "gtk_selection_fg_color"))

             (((class color) (min-colors 88) (background light) (type ns))
              (:background "ns_selection_bg_color" :distant-foreground "ns_selection_fg_color"))

             (((class color) (min-colors 88) (background light))
              (:background "lightgoldenrod2"))

             (((class color) (min-colors 16) (background dark))
              (:background "blue3"))

             (((class color) (min-colors 16) (background light))
              (:background "lightgoldenrod2"))

             (((class color) (min-colors 8))
              (:foreground "white" :background "blue"))

             (((type tty) (class mono))
              (:inverse-video t))

             (t (:background "gray"))))
   '(shadow ((((class color grayscale) (min-colors 88) (background light)) (:foreground "grey50")) (((class color grayscale) (min-colors 88) (background dark)) (:foreground "grey70")) (((class color) (min-colors 8) (background light)) (:foreground "green")) (((class color) (min-colors 8) (background dark)) (:foreground "yellow"))))
   '(secondary-selection ((((class color) (min-colors 88) (background light)) (:background "yellow1")) (((class color) (min-colors 88) (background dark)) (:background "SkyBlue4")) (((class color) (min-colors 16) (background light)) (:background "yellow")) (((class color) (min-colors 16) (background dark)) (:background "SkyBlue4")) (((class color) (min-colors 8)) (:foreground "black" :background "cyan")) (t (:inverse-video t))))
   '(trailing-whitespace ((((class color) (background light)) (:background "red1")) (((class color) (background dark)) (:background "red1")) (t (:inverse-video t))))
   '(font-lock-builtin-face ((((class grayscale) (background light)) (:weight bold :foreground "LightGray")) (((class grayscale) (background dark)) (:weight bold :foreground "DimGray")) (((class color) (min-colors 88) (background light)) (:foreground "dark slate blue")) (((class color) (min-colors 88) (background dark)) (:foreground "LightSteelBlue")) (((class color) (min-colors 16) (background light)) (:foreground "Orchid")) (((class color) (min-colors 16) (background dark)) (:foreground "LightSteelBlue")) (((class color) (min-colors 8)) (:weight bold :foreground "blue")) (t (:weight bold))))
   '(font-lock-comment-delimiter-face ((default (:inherit (font-lock-comment-face)))))
   '(font-lock-comment-face ((((class grayscale) (background light)) (:slant italic :weight bold :foreground "DimGray")) (((class grayscale) (background dark)) (:slant italic :weight bold :foreground "LightGray")) (((class color) (min-colors 88) (background light)) (:foreground "Firebrick")) (((class color) (min-colors 88) (background dark)) (:foreground "chocolate1")) (((class color) (min-colors 16) (background light)) (:foreground "red")) (((class color) (min-colors 16) (background dark)) (:foreground "red1")) (((class color) (min-colors 8) (background light)) (:foreground "red")) (((class color) (min-colors 8) (background dark)) (:foreground "yellow")) (t (:slant italic :weight bold))))
   '(font-lock-constant-face ((((class grayscale) (background light)) (:underline (:color foreground-color :style line) :weight bold :foreground "LightGray")) (((class grayscale) (background dark)) (:underline (:color foreground-color :style line) :weight bold :foreground "Gray50")) (((class color) (min-colors 88) (background light)) (:foreground "dark cyan")) (((class color) (min-colors 88) (background dark)) (:foreground "Aquamarine")) (((class color) (min-colors 16) (background light)) (:foreground "CadetBlue")) (((class color) (min-colors 16) (background dark)) (:foreground "Aquamarine")) (((class color) (min-colors 8)) (:foreground "magenta")) (t (:underline (:color foreground-color :style line) :weight bold))))
   '(font-lock-doc-face ((t (:inherit (font-lock-string-face)))))
   '(font-lock-function-name-face ((((class color) (min-colors 88) (background light)) (:foreground "Blue1")) (((class color) (min-colors 88) (background dark)) (:foreground "LightSkyBlue")) (((class color) (min-colors 16) (background light)) (:foreground "Blue")) (((class color) (min-colors 16) (background dark)) (:foreground "LightSkyBlue")) (((class color) (min-colors 8)) (:weight bold :foreground "blue")) (t (:weight bold :inverse-video t))))
   '(font-lock-keyword-face ((((class grayscale) (background light)) (:weight bold :foreground "LightGray")) (((class grayscale) (background dark)) (:weight bold :foreground "DimGray")) (((class color) (min-colors 88) (background light)) (:foreground "Purple")) (((class color) (min-colors 88) (background dark)) (:foreground "Cyan1")) (((class color) (min-colors 16) (background light)) (:foreground "Purple")) (((class color) (min-colors 16) (background dark)) (:foreground "Cyan")) (((class color) (min-colors 8)) (:weight bold :foreground "cyan")) (t (:weight bold))))
   '(font-lock-negation-char-face ((t nil)))
   '(font-lock-preprocessor-face ((t (:inherit (font-lock-builtin-face)))))
   '(font-lock-regexp-grouping-backslash ((t (:inherit (bold)))))
   '(font-lock-regexp-grouping-construct ((t (:inherit (bold)))))
   '(font-lock-string-face ((((class grayscale) (background light)) (:slant italic :foreground "DimGray")) (((class grayscale) (background dark)) (:slant italic :foreground "LightGray")) (((class color) (min-colors 88) (background light)) (:foreground "VioletRed4")) (((class color) (min-colors 88) (background dark)) (:foreground "LightSalmon")) (((class color) (min-colors 16) (background light)) (:foreground "RosyBrown")) (((class color) (min-colors 16) (background dark)) (:foreground "LightSalmon")) (((class color) (min-colors 8)) (:foreground "green")) (t (:slant italic))))
   '(font-lock-type-face ((((class grayscale) (background light)) (:weight bold :foreground "Gray90")) (((class grayscale) (background dark)) (:weight bold :foreground "DimGray")) (((class color) (min-colors 88) (background light)) (:foreground "ForestGreen")) (((class color) (min-colors 88) (background dark)) (:foreground "PaleGreen")) (((class color) (min-colors 16) (background light)) (:foreground "ForestGreen")) (((class color) (min-colors 16) (background dark)) (:foreground "PaleGreen")) (((class color) (min-colors 8)) (:foreground "green")) (t (:underline (:color foreground-color :style line) :weight bold))))
   '(font-lock-variable-name-face ((((class grayscale) (background light)) (:slant italic :weight bold :foreground "Gray90")) (((class grayscale) (background dark)) (:slant italic :weight bold :foreground "DimGray")) (((class color) (min-colors 88) (background light)) (:foreground "sienna")) (((class color) (min-colors 88) (background dark)) (:foreground "LightGoldenrod")) (((class color) (min-colors 16) (background light)) (:foreground "DarkGoldenrod")) (((class color) (min-colors 16) (background dark)) (:foreground "LightGoldenrod")) (((class color) (min-colors 8)) (:weight light :foreground "yellow")) (t (:slant italic :weight bold))))
   '(font-lock-warning-face ((t (:inherit (error)))))
   '(button ((t (:inherit (link)))))
   '(link ((((class color) (min-colors 88) (background light)) (:underline (:color foreground-color :style line) :foreground "RoyalBlue3")) (((class color) (background light)) (:underline (:color foreground-color :style line) :foreground "blue")) (((class color) (min-colors 88) (background dark)) (:underline (:color foreground-color :style line) :foreground "cyan1")) (((class color) (background dark)) (:underline (:color foreground-color :style line) :foreground "cyan")) (t (:inherit (underline)))))
   '(link-visited ((default (:inherit (link))) (((class color) (background light)) (:foreground "magenta4")) (((class color) (background dark)) (:foreground "violet"))))
   '(fringe ((((class color) (background light)) (:background "grey95")) (((class color) (background dark)) (:background "grey10")) (t (:background "gray"))))
   '(header-line ((default (:inherit (mode-line))) (((type tty)) (:underline (:color foreground-color :style line) :inverse-video nil)) (((class color grayscale) (background light)) (:box nil :foreground "grey20" :background "grey90")) (((class color grayscale) (background dark)) (:box nil :foreground "grey90" :background "grey20")) (((class mono) (background light)) (:underline (:color foreground-color :style line) :box nil :inverse-video nil :foreground "black" :background "white")) (((class mono) (background dark)) (:underline (:color foreground-color :style line) :box nil :inverse-video nil :foreground "white" :background "black"))))
   '(tooltip ((((class color)) (:inherit (variable-pitch) :foreground "black" :background "lightyellow")) (t (:inherit (variable-pitch)))))
   '(mode-line ((((class color) (min-colors 88)) (:foreground "black" :background "grey75" :box (:line-width -1 :color nil :style released-button))) (t (:inverse-video t))))
   '(mode-line-buffer-id ((t (:weight bold))))
   '(mode-line-emphasis ((t (:weight bold))))
   '(mode-line-highlight ((((class color) (min-colors 88)) (:box (:line-width 2 :color "grey40" :style released-button))) (t (:inherit (highlight)))))
   '(mode-line-inactive ((default (:inherit (mode-line))) (((class color) (min-colors 88) (background light)) (:background "grey90" :foreground "grey20" :box (:line-width -1 :color "grey75" :style nil) :weight light)) (((class color) (min-colors 88) (background dark)) (:background "grey30" :foreground "grey80" :box (:line-width -1 :color "grey40" :style nil) :weight light))))
   '(isearch ((((class color) (min-colors 88) (background light)) (:foreground "lightskyblue1" :background "magenta3")) (((class color) (min-colors 88) (background dark)) (:foreground "brown4" :background "palevioletred2")) (((class color) (min-colors 16)) (:foreground "cyan1" :background "magenta4")) (((class color) (min-colors 8)) (:foreground "cyan1" :background "magenta4")) (t (:inverse-video t))))
   '(isearch-fail ((((class color) (min-colors 88) (background light)) (:background "RosyBrown1")) (((class color) (min-colors 88) (background dark)) (:background "red4")) (((class color) (min-colors 16)) (:background "red")) (((class color) (min-colors 8)) (:background "red")) (((class color grayscale)) (:foreground "grey")) (t (:inverse-video t))))
   '(lazy-highlight ((((class color) (min-colors 88) (background light)) (:background "paleturquoise")) (((class color) (min-colors 88) (background dark)) (:background "paleturquoise4")) (((class color) (min-colors 16)) (:background "turquoise3")) (((class color) (min-colors 8)) (:background "turquoise3")) (t (:underline (:color foreground-color :style line)))))
   '(match ((((class color) (min-colors 88) (background light)) (:background "yellow1")) (((class color) (min-colors 88) (background dark)) (:background "RoyalBlue3")) (((class color) (min-colors 8) (background light)) (:foreground "black" :background "yellow")) (((class color) (min-colors 8) (background dark)) (:foreground "white" :background "blue")) (((type tty) (class mono)) (:inverse-video t)) (t (:background "gray"))))
   '(next-error ((t (:inherit (region)))))
   '(query-replace ((t (:inherit (isearch))))))

  (provide-theme theme-name)

  )

;; '(default button cursor escape-glyph fixed-pitch
;;    font-lock-builtin-face font-lock-comment-delimiter-face font-lock-comment-face font-lock-constant-face font-lock-doc-face font-lock-function-name-face font-lock-keyword-face font-lock-negation-char-face font-lock-preprocessor-face font-lock-regexp-grouping-backslash font-lock-regexp-grouping-construct font-lock-string-face font-lock-type-face font-lock-variable-name-face font-lock-warning-face
;;    fringe header-line highlight homoglyph isearch isearch-fail lazy-highlight link link-visited
;;    match minibuffer-prompt
;;    mode-line mode-line-buffer-id mode-line-emphasis mode-line-highlight mode-line-inactive
;;    next-error query-replace
;;    region secondary-selection
;;    shadow tooltip trailing-whitespace variable-pitch)

;; (custom-set-faces
;;  '(default ((t (:family "default" :foundry "default" :width normal :height 1 :weight bold :slant normal :underline nil :overline nil :strike-through nil :box nil :inverse-video nil :foreground "unspecified-fg" :background "unspecified-bg" :stipple nil :inherit nil))))
;;  '(region ((((class color) (min-colors 88) (background dark))
;;             (:background "blue3"))

;;            (((class color) (min-colors 88) (background light) (type gtk))
;;             (:background "gtk_selection_bg_color" :distant-foreground "gtk_selection_fg_color"))

;;            (((class color) (min-colors 88) (background light) (type ns))
;;             (:background "ns_selection_bg_color" :distant-foreground "ns_selection_fg_color"))

;;            (((class color) (min-colors 88) (background light))
;;             (:background "lightgoldenrod2"))

;;            (((class color) (min-colors 16) (background dark))
;;             (:background "blue3"))

;;            (((class color) (min-colors 16) (background light))
;;             (:background "lightgoldenrod2"))

;;            (((class color) (min-colors 8))
;;             (:foreground "white" :background "blue"))

;;            (((type tty) (class mono))
;;             (:inverse-video t))

;;            (t (:background "gray"))))
;;  )
