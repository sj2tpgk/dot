(progn ;; hippie-expand

  (setq hippie-expand-try-functions-list
        '(try-complete-file-name-partially
          try-complete-file-name
          try-expand-all-abbrevs
          ;; try-expand-list
          ;; try-expand-line
          try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill
          try-complete-lisp-symbol-partially
          try-complete-lisp-symbol
          ))

  (setq hippie-expand-verbose nil)

  ;; (fset 'hippie/complete-file
  ;;      (make-hippie-expand-function '(try-complete-file-name-partially
  ;;                                     try-complete-file-name)))

  )
