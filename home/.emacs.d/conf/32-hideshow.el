(progn ;; hideshow

  (defun hideshow-p () (and (boundp 'hs-minor-mode) hs-minor-mode))
  (defun hs-enable-and-hide () (hs-minor-mode 1) (hs-toggle-hiding))
  
  ;; (definteractive smart-toggle-folding
  ;;   "Enable hideshow and hide all or toggle folding."
  ;;   (cond
  ;;    ((eq major-mode 'org-mode) (org-cycle))
  ;;    ((hideshow-p)              (hs-toggle-hiding))
  ;;    (t                         (hs-enable-and-hide))))

  )
