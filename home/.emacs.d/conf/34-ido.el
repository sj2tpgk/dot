(progn ;; ido

  (ido-mode 1)
  (ido-everywhere)

  ;; Flexible matching
  (setq ido-enable-flex-matching t)

  ;; Display ido results vertically, rather than horizontally
  (setq ido-decorations
        '("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]"
          " [Not readable]" " [Too big]" " [Confirm]"))

  (add-hook 'ido-setup-hook
            (lambda ()
              (define-key ido-completion-map (kbd "<up>")   'ido-prev-match)
              (define-key ido-completion-map (kbd "<down>") 'ido-next-match)))

  ;; ido-ubiquitous
  ;; (package-install 'ido-completing-read+)
  ;; (require 'ido-completing-read+)
  ;; (ido-ubiquitous-mode 1))

  )
