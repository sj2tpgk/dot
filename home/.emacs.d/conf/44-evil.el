(use evil
  :init
                                        ;(setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil) ;; set nil when using evil-collection

  :config
  (setq evil-mode-line-format nil)
  (cua-mode 0)
  (evil-mode 1)
  (setq-default evil-cross-lines nil)
  (setq evil-split-window-below  t
        evil-vsplit-window-right t
        evil-flash-delay         60) ;; Highilght longer time

  (use $surround:gm)
  (use $commentary:m)
  (use $snipe:override-mode)
  (use $terminal-cursor-changer:activate)
  (use $collection
    :c
    (evil-collection-init
     '(help dired xref grep))
    (evil-collection-translate-key
      nil 'evil-motion-state-map
      ;; colemak hnei is qwerty hjkl
      "k" "h"
      "n" "j"
      "e" "k"
      "i" "l"
      "h" "k"
      "j" "n"
      "k" "e"
      "l" "i")))

