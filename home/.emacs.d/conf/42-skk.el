(use-package skk
  :straight ddskk

  :commands (skk-mode)

  :init
  (global-set-key (kbd "<zenkaku-hankaku>") 'skk-mode)
  (global-set-key (kbd "C-x C-j")           'skk-mode)
  (setq skk-status-indicator nil)

  :config
  (setq skk-use-viper nil
        skk-tut-file  "~/.emacs.d/initels/SKK.tut"
        skk-init-file "~/.emacs.d/initels/skkinit.el")

  (progn ;; モード遷移 ( 一部は かなテーブルに記述 ) {{{
    ;; latin mode --> かなmode
    (define-key skk-latin-mode-map (kbd "<zenkaku-hankaku>") #'skk-kakutei)
    (define-key skk-latin-mode-map (kbd "C-k") #'skk-kakutei)

    ;; かなmode --> latin mode
    (define-key skk-j-mode-map     (kbd "<zenkaku-hankaku>") #'skk-latin-mode)
    (define-key skk-j-mode-map     (kbd "C-k") #'skk-latin-mode)

    ;; 通常時は L でひらがなとカタカナをトグル
    (define-key skk-j-mode-map     (kbd "L")                 #'skk-toggle-characters)
    ) ;; }}}

  (progn ;; 動的補完 {{{
    (setq skk-dcomp-activate t)
    (defun my-skk-add-comp-map ()
      ;; 漢字変換時: TABで補完+変換
      (define-key skk-j-mode-map (kbd "<tab>") 'skk-comp-start-henkan)
      ;; abbrev時: TABでアルファベットのまま補完 (さらにSPCで片仮名変換)
      (define-key skk-abbrev-mode-map (kbd "<tab>") 'skk-try-completion)
      )
    ;; マッピングをフックなしで実行しても反映されない?
    (add-hook 'skk-mode-hook 'my-skk-add-comp-map)
    ;; (kbd ) がS-SPCに効かない(?) ので (setq skk-start-henkan-with-completion-char (kbd ...)) はできない
    ) ;; }}}

  (progn ;; Commit+Space {{{
    (defun my-skk-commit-space (&optional arg word)
      "確定した後、スペースを入力する。"
      (interactive "*p")
      (skk-kakutei)
      ;; don't use skk-insert here;
      ;; skk-insert ignores arg and input the pressed key.
      (insert " ")
      )
    (define-key skk-j-mode-map      (kbd "S-SPC") 'my-skk-commit-space)
    (define-key skk-abbrev-mode-map (kbd "S-SPC") 'my-skk-commit-space)
    ) ;; }}}

  (progn ;; Abbrevモード Mapping {{{
    ;; SPC:スペース, 下キー:変換, 右キー:確定
    (defun my-skk-add-abbrev-mode-keys ()
      (define-key skk-abbrev-mode-map (kbd "SPC")     #'skk-abbrev-insert)
      (define-key skk-abbrev-mode-map (kbd "<down>")  #'skk-start-henkan)
      (define-key skk-abbrev-mode-map (kbd "<right>") #'skk-kakutei)
      )
    (my-skk-add-abbrev-mode-keys)
    (add-hook 'skk-mode-hook 'my-skk-add-abbrev-mode-keys)
    ) ;; }}}

  (progn ;; 変換中 Mapping {{{
    ;; You can also use 'x'
    (defun my-skk-prev-candidate-or-up ()
      "▼モードの場合は skk-previous-candidate を行い、それ以外では up"
      (interactive)
      (if (eq skk-henkan-mode 'active)
          (skk-previous-candidate)
        (evil-previous-line)
        ))

    (define-key skk-j-mode-map (kbd "<up>") 'my-skk-prev-candidate-or-up)
    ) ;; }}}

  (progn ;; 変換候補 {{{
    (setq skk-show-inline nil)                                     ;; nil (use minibuffer), vertical (カーソル位置)
    (setq skk-show-candidates-nth-henkan-char 5)                   ;; SPCをn回押すと候補表示
    (setq skk-henkan-show-candidates-keys '(?0 ?1 ?2 ?3 ?4 ?5 ?6)) ;; 候補選択用文字 (?a のようにする; 7の倍数個)
    (setq skk-henkan-number-to-display-candidates 6)               ;; 一度に表示する候補数 (length skk-henkan-show-candidates-keys) より大きいとエラー
    (setq skk-inline-show-face (skk-make-face 'PeachPuff2))        ;; 候補の装飾 ('fg/bg の形式も可)
    ) ;; }}}

  (progn ;; Misc {{{
    (setq skk-egg-like-newline t)  ; 変換の Ret で改行しない

    (defun my-skk-toggle-kana-or-abbrev-mode (arg)
      "▽モードの場合は toggle-kana を行い、それ以外では abbrev-mode に入る"
      (if (eq skk-henkan-mode 'on) (skk-toggle-kana arg) (skk-abbrev-mode arg))
      )

    (setq skk-search-katakana t) ;; 片仮名語も変換で出す
    ;; 片仮名語を辞書に自動登録したい
    ;; hook?
    ;; それよりも my-skk-toggle-kana-or-abbrev-mode で呼び出す関数を変更した方が
    ) ;; }}}

  (progn ;; Azik {{{
    (setq skk-use-azik t)
    ;; (defun my-skk-double-k (arg)
    ;;   (cond
    ;;    (skk-katakana (insert "カイ"))
    ;;    ((and (not skk-katakana) skk-j-mode) (insert "かい")))
    ;;   (skk-set-char-before-as-okurigana))

    ;; (setq skk-downcase-alist '((?L . ?l) (?* . ?:)))
    ;; (add-to-list 'skk-set-henkan-point-key ?*)
    ;; (setq skk-okuri-char-alist '((":" . "t")))

    ;; Add ?L ?X ?Q to start henkan with L X Q
    ;; if you forget this setting, L X Q are literally input.
    ;; (setq skk-set-henkan-point-key
    ;;       (append '(?* ?Q ?L ?X) skk-set-henkan-point-key))
    (setq skk-downcase-alist
          (append '((?* . ?:)) skk-downcase-alist))

    (let ((rules-for-symbols
           '((" " " ") ("h " " ") ("z " "　")
             )))
      (mapc (lambda (x)
              (add-to-list 'skk-rom-kana-rule-list `(,(car x) nil ,(cadr x))))
            rules-for-symbols))
    (add-to-list 'skk-rom-kana-rule-list `(":" nil ("ッ" . "っ")))
    ;; (define-key skk-j-mode-map (kbd ":") #'skk-insert)
    ) ;; }}}
  (defun read-a-sexp-from-file (file)
    "Like scheme's (call-with-input-file file read)"
    (with-temp-buffer
      (info-insert-file-contents file)
      (goto-char (point-min))
      (read (current-buffer))))
  (defun pair-to-skk-rule (pair)
    "Convert (\"ka\" . \"か\") to (\"ka\" nil (\"カ\" . \"か\")) etc."
    (let* ((in-str   (car pair))
           (out-hira (cdr pair))
           (out-kata (skk-hiragana-to-katakana out-hira)))
      (if (string-equal out-hira out-kata)
          `(,in-str nil ,out-hira)
        `(,in-str nil (,out-kata . ,out-hira)))))
  (defun my-skk-azik-add-rules ()
    (setq skk-rom-kana-base-rule-list '())
    (setq skk-rom-kana-rule-list '())
    ;; Note: 後に add-to-list 追加されたルールの方が優先度が *低い*
    ;; よって以下の append の最初の引数に my-skk-custom-rules を指定しないと、
    ;; ddskk-specific のルールが uim 用のルールで上書きされる可能性がある。
    ;; Should remove conflicting rules
    (let ((skk-rule-file "~/.uim.d/azik_colemakdh/rules.scm")
          (my-skk-specific-rules
           '(("kK" nil my-skk-double-k)
             ("l"  nil my-skk-toggle-kana-or-abbrev-mode)
             (":"  nil ("ッ" . "っ"))
             ("h " nil " ")
             ("z " nil "　")
             ("s-" nil "- ")
             ("s:" nil ": ")
             ;; ("L"  "tg" nil) ; see skk-set-henkan-point-key
             ;; ("X"  "sh" nil)
             )))
      (let ((rules (mapcar 'pair-to-skk-rule (read-a-sexp-from-file skk-rule-file))))
        (dolist (rule (append my-skk-specific-rules rules))
          (add-to-list 'skk-rom-kana-rule-list rule)))))
  (my-skk-azik-add-rules)
  (add-hook 'skk-mode-hook #'my-skk-azik-add-rules)

  (defun skk-workaround-help (tree)
    (cond
     ((and (listp tree) (listp (cdr tree))
           (stringp (cadr tree)) (string-equal (cadr tree) "f"))
      (setcdr (cdddr tree) nil))
     (t (while (and tree (listp tree)) (skk-workaround-help (pop tree))))))

  (add-hook 'skk-mode-hook (lambda () (skk-workaround-help skk-rule-tree)))
  )



;; uim-skk との辞書の統合
