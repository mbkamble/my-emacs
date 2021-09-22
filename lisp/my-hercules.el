;;* My hercules definitions          -*- lexical-binding: t; -*-

(hercules-def
 ;; read further to see why this works
 :toggle-funs #'org-babel-mode
 :keymap 'org-babel-map
 :transient t)
 
;; tweak binding to taste
(define-key org-mode-map (kbd "C-c t") #'org-babel-mode)

(hercules-def
 :toggle-funs #'org-babel-mode
 :keymap 'org-babel-map
 :whitelist-keys '("n" "p" "t")
:transient t)
 
(define-key <map-symbol> (kbd "<key>") #'org-babel-mode)

(hercules-def
 :toggle-funs #'lispy-forward
 :keymap 'lispy-mode-map
 :transient t)

(hercules-def
 :toggle-funs #'my-foo-mode
 :keymap 'lispy-mode-map
 ;; :whitelist-keys '("n" "p" "t")
 :transient t)
(bind-key "<return>" nil 'lispy-mode-map)
(bind-key "t" nil 'lispy-mode-map)
(bind-key "C-c e" #'my-foo-mode)

(hercules-def
 :toggle-funs #'my-ctl-cd-mode
 :keymap 'org-mode-map
 :transient t)
(bind-key "C-c d" #'my-ctl-cd-mode)

(hercules-def
 :toggle-funs #'org-babel-mode-foo
 :keymap 'org-babel-map
 :transient t)
(bind-key "C-c f" #'org-babel-mode-foo)

(setq my-fictional-keymap (make-sparse-keymap))
(bind-key "a" #'org-show-all 'my-fictional-keymap)

(hercules-def
 :show-funs #'foo-goto 			; we can choose arb name
 :keymap 'goto-map
 :transient t
 )
(bind-key "C-c f" #'foo-goto) 		; must match the arb name

(hercules-def
 :toggle-funs #'mbk-yank-hydra
 :keymap 'bookmark-map
 :transient t)
(bind-key "C-c g" #'mbk-yank-hydra)
(bind-key "y" #'mbk-yank-hydra 'ryo-modal-mode-map)

(hercules-def
 :toggle-funs #'mbk-ctlx-hydra
 :keymap 'ctl-x-map
 :transient t)
(bind-key "x" #'mbk-ctlx-hydra 'ryo-modal-mode-map)

(hercules-def
 :show-funs #'mbk-search-hydra
 ;; learning: we need to exit the hercules pop on invocation of functions like consult-ripgrep, occur etc
 ;; which in turn use the minibuffer to read user input. If hercules lingers, then it captures the user input,
 ;; which usually ends up into self-insert or triggering other commands from the key-map being displayed
 ;; :hide-funs '(consult-ripgrep occur)
 :hide-funs (-keep (lambda (x) (and (listp x ) (cdr x))) (cdr (-flatten search-map)))
 :keymap 'search-map
 :flatten t
 ;; :transient t
 )
(bind-key "s" #'mbk-search-hydra 'ryo-modal-mode-map)
