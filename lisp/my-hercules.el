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
