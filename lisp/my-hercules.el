;;* My hercules definitions          -*- lexical-binding: t; -*-

(require 'dash)
(hercules-def
 ;; read further to see why this works
 :toggle-funs #'org-babel-mode
 :keymap 'org-babel-map
 :transient t)
;; tweak binding to taste
;; (define-key org-mode-map (kbd "C-c t") #'org-babel-mode)

;; (hercules-def
;;  :toggle-funs #'any-name
;;  :keymap 'org-babel-map  ; choose the map you want to reuse
;;  :whitelist-keys '("n" "p" "t")
;; :transient t) ; any non-key map key will quit the hydra
;; (bind-key "t" nil 'org-babel-map) ; t will quit the hydra
;; (bind-key "C-c e" #'any-name)

;; learning and documentation
;; pressing any key outside the keymap will hide away the hercules popup if TRANSIENT is t
;; within the commands mapped by the keymap, those that are in the HIDE-FUNS list will hide the popup and then execute the target command. this is most useful for commands that read arguments from minibuffer
;; those that are in the SHOW-FUNS list will cause the popup to show
;; those commands that are not specified in hide-funs list, will execute and keep the popup so that we can deliver same or other commands from that keymap
;; FLATTEN will flatten the keymap recursively thru its sub keymaps

(hercules-def
 :show-funs #'my-search-map-nav
 ;; almost all funs in the search-map are user interactive (taking user input from minibuffer to get their args) functions.
 ;; So we progamatically flatten the keymap and extract the target commands which sit at the cdr cell of each list-item of the keymap
 :hide-funs (-keep (lambda (x) (and (listp x ) (cdr x))) (cdr (-flatten search-map)))
 :keymap 'search-map
 :flatten t
 :transient t
 )
(bind-key "s" #'my-search-map-nav 'ryo-modal-mode-map)

(hercules-def
 :show-funs #'my-goto-map-nav
 :hide-funs '(goto-char consult-goto-line move-to-column consult-outline)
 :keymap 'goto-map
 :transient t)
(bind-key "c" #'my-goto-map-nav 'ryo-modal-mode-map)

;; create our own map for outshine and outline minor modes
(setq my-outline-nav-map (make-sparse-keymap))
(bind-keys :map my-outline-nav-map
	   ("S" . outline-show-all)
	   ("u" . outline-up-heading)
	   ("f" . outline-forward-same-level)
	   ("b" . outline-backward-same-level)
	   ("n" . outline-next-visible-heading)
	   ("p" . outline-previous-visible-heading)
	   ("RET" . outline-insert-heading)
	   ("q" . nil))
(hercules-def
 :show-funs #'my-outline-nav
 :hide-funs '(outline-insert-heading)
 :keymap 'my-outline-nav-map
 :flatten t
 :transient t
 )
(bind-key "f" #'my-outline-nav 'ryo-modal-mode-map)
;; (define-key ryo-modal-mode-map "f" my-outline-nav-map)
