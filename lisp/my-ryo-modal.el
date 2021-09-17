;;         -*- lexical-binding: t; -*-
;; use M-x ryo-modal-bindings to view your bindings

;; form structure
;; (ryo-modal-keys list-of-keypairs), list-of-keypairs is (key target [keyword args]), target is unquotted command
;; (ryo-modal-key key target [keyword args], target is quoted
;; keywords :: :name, :mode, :exit, :read, :then, :first, :norepeat, :mc-all

;;  :ryo is a keyword supported for use-package

;; which-key integration
;; If you’re using which-key you might be annoyed that ryo prefixes some commands with ryo:<hash>:. In order to remove that from the which-key menus, add this to your init-file
(push '((nil . "ryo:.*:") . (nil . "")) which-key-replacement-alist)

(use-package ryo-modal
  :commands ryo-modal-mode
  :bind ("M-g" . ryo-modal-mode)		; hijack M-g to enter/exit ryo-modal-mode
  ;; norepeat is not set. so all commands will be remembered and will respond to '.'
  )
(defun mbk-insert-semicolon () (interactive) (insert-char ?\;))
(ryo-modal-keys
 ("." ryo-modal-repeat)	   ; '.' is more intuitive than ',' for repeat
 ("t" backward-char)
 ("n" next-line)
 ("g" previous-line)
 ("h" forward-char)
 ;; ("C-;" mbk-insert-semicolon :exit t)
 )
(define-key ryo-modal-mode-map "c" goto-map ) ; access gotomap using M-g c

(ryo-modal-keys
 ;; First argument to ryo-modal-keys may be a list of keywords.
 ;; These keywords will be applied to all keybindings.
 (:norepeat t)
 ("0" "M-0")
 ("1" "M-1")
 ("2" "M-2")
 ("3" "M-3")
 ("4" "M-4")
 ("5" "M-5")
 ("6" "M-6")
 ("7" "M-7")
 ("8" "M-8")
 ("9" "M-9"))

;; example of bindings specific to a major mode
(if disable-this-snippet nil
  (ryo-modal-major-mode-keys
   'python-mode
   ("G" python-nav-forward-defun)
   ("H" python-nav-backward-defun))
  )

;; SPC, 'v', 'k' and 'c' become prefix keys in ryo-modal-mode
;; spc does not exit.
;; c will exit ryo-modal before executing the command
;; (ryo-modal-key
;;  "SPC" '(("s" save-buffer)
;;          ("g" magit-status)
;;          ("b" ibuffer-list-buffers)))
;; notice that mark-word, mark-defun etc. have been repeated in the config below
(if disable-this-snippet nil
  (ryo-modal-keys
   ("v"
    (("w" er/mark-word :name "Mark word")
     ("d" er/mark-defun :name "Mark defun")
     ("s" er/mark-sentence :name "Mark sentence"))
    :name "mbk-mark"	       ; friendly name to display in which-key
    )
   ("k"
    (("w" er/mark-word :name "Kill word")
     ("d" er/mark-defun :name "Kill defun")
     ("s" er/mark-sentence :name "Kill sentence"))
    :name "mbk-kill" :then '(kill-region))
   ("c"
    (("w" er/mark-word :name "Change word")
     ("d" er/mark-defun :name "Change defun")
     ("s" er/mark-sentence :name "Change sentence"))
    :name "mbk-change" :then '(kill-region) :exit t)))

;; an alternate way to reduce redundant typing is to use macro expansion
;; text-objects are now defined and reused
(if disable-this-snippet nil
  (let ((text-objects
	 '(("w" er/mark-word :name "Word")
	   ("d" er/mark-defun :name "Defun")
	   ("s" er/mark-sentence :name "Sentence"))))
    (eval `(ryo-modal-keys
	    ("v" ,text-objects)
	    ("k" ,text-objects :name "mbk-kill" :then '(kill-region))
	    ("c" ,text-objects :name "mbk-change" :then '(kill-region) :exit t))))
  )

;; creating an hydra and binding to a key sequence in ryo-modal state
(ryo-modal-key
 "SPC g" :hydra
 '(hydra-git ()
             "A hydra for git!"
             ("j" git-gutter:next-hunk "next")
             ("k" git-gutter:previous-hunk "previous")
             ("d" git-gutter:popup-hunk "diff")
             ("s" git-gutter:stage-hunk "stage")
             ("r" git-gutter:revert-hunk "revert")
             ("m" git-gutter:mark-hunk "mark")
             ("q" nil "cancel" :color blue)))
;; adding to an existing hydra (hydra-git in this case)
(ryo-modal-key
 "SPC g" :hydra+
 '(hydra-git ()
             "A hydra for git!"
             ("g" magit-status "magit" :color blue)))




;; this works as intended. But does not show up in the hydra of SPC-m 
(if disable-this-snippet nil
  (bind-key "g" goto-map my-point-motion-hydra/keymap))

(ryo-modal-key
 "SPC m" :hydra
 '(my-point-motion-hydra
   () "A hydra for point motion"
   ("t" backward-char)
   ("h" next-line)
   ("g" previous-line)
   ("n" forward-char)
   ("T" backward-word)
   ("N" forward-word)
   ;; ("g" "M-g" :name "goto map")
   ))

(if disable-this-snippet nil
  (ryo-modal-keys (:norepeat t)
		  (":" "C-c" :name "C-c maps"))
  )



