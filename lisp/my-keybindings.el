;; * personal configuration       -*- lexical-binding: t; -*-
;;
;; * Commentary
;; Pressing C-h when having pressed a prefix-vector of keys (eg C-x or C-x-4) invokes prefix-command-help
;; which can be customized to something like embark-prefix-help-command, even whem which-key is active
;; But with hercules, the which-key interface is displayed as a consequence of a *full* keybinding sequence eg `C-x r t' or in our case `C-c p h'
;; thus prefix-command-help deos not come into the picture even though which-key is popup is being displayed as if we have typed only a prefix
;; unfortunately, for now, there is no way to filter or search in the hercules-which-key-popup

;; | prefix | category     | comments                                           |
;; | C-a    | apps         | eshell, calc, dired, grep, compile, ielm, keepass           |
;; | C-b    | buffer       | save, revert                                       |
;; | C-p    | point motion | directional mv, avy, scroll,                       |
;; | C-w    | window       |                                                    |
;; | C-f    | folding      |                                                    |
;; | C-e    | edit         | case convert, transpose, rectangle, cut/copy/paste, outorg |
;; |        |              |                                                    |

;; * Code

;; one method of creating a keymap explicitly
;; (defvar which-key-C-h-map
;;   (let ((map (make-sparse-keymap)))
;;     (dolist (bind `(("\C-a" . which-key-abort)
;;                     ("a" . which-key-abort)
;;                     ("\C-d" . which-key-toggle-docstrings)
;;                     ("d" . which-key-toggle-docstrings)
;;                     (,(vector help-char) . which-key-show-standard-help)
;;                     ("h" . which-key-show-standard-help)
;;                     ("\C-n" . which-key-show-next-page-cycle)
;;                     ("n" . which-key-show-next-page-cycle)
;;                     ("\C-p" . which-key-show-previous-page-cycle)
;;                     ("p" . which-key-show-previous-page-cycle)
;;                     ("\C-u" . which-key-undo-key)
;;                     ("u" . which-key-undo-key)
;;                     ))
;;       (define-key map (car bind) (cdr bind)))
;;     map)
;;   "Keymap for C-h commands.")

;; ** my keymaps
;; *** Apps
(define-prefix-command 'my--apps-map)
(bind-keys
 :map 'my--apps-map 
 ("e" . eshell)
 ("c" . calc)
 ("d" . dired)
 ("e" . consult-complex-command)
 ("E" . lispy-eval-expression)
 ("g" . magit-status)
 ("k" . unlock-keepass)
 ("i" . ielm))
;; *** Buff ops
(define-prefix-command 'my--buffops-map)
(bind-keys
 :map 'my--buffops-map 
 ("w" . ace-window)
 ("b" . consult-buffer) 		; frees up C-x b
 ("B" . consult-buffer-other-window) 	; frees up C-x 4 b
 ("o" . display-buffer)                 ; frees uo C-x 4 C-o
 ("r" . revert-buffer)
 ("v" . toggle-read-only)
 ("s" . save-some-buffers))
;; *** edit ops on existing content
(define-prefix-command 'my--edit-map)
(bind-keys
 :map 'my--edit-map 
 ("u" . upcase-word)
 ("c" . kill-ring-save)
 ("C" . capitalize-word)
 ("l" . downcase-word)
 ("t" . transpose-chars)
 ("T" . transpose-words)
 ("v" . yank)
 )
;; *** point motion
(define-prefix-command 'my--pointmotion-map)
(bind-keys
 :map 'my--pointmotion-map 
 ("a" . crux-move-beginning-of-line)
 ("A" . move-beginning-of-line)
 ("h" . next-line)
 ("H" . forward-paragraph)
 ("g" . previous-line)
 ("G" . backward-paragraph)
 ("t" . backward-char)
 ("T" . backward-sentence)
 ("n" . forward-char)
 ("N" . forward-sentence)
 ("e" . end-of-line)
 ("z" . beginning-of-buffer)
 ("l" . avy-goto-line)
 ("m" . pop-to-mark-command)
 ("Z" . end-of-buffer)
 ("f" . avy-goto-word-1)
 ("w" . forward-to-word)
 ("b" . backward-word)
 )
(cl-loop for (key . map) in
 '(("o" . goto-map)
   ("s" . search-map))
 do (bind-key key map 'my--pointmotion-map)
 )
;; (bind-key "o" goto-map 'my--pointmotion-map)
;; (bind-key "s" search-map 'my--pointmotion-map)
;; *** my outline map
;; inherited from outline-mode-prefix-map with C-c mapped to c and M-c mapped to C
(define-prefix-command 'my--ol-map)
(with-eval-after-load 'outshine
  (set-keymap-parent my--ol-map outline-mode-prefix-map)
  ;; for a-z bind a to what C-a binds and A to what M-a binds
  (-each (number-sequence ?a ?z)
    (lambda (x)
      (bind-key (format "%c" x)
		(lookup-key my--ol-map (kbd (format "C-%c" x))) my--ol-map nil)
      (bind-key (format "%c" (upcase x))
		(lookup-key my--ol-map (kbd (format "M-%c" x))) my--ol-map nil)))
  (bind-key "#" (lookup-key my--ol-map (kbd "M-#")) my--ol-map nil)
  (bind-key "+" (lookup-key my--ol-map (kbd "M-+")) my--ol-map nil))
;; ** define some hercules hydras
(hercules-def
 :toggle-funs #'my--ol-herc
 :keymap 'my--ol-map
 ;; blacklist-keys '("%" "C-a" "C-b" "C-c" "C-d") does not work because
 ;; these keys are define in outline-mode-prefix-map (which is inherited by my--ol-prefix-map)
 ;; hercules looks only at keys defined in my--ol-prefix-map

 ;; :flatten t
 :transient t)

;; my--pointmotion-map is created in my-keybindings.el
(hercules-def
 :show-funs #'my--pointmotion-herc
 :hide-funs '(avy-goto-word-1 isearch-forward-regexp consult-goto-line)
 :keymap 'my--pointmotion-map
 :transient t)
;; ** establish bindings to my maps and hercules funs into global map under C-c char
(bind-keys ("C-c a" . my--apps-map)
	   ("C-c b" . my--buffops-map)
	   ("C-c p" . my--pointmotion-herc)
	   ("C-c o" . my--ol-herc))

(provide 'my-keybindings)
