;; personal keybindings       -*- lexical-binding: t; -*-
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

;; ** my keymaps
;; *** Apps
(let () 				; populate my--apps-map
  (define-prefix-command 'my--apps-map)
  (bind-keys
   :map 'my--apps-map 
   ("s" . eshell)
   ("c" . calc-dispatch)
   ("r" . calendar)
   ("d" . dired)
   ("e" . consult-complex-command)
   ("E" . lispy-eval-expression)
   ("g" . magit-status)
   ("k" . unlock-keepass)
   ("i" . ielm)))
;; *** Buff ops
(let () 				; populate my--buffops-map
  (define-prefix-command 'my--buffops-map)
  (bind-keys
   :map 'my--buffops-map 
   ("w" . ace-window)
   ("b" . consult-buffer)		  ; frees up C-x b
   ("B" . consult-buffer-other-window)	  ; frees up C-x 4 b
   ("o" . display-buffer)		  ; frees uo C-x 4 C-o
   ("f" . find-file)
   ("r" . revert-buffer)
   ("v" . toggle-read-only)
   ("s" . save-some-buffers)))
;; *** edit ops on existing content
(let () 				; populate my--edit-map
  (define-prefix-command 'my--edit-map)
  (bind-keys				
   :map 'my--edit-map
   ("SPC" . just-one-space) 		; frees up M-SPC
   ("u" . upcase-word)
   ("c" . kill-ring-save)
   ("C" . capitalize-word)
   ("l" . downcase-word)
   ("t" . transpose-chars)
   ("T" . transpose-words)
   ("v" . yank)
   ))
;; *** point motion
(unless (fboundp 'goto-map) (fset 'goto-map goto-map))
(unless (fboundp 'SEARCH-map) (fset 'search-map search-map))
(let () 				; populate my--pointmotion-map
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
   ("o" . goto-map)
   ("s" . search-map)
   ("<" . backward-list)
   (">" . forward-list)
   ))

;; keymap learning!!
;; you can define any function as a keymap which exists at any prefix key (we need not know the name of the keymap)
;; eg. (fset 'my--foo-command (lookup-key outline-mode-map (kbd "C-c C-")))
;; then my--foo-command is same as an interactive command, and you could bind any key to it, which will make that key as a prefix

;; *** my outline map
;; inherited from outline-mode-prefix-map with C-c mapped to c and M-c mapped to C
(define-prefix-command 'my--ol-map)  	; this sets function cell of symbol my--ol-map to a sparse keymap
(define-prefix-command 'my--hs-map)   	; map for hideshow
(with-eval-after-load 'outshine
  ;; note: outline-mode-prefix-map eq (lookup-key outline-mode-map (kbd "C-c"))
  (set-keymap-parent my--ol-map outline-mode-prefix-map)
  ;; for a-z bind a to what C-a binds and A to what M-a binds
  (-each (number-sequence ?a ?z)
    (lambda (x)
      (bind-key (format "%c" x)
		(lookup-key my--ol-map (kbd (format "C-%c" x))) my--ol-map nil)
      (bind-key (format "%c" (upcase x))
		(lookup-key my--ol-map (kbd (format "M-%c" x))) my--ol-map nil)))
  (bind-key "#" (lookup-key my--ol-map (kbd "M-#")) my--ol-map nil)
  (bind-key "+" (lookup-key my--ol-map (kbd "M-+")) my--ol-map nil)
  (bind-key "h" my--hs-map my--ol-map nil))

(with-eval-after-load 'hideshow
  (bind-keys
   :map 'my--hs-map
   ("h" . hs-hide-block)
   ("s" . hs-show-block)
   ("t" . hs-hide-all)
   ("a" . hs-show-all)
   ("l" . hs-hide-level) ; num prefix N(=1) to hide everything below level N
   ("e" . hs-toggle-hiding))
  )

;; *** mode or other toggling
(let ()
  (define-prefix-command 'my--toggles-map)
  (bind-keys
   :map 'my--toggles-map
   ("h" . hs-minor-mode)
   ("H" . hs-org/minor-mode)
   ("o" . outshine-mode)
   ("l" . outline-minor-mode)
   ))
;; ** define some hercules hydras
(hercules-def  				; my--ol-herc
 :toggle-funs #'my--ol-herc
 :keymap 'my--ol-map
 ;; blacklist-keys '("%" "C-a" "C-b" "C-c" "C-d") does not work because
 ;; these keys are define in outline-mode-prefix-map (which is inherited by my--ol-map)
 ;; hercules looks only at keys defined in my--ol-map

 ;; :flatten t
 :transient t)

(hercules-def 				; my--pointmotion-herc
 :show-funs #'my--pointmotion-herc
 :hide-funs '(avy-goto-word-1 isearch-forward-regexp consult-goto-line)
 :keymap 'my--pointmotion-map
 :transient t)
;; ** establish bindings to my maps and hercules funs into global map under C-c char
(bind-keys
 ("M-SPC" . hippie-expand)
 ("C-c a" . my--apps-map)
 ("C-c b" . my--buffops-map)
 ("C-c e" . my--edit-map)
 ("C-c p" . my--pointmotion-herc)
 ("C-c o" . my--ol-herc) ; o is for hercules folding motion... thus multiple motions until quit (C-g)
 ("C-c f" . my--ol-map)	 ; f is for single folding motion
 ("C-c t" . my--toggles-map))

(provide 'my-keybindings)
