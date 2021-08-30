;; (defhydra hydra-emms (:color teal
;;                              :hint nil)
;;   "
;;     _p_:laylist  _b_:rowse  _r_:eset  _c_:onnect
;;     _k_:ill      _u_:pdate
;;   "
;;   ("q" nil "quit")
;;   ("p" emms)
;;   ("b" emms-smart-browse)
;;   ("r" emms-player-mpd-update-all-reset-cache)
;;   ("c" mpd/start-music-daemon)
;;   ("k" mpd/kill-music-daemon)
;;   ("u" mpd/update-database))

;; (global-set-key (kbd "s-m") 'hydra-emms/body)
;; (global-set-key (kbd "C-: m") 'hydra-emms/body)

;; (defun exwm-async-run (name)
;;   (start-process name nil name))

(defhydra hydra-programs (:color teal
				 :hint nil)
  "
  _a_:genda  _b_:ookmarks  _c_:alc      _d_:ictionary
  _i_:spell  _k_:eepass    _r_:ecentf   _s_:hell   
  "
  ("q" nil "quit")
  ;; ("B" (exwm-async-run "chromium"))
  ("b" hydra-bookmarks/body)
  ("d" hydra-dictionary/body)
  ("a" org-agenda)
  ("i" hydra-ispell/body)
  ;; ("e" elfeed)
  ;; ("E" eww-search-words)
  ;; ("p" pass)
  ("c" calc)
  ("k" unlock-keepass)
  ("r" counsel-recentf)
  ;; ("g" gnus)
  ;; ("D" debbugs-gnu)
  ("s" eshell)
  ;; ("w" webjump)
  ;; ("y" hydra-ytdl/body)
  )

(bind-key "C-c a" 'hydra-programs/body)

(defhydra hydra-dictionary (:color teal
				   :hint nil)
  ("q" nil "quit")
  ("l" dictionary-lookup-definition "lookup")
  ("s" dictionary-search "search")
  ("n" dictionary-new-search "new search")
  ("p" dictionary-previous "previous")
  ("c" dictionary-close "close"))

(defhydra hydra-ispell (:color teal
			       :hint nil)
  "
    _r_:egion  _c_:hange-dictionary
    "
  ("q" nil "quit")
  ("r" ispell-region)
  ("c" ispell-change-dictionary))

(defhydra hydra-multiple-cursors (:color pink
					 :hint nil
					 :post hydra-modal--call-body-conditionally)
  ("q" nil "quit")
  ("n" mc/mark-next-like-this "next" :column "Mark")
  ("p" mc/mark-previous-like-this "previous")
  ("N" mc/unmark-next-like-this "next" :column "Unmark")
  ("P" mc/unmark-previous-like-this "previous")
  ("r" mc/mark-all-like-this "like region" :column "All like this")
  ("R" mc/mark-all-in-region "in region")
  ("a" mc/edit-beginnings-of-lines "beginning" :column "Lines")
  ("e" mc/edit-ends-of-lines "end")
  ("i n" mc/insert-numbers "numbers" :column "Insert")
  ("i l" mc/insert-letters "letters")
  ("S s" mc/sort-regions "sort" :column "Sort")
  ("S r" mc/reverse-regions "reverse")
  ("s n" mc/skip-to-next-like-this "next" :column "Skip")
  ("s p" mc/skip-to-previous-like-this "previous"))

(global-set-key (kbd "C-c m") 'hydra-multiple-cursors/body)

(defhydra hydra-project (:color teal
				:hint nil)
  "
  _f_: find-file  _g_: regexp  _e_: eshell   _G_: interactive regexp
  _c_: compile    _d_: dired   _r_: replace  _&_: async shell
  _b_: buffers    _p_: projects            ^^_k_: kill buffer
  "
  ("q" nil "quit")
  ("f" project-find-file)
  ("g" project-find-regexp)
  ("b" project-switch-to-buffer)
  ("k" project-kill-buffers)
  ("G" project-search)
  ("r" project-query-replace-regexp)
  ("d" project-dired)
  ("e" project-eshell)
  ("c" project-compile)
  ("p" project-switch-project)
  ("&" project-async-shell-command))

(global-set-key (kbd "C-c P") 'hydra-project/body)

(defhydra hydra-avy (:color teal
			    :hint nil
			    :post hydra-modal--call-body-conditionally)
  ("q" nil "quit")
  (":" avy-goto-char-timer "timer" :column "Motion")
  (";" avy-goto-char-2 "char2")
  ("p" avy-goto-word-1-above "above")
  ("n" avy-goto-word-1-below "below")
  ("'" avy-goto-line "line")
  ("o" avy-pop-mark "pop")
  ("m '" avy-move-line "line" :column "Move")
  ("m r" avy-move-region "region")
  ("t" avy-transpose-lines-in-region "transpose")
  ("k r" avy-kill-region "region" :column "Kill")
  ("k '" avy-kill-whole-line "line"))

(global-set-key (kbd "C-;") 'hydra-avy/body)

(defhydra hydra-macros (:color teal
			       :hint nil)
  "
  _r_: region  _e_: execute   _c_: counter  _f_: format
  _n_: next    _p_: previous  _i_: insert   _q_: query
 _(_: start  _)_: stop
  "
  ("q" nil "quit")
  ("Q" kbd-macro-query)
  ("(" kmacro-start-macro-or-insert-counter)
  (")" kmacro-end-or-call-macro)
  ("r" apply-macro-to-region-lines)
  ("e" kmacro-end-and-call-macro)
  ("n" kmacro-cycle-ring-next)
  ("p" kmacro-cycle-ring-previous)
  ("i" kmacro-insert-counter)
  ("c" kmacro-set-counter)
  ("q" kbd-macro-query)
  ("f" kmacro-set-format))

(global-set-key (kbd "C-c M") 'hydra-macros/body)

;; (defhydra hydra-ytdl (:color teal
;;                              :hint nil)
;;   "
;;   _d_:ownload   _l_:ist  _o_:pen  _p_:laylist
;;   "
;;   ("q" nil "quit")
;;   ("d" ytdl-download)
;;   ("o" ytdl-download-open)
;;   ("l" ytdl-show-list)
;;   ("p" ytdl-download-playlist))

;; (defhydra hydra-dumb-jump (:color teal :columns 3)
;;   "Dumb Jump"
;;   ("q" nil "quit")
;;   ("j" dumb-jump-go "Go")
;;   ("o" dumb-jump-go-other-window "Other window")
;;   ("e" dumb-jump-go-prefer-external "Go external")
;;   ("i" dumb-jump-go-prompt "Prompt")
;;   ("l" dumb-jump-quick-look "Quick look")
;;   ("b" dumb-jump-back "Back")
;;   ("x" dumb-jump-go-prefer-external-other-window "Go external other window"))

;; (defhydra hydra-ide (:color teal
;; 			    :hint nil)
;;   ("q" nil "quit")
;;   ("l" hydra-lsp/body "lsp" :column "IDE features")
;;   ("d" hydra-dumb-jump/body "dumb-jump"))

;; (global-set-key (kbd "C-c i") 'hydra-ide/body)

;; (defhydra hydra-lsp (:exit t :hint nil)
;;   "
;;  Buffer^^               Server^^                   Symbol
;; -------------------------------------------------------------------------------------
;;  [_f_] format           [_M-r_] restart            [_d_] declaration  [_i_] implementation  [_o_] documentation
;;  [_m_] imenu            [_S_]   shutdown           [_D_] definition   [_t_] type            [_r_] rename
;;  [_x_] execute action   [_M-s_] describe session   [_R_] references   [_s_] signature"
;;   ("d" lsp-find-declaration)
;;   ("D" lsp-ui-peek-find-definitions)
;;   ("R" lsp-ui-peek-find-references)
;;   ("i" lsp-ui-peek-find-implementation)
;;   ("t" lsp-find-type-definition)
;;   ("s" lsp-signature-help)
;;   ("o" lsp-describe-thing-at-point)
;;   ("r" lsp-rename)

;;   ("f" lsp-format-buffer)
;;   ("m" lsp-ui-imenu)
;;   ("x" lsp-execute-code-action)

;;   ("M-s" lsp-describe-session)
;;   ("M-r" lsp-restart-workspace)
;;   ("S" lsp-shutdown-workspace))

;; (defhydra hydra-roam (:color teal
;;                              :hint nil)
;;   "
;;   _f_:ind file  _i_:nsert  _I_:ndex  _g_:raph
;;   _c_:apture  _s_:erver
;;   "
;;   ("q" nil "quit")
;;   ("f" org-roam-find-file)
;;   ("i" org-roam-insert)
;;   ("I" org-roam-jump-to-index)
;;   ("g" org-roam-graph)
;;   ("c" org-roam-capture)
;;   ("s" org-roam-server-mode))

;; (global-set-key (kbd "C-c r") 'hydra-roam/body)

(defhydra hydra-frames-windows (:color teal
				       :hint nil)
  "
  Frame commands:
  _m_: make-frame   _d_: delete-frame          _Z_: suspend-frame
  _q_: quit         _b_: buffer-other-frame    _M_: toggle-maximize
  _o_: other-frame  _f_: find-file-other-frame
  Window commands:
  _0_: delete-window     _1_: delete-other-window  _2_: split below
  _3_: split right       _\\^_: enlarge vertical     _-_: shrink vertical
  _{_: shrink horizontal _}_: enlarge horizontal   _+_: balance-windows
  _a_: ace-window        _t_: toggle-window-split  _O_: other-window
  _k_/_j_/_h_/_l_: windmove-up/down/left/right
  _M-k_/_M-j_/_M-h_/_M-l_: buf-move-up/down/left/right
  Tab hydra: _T_"
  ;; Frame commands
  ("m" make-frame-command)
  ("b" switch-to-buffer-other-frame)
  ("d" delete-frame)
  ("o" other-frame)
  ("f" find-file-other-frame)
  ("Z" suspend-frame)
  ("M" toggle-frame-maximized)
  ;; Window commands
  ("0" delete-window)
  ("1" delete-other-windows)
  ("2" split-window-below)
  ("3" split-window-right)
  ("^" enlarge-window :color pink)
  ("-" shrink-window :color pink)
  ("}" enlarge-window-horizontally :color pink)
  ("{" shrink-window-horizontally :color pink)
  ("+" balance-windows)
  ("t" toggle-window-split)
  ("a" ace-window)
  ("O" other-window)
  ("k" windmove-up)
  ("j" windmove-down)
  ("h" windmove-left)
  ("l" windmove-right)
  ("M-k" buf-move-up)
  ("M-j" buf-move-down)
  ("M-h" buf-move-left)
  ("M-l" buf-move-right)
  ;; Tab hydra
  ("T" hydra-tab/body)
  ("q" nil))

(global-set-key (kbd "C-z") 'hydra-frames-windows/body)

(defhydra hydra-tab (:color teal
			    :hint nil)
  "
  tab-bar commands:
  _2_: tab-new  _<tab>_: tab-next      _C-<tab>_: tab-previous  _b_: switch-to-buffer-other-tab
  _u_: tab-undo  _d_: dired-other-tab  _r_: tab-rename         _<RET>_: tab-bar-select-tab-by-name
  "
  ("q" nil "quit")
  ("2" tab-new)
  ("r" tab-rename)
  ("b" switch-to-buffer-other-tab)
  ("f" find-file-other-tab)
  ("<RET>" tab-bar-select-tab-by-name)
  ("C-<tab>" tab-previous)
  ("<tab>" tab-next)
  ("u" tab-undo)
  ("d" dired-other-tab))

;; (use-package ledger-mode
;;   :straight t
;;   :mode "\\.dat\\'"
;;   :config
;;   (setq ledger-reports
;;         '(("bal" "gpg2 --decrypt %(ledger-file) 2>/dev/null | %(binary) -f - bal")
;;           ("reg" "gpg2 --decrypt %(ledger-file) 2>/dev/null | %(binary) -f - reg")
;;           ("payee" "gpg2 --decrypt %(ledger-file) 2>/dev/null | %(binary) -f - reg @%(payee)")
;;           ("account" "gpg2 --decrypt %(ledger-file) 2>/dev/null | %(binary) -f - reg %(account)"))))

;; (defhydra hydra-bookmarks (:color teal
;;                                   :hint nil)
;;   ("m" bookmark-set "set")
;;   ("b" bookmark-jump "jump")
;;   ("l" list-bookmarks "list")
;;   ("s" bookmark-save "save")
;;   ("q" nil "quit"))


