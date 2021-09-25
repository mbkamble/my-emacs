;; Emacs initialization, customization from scratch        -*- lexical-binding: t; -*-
;; Copyright (C) 2021 by Milind Kamble

;; set this to enter debugger when we want to debug errors thrown by condition-case-unless-debug
;; form, which does not stop unless debug-on-error is 
(setq debug-on-error t)

(setq disable-this-snippet t) 		; bypass experimental code

;; done in early-init for emacs 27+
(when (< emacs-major-version 27)
  (setq gc-cons-threshold most-positive-fixnum)
  (setq gc-cons-percentage 0.6))
(add-to-list 'load-path (concat user-emacs-directory "lisp"))

;;** Essentials from radian. configure key packages: straight, use-package, org, el-patch
;;*** Reuse a couple of macros from radian
(defmacro radian-defadvice (name arglist where place docstring &rest body)
  "Define an advice called NAME and add it to a function.
ARGLIST is as in `defun'. WHERE is a keyword as passed to
`advice-add', and PLACE is the function to which to add the
advice, like in `advice-add'. PLACE should be sharp-quoted.
DOCSTRING and BODY are as in `defun'."
  (declare (indent 2)
           (doc-string 5))
  (unless (stringp docstring)
    (error "Radian: advice `%S' not documented'" name))
  (unless (and (listp place)
               (= 2 (length place))
               (eq (nth 0 place) 'function)
               (symbolp (nth 1 place)))
    (error "Radian: advice `%S' does not sharp-quote place `%S'" name place))
  `(progn
     ;; You'd think I would put an `eval-and-compile' around this. It
     ;; turns out that doing so breaks the ability of
     ;; `elisp-completion-at-point' to complete on function arguments
     ;; to the advice. I know, right? Apparently this is because the
     ;; code that gets the list of lexically bound symbols at point
     ;; tries to `macroexpand-all', and apparently macroexpanding
     ;; `eval-and-compile' goes ahead and evals the thing and returns
     ;; only the function symbol. No good. But the compiler does still
     ;; want to know the function is defined (this is a Gilardi
     ;; scenario), so we pacify it by `eval-when-compile'ing something
     ;; similar (see below).
     (defun ,name ,arglist
       ,(let ((article (if (string-match-p "^:[aeiou]" (symbol-name where))
                           "an"
                         "a")))
          (format "%s\n\nThis is %s `%S' advice for `%S'."
                  docstring article where
                  (if (and (listp place)
                           (memq (car place) ''function))
                      (cadr place)
                    place)))
       ,@body)
     (eval-when-compile
       (declare-function ,name nil))
     (advice-add ,place ',where #',name)
     ',name))

(defmacro radian-defhook (name arglist hooks docstring &rest body)
  "Define a function called NAME and add it to a hook.
ARGLIST is as in `defun'. HOOKS is a list of hooks to which to
add the function, or just a single hook. DOCSTRING and BODY are
as in `defun'."
  (declare (indent 2)
           (doc-string 4))
  (unless (listp hooks)
    (setq hooks (list hooks)))
  (dolist (hook hooks)
    (unless (string-match-p "-\\(hook\\|functions\\)$" (symbol-name hook))
      (error "Symbol `%S' is not a hook" hook)))
  (unless (stringp docstring)
    (error "Radian: no docstring provided for `radian-defhook'"))
  (let ((hooks-str (format "`%S'" (car hooks))))
    (dolist (hook (cdr hooks))
      (setq hooks-str (format "%s\nand `%S'" hooks-str hook)))
    `(progn
       (defun ,name ,arglist
         ,(format "%s\n\nThis function is for use in %s."
                  docstring hooks-str)
         ,@body)
       (dolist (hook ',hooks)
         (add-hook hook ',name)))))

;;*** bootstrap 'straight'
(setq straight-check-for-modifications '(check-on-save)
      straight-repository-branch "develop"
      straight-vc-git-default-clone-depth 1)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;*** use-package

;; Package `use-package' provides a handy macro by the same name which
;; is essentially a wrapper around `with-eval-after-load' with a lot
;; of handy syntactic sugar and useful features.
(straight-use-package 'use-package)
(setq use-package-compute-statistics t)

;; When configuring a feature with `use-package', also tell
;; straight.el to install a package of the same name, unless otherwise
;; specified using the `:straight' keyword.
(setq straight-use-package-by-default t)

;; Tell `use-package' to always load features lazily unless told
;; otherwise. It's nicer to have this kind of thing be deterministic:
;; if `:demand' is present, the loading is eager; otherwise, the
;; loading is lazy. See
;; https://github.com/jwiegley/use-package#notes-about-lazy-loading.
(setq use-package-always-defer t)

(defmacro use-feature (name &rest args)
  "Like `use-package', but with `straight-use-package-by-default' disabled.
NAME and ARGS are as in `use-package'."
  (declare (indent defun))
  `(use-package ,name
     :straight nil
     ,@args))

;;*** not working  `esup' - emacs startup profiler
;; (use-package esup
;;   :ensure t
;;   :custom
;;   (esup-depth 0)
;;   :config
;;   (esup-child-max-depth 0))

;;*** `all-the-icons' - multipurpose eye candy glyphs
;; 
;; all-the-icons-alltheicon (
;; all-the-icons-faicon (https://fontawesome.com/v4.7/cheatsheet/, https://fontawesome.com/v5.15/icons?d=gallery&p=2) +1600 icons
;; all-the-icons-fileicon (https://github.com/file-icons/icons/blob/master/charmap.md) +900 icons
;; all-the-icons-octicon  (https://primer.style/octicons/) github icons
;; all-the-icons-wicon (https://erikflowers.github.io/weather-icons/) various weather icons
;; google material icons (https://material.io/icons/, or https://fonts.google.com/icons)
;; https://github.com/file-icons/DevOpicons/blob/master/charmap.md
(use-package all-the-icons
  :demand t)

(use-package use-package-ensure-system-package)

;;*** prevent Emacs provided orgmode from being loaded
;; Our real configuration for Org comes much later. Doing this now
;; means that if any packages that are installed in the meantime
;; depend on Org, they will not accidentally cause the Emacs-provided
;; (outdated and duplicated) version of Org to be loaded before the
;; real one is registered.

(straight-register-package 'org)
(straight-register-package 'org-contrib)


(setq my-enable-org-contrib
      nil)
(if my-enable-org-contrib
    (straight-use-package 'org-contrib)
  (straight-use-package 'org))



;;*** install/enable the el-patch package (cf https://github.com/raxod502/el-patch)
;; this is used to patch functions (and maybe macros) with the ability to detect if/when the original changes
(use-package el-patch
  :demand t)

(use-package map)

(use-package blackout)


;;** Version control
;;*** disable native VC backends since we will use magit 
;; Feature `vc-hooks' provides hooks for the Emacs VC package. We
;; don't use VC, because Magit is superior in pretty much every way.
(use-feature vc-hooks
  :config

  ;; Disable VC. This improves performance and disables some annoying
  ;; warning messages and prompts, especially regarding symlinks. See
  ;; https://stackoverflow.com/a/6190338/3538165.
  (setq vc-handled-backends nil))

;;*** visualize and resolve git merge conflicts using feature `smerge-mode'
(use-feature smerge-mode
  :blackout t)

;;*** use emacs as external editor through package `with-editor'
(use-package with-editor
  )

;;*** magit requires package `transient' to display popups.
(use-package transient
  :demand t
  :config

  ;; Allow using `q' to quit out of popups, in addition to `C-g'. See
  ;; <https://magit.vc/manual/transient.html#Why-does-q-not-quit-popups-anymore_003f>
  ;; for discussion.
  (transient-bind-q-to-quit))

;;*** configure package `magit'
(use-package magit
  :bind (;; This is the primary entry point for Magit. Binding to C-x
         ;; g is recommended in the manual:
         ;; https://magit.vc/manual/magit.html#Getting-Started
         ("C-x g" . #'magit-status)
         ;; Alternate transient entry point; binding recommended in
         ;; <https://magit.vc/manual/magit.html#Transient-Commands>.
         ("C-x M-g" . #'magit-dispatch)
         ;; Completing the trio of bindings in `magit-file-mode-map'.
         ("C-c M-g" . #'magit-file-dispatch))

  :init

  ;; Suppress the message we get about "Turning on
  ;; magit-auto-revert-mode" when loading Magit.
  (setq magit-no-message '("Turning on magit-auto-revert-mode..."))

  :config/el-patch

  ;; Prevent Emacs asking if we're sure we want to exit, if a
  ;; Magit-spawned git-credential-cache process is running.
  (defun magit-maybe-start-credential-cache-daemon ()
    "Maybe start a `git-credential-cache--daemon' process.

If such a process is already running or if the value of option
`magit-credential-cache-daemon-socket' is nil, then do nothing.
Otherwise start the process passing the value of that options
as argument."
    (unless (or (not magit-credential-cache-daemon-socket)
                (process-live-p magit-credential-cache-daemon-process)
                (memq magit-credential-cache-daemon-process
                      (list-system-processes)))
      (setq magit-credential-cache-daemon-process
            (or (--first (let* ((attr (process-attributes it))
                                (comm (cdr (assq 'comm attr)))
                                (user (cdr (assq 'user attr))))
                           (and (string= comm "git-credential-cache--daemon")
                                (string= user user-login-name)))
                         (list-system-processes))
                (condition-case nil
                    (el-patch-wrap 2
                      (with-current-buffer
                          (get-buffer-create " *git-credential-cache--daemon*")
                        (start-process "git-credential-cache--daemon"
                                       (el-patch-swap
                                         " *git-credential-cache--daemon*"
                                         (current-buffer))
                                       magit-git-executable
                                       "credential-cache--daemon"
                                       magit-credential-cache-daemon-socket)
                        (el-patch-add
                          (set-process-query-on-exit-flag
                           (get-buffer-process (current-buffer)) nil))))
                  ;; Some Git implementations (e.g. Windows) won't have
                  ;; this program; if we fail the first time, stop trying.
                  ((debug error)
                   (remove-hook
                    'magit-credential-hook
                    #'magit-maybe-start-credential-cache-daemon)))))))

  :config

  ;; The default location for git-credential-cache is in
  ;; ~/.config/git/credential. However, if ~/.git-credential-cache/
  ;; exists, then it is used instead. Magit seems to be hardcoded to
  ;; use the latter, so here we override it to have more correct
  ;; behavior.
  (unless (file-exists-p "~/.git-credential-cache/")
    (let* ((xdg-config-home (or (getenv "XDG_CONFIG_HOME")
                                (expand-file-name "~/.config/")))
           (socket (expand-file-name "git/credential/socket" xdg-config-home)))
      (setq magit-credential-cache-daemon-socket socket)))

  ;; Don't try to save unsaved buffers when using Magit. We know
  ;; perfectly well that we need to save our buffers if we want Magit
  ;; to see them.
  (setq magit-save-repository-buffers nil)

  (transient-append-suffix
    'magit-merge "-n"
    '("-u" "Allow unrelated" "--allow-unrelated-histories"))

  (transient-append-suffix 'magit-pull "-r"
    '("-a" "Autostash" "--autostash"))

  (transient-append-suffix 'magit-fetch "-t"
    '("-u" "Unshallow" "--unshallow")))

;;*** Feature `magit-diff' from package `magit' handles interactive Git diffs.
(use-feature magit-diff
  :config

  (radian-defadvice radian--magit-diff-revert-before-smerge (buf _pos)
		    :before #'magit-diff-visit-file--setup
		    "Before calling `smerge-start-session', try to revert buffer.
This is necessary because it's possible that the file being
visited has changed on disk (due to merge conflict, for example)
but it was already visited, and hasn't been autoreverted yet
(because it hasn't been visible in a window, for example). But
`smerge-start-session', which is called by Magit while jumping
you to the file, will not wait for an autorevert. It will just
see that there aren't any conflict markers in the file and
disable itself. Sad."
		    (with-current-buffer buf
		      (auto-revert-handler))))

;;*** commit message editing capabilities through feature `git-commit' from package `magit'
(use-feature git-commit
  :config

  ;; Max length for commit message summary is 50 characters as per
  ;; https://chris.beams.io/posts/git-commit/.
  (setq git-commit-summary-max-length 50))

;; Package `emacsql-sqlite' is a dependency of Forge which is used to
;; interact with the SQLite database that Forge uses to keep track of
;; information about pull requests.
;; (use-feature emacsql-sqlite
;;   :init

;;   ;; Put the EmacSQL binary in the repository, not the build dir. That
;;   ;; way we don't have to recompile it every time packages get rebuilt
;;   ;; by straight.el. See
;;   ;; <https://github.com/raxod502/straight.el/issues/274> for not
;;   ;; having to use the internal function `straight--dir'.
;;   (setq emacsql-sqlite-data-root (straight--repos-dir "emacsql"))

;;   :config

;;   (radian-defadvice radian--advice-emacsql-no-compile-during-compile
;;       (&rest _)
;;     :before-until #'emacsql-sqlite-ensure-binary
;;     "Prevent EmacSQL from trying to compile stuff during byte-compilation.
;; This is a problem because Forge tries to get EmacSQL to compile
;; its binary at load time, which is bad (you should never do
;; anything significant at package load time) since it breaks CI."
;;     byte-compile-current-file))

;; Package `forge' provides a GitHub/GitLab/etc. interface directly
;; within Magit.
;; (use-package forge)

;; Feature `forge-core' from package `forge' implements the core
;; functionality.
;; (use-feature forge-core
;;   :config

;;   (radian-defadvice radian--forge-get-repository-lazily (&rest _)
;;     :before-while #'forge-get-repository
;;     "Make `forge-get-repository' return nil if the binary isn't built yet.
;; This prevents having EmacSQL try to build its binary (which may
;; be annoying, inconvenient, or impossible depending on the
;; situation) just because you tried to do literally anything with
;; Magit."
;;     (file-executable-p emacsql-sqlite-executable))

;;   (radian-defadvice radian--forge-build-binary-lazily (&rest _)
;;     :before #'forge-dispatch
;;     "Make `forge-dispatch' build the binary if necessary.
;; Normally, the binary gets built as soon as Forge is loaded, which
;; is terrible UX. We disable that above, so we now have to manually
;; make sure it does get built when we actually issue a Forge
;; command."
;;     (unless (file-executable-p emacsql-sqlite-executable)
;;       (emacsql-sqlite-compile 2))))

;; Package `git-gutter' adds a column to the left-hand side of each
;; window, showing which lines have been added, removed, or modified
;; since the last Git commit.
;; (use-package git-gutter
;;   :commands (git-gutter:previous-hunk
;;              git-gutter:next-hunk
;;              radian-git-gutter:beginning-of-hunk
;;              git-gutter:end-of-hunk
;;              git-gutter:revert-hunk)
;;   :init

;;   (radian-bind-key "v p" #'git-gutter:previous-hunk)
;;   (radian-bind-key "v n" #'git-gutter:next-hunk)
;;   (radian-bind-key "v a" #'radian-git-gutter:beginning-of-hunk)
;;   (radian-bind-key "v e" #'git-gutter:end-of-hunk)
;;   (radian-bind-key "v k" #'git-gutter:revert-hunk)

;;   ;; Disable in Org mode, as per
;;   ;; <https://github.com/syl20bnr/spacemacs/issues/10555> and
;;   ;; <https://github.com/syohex/emacs-git-gutter/issues/24>.
;;   ;; Apparently, the mode-enabling function for global minor modes
;;   ;; gets called for new buffers while they are still in
;;   ;; `fundamental-mode', before a major mode has been assigned. I
;;   ;; don't know why this is the case, but adding `fundamental-mode'
;;   ;; here fixes the issue.
;;   (setq git-gutter:disabled-modes '(fundamental-mode org-mode))

;;   (radian-defhook radian--git-gutter-load ()
;;     find-file-hook
;;     "Load `git-gutter' when initially finding a file."
;;     (require 'git-gutter)
;;     (remove-hook 'find-file-hook #'radian--git-gutter-load))

;;   :config

;;   ;; Don't prompt when reverting hunk.
;;   (setq git-gutter:ask-p nil)

;;   (global-git-gutter-mode +1)

;;   (defun radian-git-gutter:beginning-of-hunk ()
;;     "Move to beginning of current diff hunk."
;;     (interactive)
;;     (git-gutter:awhen (git-gutter:search-here-diffinfo git-gutter:diffinfos)
;;       (let ((lines (- (git-gutter-hunk-start-line it) (line-number-at-pos))))
;;         ;; This will move backwards since lines will be negative.
;;         (forward-line lines))))

;;   ;; Shuffle around all the hooks. `git-gutter' puts itself on a bunch
;;   ;; of different things, but not exactly the right things. Remove all
;;   ;; its meddling, and then do the right thing (run on window or
;;   ;; buffer switch after a top-level command, after a buffer revert,
;;   ;; and after Apheleia runs).

;;   (remove-hook 'post-command-hook #'git-gutter:post-command-hook)
;;   (ad-deactivate #'quit-window)
;;   (ad-deactivate #'switch-to-buffer)

;;   (defvar radian--git-gutter-last-buffer-and-window nil
;;     "Cons of current buffer and selected window before last command.
;; This is used to detect when the current buffer or selected window
;; changes, which means that `git-gutter' needs to be re-run.")

;;   (radian-defhook radian--git-gutter-on-buffer-or-window-change ()
;;     post-command-hook
;;     "Update `git-gutter' when current buffer or selected window changes."
;;     (let ((new (cons (current-buffer) (selected-window))))
;;       (unless (equal new radian--git-gutter-last-buffer-and-window)
;;         (setq radian--git-gutter-last-buffer-and-window new)
;;         ;; Sometimes the current buffer has not gotten updated yet
;;         ;; after switching window, for example after `quit-window'.
;;         (with-current-buffer (window-buffer)
;;           (when git-gutter-mode
;;             (when buffer-file-name
;;               (unless (file-remote-p buffer-file-name)
;;                 (git-gutter))))))))

;;   (use-feature autorevert
;;     :config

;;     (radian-defhook radian--git-gutter-after-autorevert ()
;;       after-revert-hook
;;       "Update `git-gutter' after the buffer is autoreverted."
;;       (when git-gutter-mode
;;         (git-gutter))))

;;   (use-feature apheleia
;;     :config

;;     (radian-defhook radian--git-gutter-after-apheleia ()
;;       apheleia-post-format-hook
;;       "Update `git-gutter' after Apheleia formats the buffer."
;;       (when git-gutter-mode
;;         (git-gutter))))

;;   :blackout git-gutter-mode)

;; Package `git-gutter-fringe' integrates with `git-gutter' to make
;; the gutter display use the window fringe rather than a column of
;; text.
;;
;; Note that we only even put the package on the load path if
;; `git-gutter-fringe' fringe is defined. The function might not be
;; defined if Emacs was not built with X/Cocoa support, and if that's
;; the case, then loading it will cause errors (and besides that, will
;; break `git-gutter' since the fringe stuff is not available).
;; However, we do need to load the package in order to byte-compile
;; this configuration. That's okay since it's only done in a
;; subprocess (so it won't break `git-gutter') but we still need to
;; fix the errors in that case. Hence the `eval-when-compile'.
;; (straight-register-package 'git-gutter-fringe)
;; (when (fboundp 'define-fringe-bitmap)
;;   (eval-when-compile
;;     (unless (fboundp 'define-fringe-bitmap)
;;       (fset 'define-fringe-bitmap #'ignore))
;;     (unless (boundp 'overflow-newline-into-fringe)
;;       (setq overflow-newline-into-fringe t)))
;;   (use-package git-gutter-fringe
;;     :demand t
;;     :after git-gutter
;;     :init

;;     (use-feature git-gutter
;;       :config

;;       ;; This function is only available when Emacs is built with
;;       ;; X/Cocoa support, see e.g.
;;       ;; <https://github.com/pft/mingus/issues/5>. If we try to
;;       ;; load/configure `git-gutter-fringe' without it, we run into
;;       ;; trouble.
;;       (when (fboundp 'define-fringe-bitmap)
;;         (require 'git-gutter-fringe)))

;;     :config

;;     (fringe-helper-define 'radian--git-gutter-blank nil
;;       "........"
;;       "........"
;;       "........"
;;       "........"
;;       "........"
;;       "........"
;;       "........"
;;       "........")

;;     (radian-defadvice radian--advice-git-gutter-remove-bitmaps
;;         (func &rest args)
;;       :around #'git-gutter-fr:view-diff-infos
;;       "Disable the cutesy bitmap pluses and minuses from `git-gutter-fringe'.
;; Instead, display simply a flat colored region in the fringe."
;;       (radian-flet ((defun fringe-helper-insert-region
;;                         (beg end _bitmap &rest args)
;;                       (apply fringe-helper-insert-region
;;                              beg end 'radian--git-gutter-blank args)))
;;         (apply func args)))))


;;** General packages 
;;*** EPA
(setq
 epa-armor t
 epa-file-name-regexp "\\.\\(gpg\\|asc\\)$"
 epa-pinentry-mode 'loopback	   ; use minibuffer reading passphrase
 epg-pinentry-mode 'loopback
 epa-file-encrypt-to '("milind.b.kamble@gmail.com"))
(epa-file-name-regexp-update)

;;*** `lispy' - ninja for sexp editing and navigation
;; Re-adjust minor-mode-map-alist so that lispy, lispy-goto and lispy-other modes go to end of it
;; This will make the keybinding precedenge of lispy to be the least
(defun my-fling-lispy-to-end ()
  (mapc (lambda (x)
	  (let ((myentry (assq x minor-mode-map-alist)))
	    ;; delete all occurences of x from alist and add it at the end
	    (assq-delete-all x minor-mode-map-alist)
	    (add-to-list 'minor-mode-map-alist myentry t)))
	'(lispy-mode lispy-other-mode lispy-goto-mode)))
(use-package lispy
  :config
  (blackout 'lispy-mode (concat " " (all-the-icons-fileicon "scheme")))
  :hook ((emacs-lisp-mode . lispy-mode)
	 (lisp-mode . lispy-mode)
	 (clojure-mode . lispy-mode)
	 (scheme-mode . lispy-mode)
	 (sly-mrepl-mode . lispy-mode)
	 (lispy-mode . my-fling-lispy-to-end)))

;;*** `bind-key' : better keybinding API
;; Package `bind-key' provides a macro by the same name (along with
;; `bind-key*' and `unbind-key') which provides a much prettier API
;; for manipulating keymaps than `define-key' and `global-set-key' do.
;; It's also the same API that `:bind' and similar keywords in
;; `use-package' use. (cf: radian.el)
(require 'bind-key)
;;*** `crux' Collection of Ridiculously Useful eXtensions for Emacs
(use-package crux)
;;*** `no-littering' - keep files inside user-emacs-directory 
(use-package no-littering
  :commands (no-littering-expand-var-file-name
             no-littering-expand-etc-file-name)
  :demand t
  :config
  ;; (add-to-list 'recentf-exclude no-littering-var-directory)
  ;; (add-to-list 'recentf-exclude no-littering-etc-directory)
  (setq
   auto-save-file-name-transforms     ; transform names of saved files
   `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))
   backup-directory-alist ; store all backup and autosave files in "backups"
   `((".*" . ,(no-littering-expand-var-file-name "backups/")))
   custom-file (no-littering-expand-etc-file-name "custom.el")))

;;*** `recentf' - handy access to recent files
;; (use-package frecency)
;; (use-package persist)
;; actually from elpa
;; (use-package frecentf
;;   :after no-littering
;;   :config
;;   (frecentf-mode)
;;   :bind
;;   ("C-x C-r" . #'frecentf-pick-file)
;;   ("C-x C-d" . #'frecentf-pick-dir)
;; )

(use-package recentf
  :after no-littering
  :init
  (recentf-mode +1)
  :config
  (add-to-list 'recentf-exclude "^/\\(?:ssh\\|su\\|sudo\\)?:")
  ;; :bind
  ;; ("C-x C-r" . #'recentf-open-files)
  :custom
  (recentf-max-saved-items 100)
  (recentf-max-menu-items 20)
  (recentf-auto-cleanup 'never)
  (recentf-keep '(file-remote-p file-readable-p)))

;;*** configure `which-key'
(use-package which-key
  ;; :demand t
  :blackout t
  :init
  (which-key-mode 1))

;;*** `orderless' - a completion style using space-separated patters in any order
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless)))

;;*** Use `selectrum' instead of ivy (or other completion systems such as ido, helm, raven, swiper etc.)
;; selectrum provides completion-at-point functionality
(use-package selectrum
  :diminish "sel"
  :init
  (selectrum-mode +1))

;;*** `company' for completion 
(progn
  (if nil nil 				; disabling company-mode in-lue of consult
    (use-package company
      :commands company-mode
      :blackout t
      :bind (:map company-active-map
		  ("C-n" . 'company-select-next)
		  ("C-p" . 'company-select-previous))
      :hook ((emacs-lisp-mode . company-mode)
             (lisp-mode . company-mode)
             (sly-mrepl-mode . company-mode))
      :config
      (setq company-idle-delay 0)

      ;; Using digits to select company-mode candidates
      ;; https://oremacs.com/2017/12/27/company-numbers/
      (setq company-show-numbers t)

      (let ((map company-active-map))
	(mapc
	 (lambda (x)
	   (define-key map (format "%d" x) 'ora-company-number))
	 (number-sequence 0 9))
	(define-key map " " (lambda ()
                              (interactive)
                              (company-abort)
                              (self-insert-command 1)))
	(define-key map (kbd "<return>") nil))

      (defun ora-company-number ()
	"Forward to `company-complete-number'.

Unless the number is potentially part of the candidate.
In that case, insert the number."
	(interactive)
	(let* ((k (this-command-keys))
               (re (concat "^" company-prefix k)))
	  (if (cl-find-if (lambda (s) (string-match re s))
			  company-candidates)
              (self-insert-command 1)
            (company-complete-number (string-to-number k))))))
    (use-package company-prescient
      :after (company precsient))
    (company-prescient-mode +1)

    ;; when idling on completion candidate, popup help after 'company-quickhelp-delay'
    (use-package company-quickhelp
      :hook (company-mode . company-quickhelp-local-mode))))


;;*** `consult' completion framework 
;; consult, marginalia, embark work together for providing completions

(load "my-consult-config")

;;*** `marginalia' enable richer annotations including richer annotation of
;; completion candidates
(use-package marginalia
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

;;*** `embark' package for mini-buffer actions and right-click contextual menu functionality
(use-package embark
  :ensure t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;;*** `embark-consult' package.
(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;;*** `prescient' sorts and filters lists of candidates generated by completion frameworks like company, consult etc.
(use-package prescient
  :defer t
  :custom
  (prescient-sort-length-enable nil)
  :config
  (prescient-persist-mode 1))

(use-package selectrum-prescient
  :init
  (selectrum-prescient-mode +1)
  :custom
  (selectrum-prescient-enable-filtering nil) ; prescient on top of orderless, enabling freceny-based sorting
  )
;; (prescient-persist-mode +1)


;;*** `outshine' for code folding
;; use super-/ for prefix than the clunky M-#
(defvar outline-minor-mode-prefix (kbd "s-/"))

(use-package outshine
  :after all-the-icons
  ;; :blackout " Oshine"
  :config
  (blackout 'outshine-mode (concat " " (all-the-icons-material "format_align_left")))
  (blackout 'outline-minor-mode)
  (setq outshine-startup-folded-p t)
  ;; using bind causes modes such as lispy (when visiting elisp files)   to override outshine-cycle-buffer when we are on a legit header line
  ;; using bind* on the other hand causes outshine-cycle-buffer to override org-mode bindings when visiting org files.
  ;; need to find a robust solution to tackle outshine and lispy conflicts
  ;; :bind*
  ;; ("<backtab>" . #'outshine-cycle-buffer)
  ;; ("s-/ <backtab>" . #'outshine-cycle-buffer)
					; enable cycle-buffer from anywhere
  :hook
  ;; enable outline-minor-mode for *ALL* programming buffers 
  (prog-mode . outshine-mode))

;;*** `undo-tree' is a more intuitive way to navigate undo instead of linear traversal
(use-package undo-tree
  :straight t
  :blackout t
  :bind (("C-x u" . undo-tree-visualize))
  :init (global-undo-tree-mode))


;;*** `hercules' is a which-key based hydra
(use-package hercules
  :commands
  (hercules-def))

;;*** `free-keys' shows unbound keys
(use-package free-keys) 
;;*** `selected'
(use-package selected
  :ensure t
  :commands selected-minor-mode
  :bind (:map selected-keymap
              ("q" . selected-off)
              ("u" . upcase-region)
              ("d" . downcase-region)
              ("w" . count-words-region)
              ("m" . apply-macro-to-region-lines)))
;;** Themes
;;*** install `poet-theme'
(use-package poet-theme
  :straight (:host github :repo "mbkamble/poet"))

;;*** `page-break-lines' - render form-feed chars as horz lines
(use-package page-break-lines)


;;*** `markdown-mode'
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;;*** prettify symbols
(add-hook 'prog-mode-hook #'prettify-symbols-mode)
(add-hook 'prog-mode-hook (lambda () (setq indicate-buffer-boundaries 'left)))

;;** Programming support
;;*** `yaml-mode'
(use-package yaml-mode
  :mode ("\\.yml\\'" . yaml-mode))
;;*** `python-mode'
(use-package python
  :ensure t)

;;*** `cc-mode' provides major modes for C, C++, Objective-C, and Java
(use-feature cc-mode
  :load-path "straight/build/cc-mode/"
  :config
  
  ;;   "Unconditionally inhibit CC submode indicators in the mode lighter.")
  ;; Rather than using radian's defadvice to nullify the c-update-modeline func
  ;; I'm directly redefining it. To me, it seems more easier to understand.
  ;; (radian-defadvice radian--advice-inhibit-c-submode-indicators (&rest _)
  ;;   :override #'c-update-modeline
  (defun c-update-modeline () nil)
  
  ;; Switch to a better indentation-and-braces style. This turns the
  ;; following code:
  ;;
  ;; if (condition)
  ;;   {
  ;;     statement;
  ;;   }
  ;;
  ;; Into this:
  ;;
  ;; if (condition)
  ;; {
  ;;   statement;
  ;; }
  ;;
  ;; We do this by defining a custom style that is based on BSD, and
  ;; then overriding the indentation (which is set to 8 spaces by
  ;; default). This style is only used for languages which do not have
  ;; a more specific style set in `c-default-style'.
  (c-add-style "radian-bsd"
               '("bsd" (c-basic-offset . 2)))
  (setf (alist-get 'other c-default-style) "radian-bsd")
  
  (put 'c-default-style 'safe-local-variable #'stringp))

;;*** `yasnippet' 
(use-package yasnippet
  :straight t
  :config
  (blackout 'yas-minor-mode (concat " " (all-the-icons-fileicon "yasm")))
  :hook ((lisp-interaction-mode . yas-minor-mode)
	 (emacs-lisp-mode . yas-minor-mode)
	 (lisp-mode . yas-minor-mode)
	 (org-mode . yas-minor-mode)
	 (c++-mode . yas-minor-mode)
	 (c-mode . yas-minor-mode)))

(use-package yasnippet-snippets
  :after yasnippet
  ;; :defer 3.2
  :defer t
  :config (yas-reload-all))

;;*** `tiny' a template expander -- loopy alternative to yasnippet
(use-package tiny
  :defer t)

;;*** `beancount' mode for accounting files
(use-package beancount
  :straight (beancount
             :type git
             :host github
             :repo "cnsunyour/beancount.el")
  :bind*
  ("C-c n" . #'beancount-goto-next-transaction)
  ;; there is no beancount-goto-prev-transaction, so backward-paragraph
  ;; is a reasonable alternative 
  ("C-c p" . #'backward-paragraph)
  :mode
  ("\\.bean\\(?:count\\)?\\'" . beancount-mode)
  :config
  (setq beancount-accounts-files
        (directory-files "~/docs/account_books/"
                         'full
                         (rx (or ".beancount" ".bc") eos))))



;;** `orgmode' customization  
;;*** `org-mode' itself 
(use-package org
  :mode ("\\.org\\'" . org-mode)
  ;; :diminish org-indent-mode
  :config
  (setq org-agenda-files "~/notes/agenda.org")
  (setq org-directory "~/notes")
  (require 'ox-html)
  (require 'ox-latex)
  (require 'ox-md)
  (setq org-startup-indented t)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (emacs-lisp . t)
     (shell . t)
     (lisp . t)
     (gnuplot . t)
     (R . t)
     (C . t)))
  (setq org-src-window-setup 'current-window)
  (setq org-agenda-window-setup 'current-window))

;;*** `poporg' edit comment strings in a popout orgmode
(use-package poporg
  :commands (poporg-dwim)
  :bind ("C-x c" . poporg-dwim))

;;*** `org-bullets'
;; see https://mstempl.netlify.app/post/beautify-org-mode/ for techniques to beautify org rendering
;; (use-package org-bullets
;;   :hook (org-mode . org-bullets-mode))

;;*** `org-superstar' prettify headings and lists
;; This package is a direct descendant of ‘org-bullets’, with most of the code base completely rewritten
(use-package org-superstar
  :config
  (setq org-superstar-special-todo-items t)
  :hook (org-mode . my-org-superstar-lw-mode))
(defun my-org-superstar-lw-mode ()
  "Start Org Superstar differently depending on the number of lists items."
  (let ((list-items
         (count-matches "^[ \t]*?\\([+-]\\|[ \t]\\*\\)"
                        (point-min) (point-max))))
    (unless (< list-items 100)
      (org-superstar-toggle-lightweight-lists)))
  (org-superstar-mode))

;;*** `ob-sync'
(use-package ob-async
  :defer 7.3)

;;*** `org-ref'
;; (use-package org-ref
;;   :straight t
;;   :defer t
;;   :config
;;   (setq reftex-default-bibliography '("~/notes/roam/math.bib")
;;         org-ref-default-bibliography '("~/notes/roam/math.bib")))

;;*** `org-roam' 
;; (use-package org-roam
;;   :straight t
;;   ;; :ensure-system-package
;;   ;; ((sqlite3)
;;   ;;  (graphviz))
;;   :defer t
;;   :bind
;;   (:map org-roam-mode-map
;;         (("C-c n l" . org-roam)
;;          ("C-c n f" . org-roam-find-file)
;;          ("C-c n g" . org-roam-graph-show))
;;         :map org-mode-map
;;         (("C-c n i" . org-roam-insert))
;;         (("C-c n I" . org-roam-insert-immediate)))
;;   :config
;;   (setq org-roam-directory "~/notes/roam/")
;;   (setq org-roam-index-file "Index.org")
;;   (setq org-roam-graph-node-extra-config '(("shape" . "ellipse")
;;                                            ("style" . "rounded,filled")
;;                                            ("fillcolor" . "#EFEFFF")
;;                                            ("color" . "#DEDEFF")
;;                                            ("fontcolor" . "#111111")))
;;   (setq org-roam-graph-viewer "chromium")
;;   (setq org-roam-capture-templates
;;         (list `("d" "default" plain #'org-roam--capture-get-point
;;                 "%?"
;;                 :file-name "%<%Y%m%d%H%M%S>-${slug}"
;;                 :head ,(concat "#+title: ${title}\n"
;;                                "#+author: \"Caio Henrique\"\n"
;;                                "#+date: <%<%Y-%m-%d>>\n")
;;                 :unnarrowed t)))
;;   (require 'org-roam-protocol))

;;*** `org-roam-server'
;; (use-package org-roam-server
;;   :straight t
;;   :defer t
;;   :config
;;   (setq org-roam-server-host "0.0.0.0"
;;         org-roam-server-port 8082
;;         org-roam-server-export-inline-images t
;;         org-roam-server-authenticate nil
;;         org-roam-server-network-poll t
;;         org-roam-server-network-arrows nil
;;         org-roam-server-network-label-truncate t
;;         org-roam-server-network-label-truncate-length 60
;;         org-roam-server-network-label-wrap-length 20))

;;*** `htmlize' - Export to html with syntax highlighting
(use-package htmlize
  :defer t)

;;*** configure `hydra' and load my-hydras 
(use-package hydra
  :defer 2.5)

;; (load "hydra-modal")
;; (load "my-hydras")

;;*** unlock keepassxc DB from emacs using dbus
;; read file content into a string http://ergoemacs.org/emacs/elisp_read_file_content.html
(defun get-string-from-file (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

;; Unlock the KeepassXC GUI-app
;; remotely without explicitly entering the password
;; Lot of learning and experimenting during development, but the
;; shortness of code is impressive.
;; Reading the safebox.gpg file auto decrypts it into plain text
;; (thanks to EPA). parson-parse converts json-structured
;; string into hierarchical alist, from which we extract the values of
;; password and security file. Finally the dbus API is used to execute
;; the method openDatabase on the keepass service
(defun unlock-keepass ()
  "unlock keepass app using dbus"
  (interactive)
  (require 'parson)
  (require 'dbus)
  (let* ((strong-box (expand-file-name
		      "~/.gnupg/keepass/keepass_safebox.gpg"))
	 (kdbx (expand-file-name
		"~/.gnupg/keepass/khaazgee_milind_keepass.kdbx"))
	 (pwd-db (parson-parse (get-string-from-file
				strong-box)))
	 (kdbx-sec (alist-get "keepasssec" (alist-get "mil" pwd-db nil nil
						      'string=)
			      nil nil 'string=))
	 (pwd (alist-get "keepasspwd" (alist-get "mil" pwd-db nil nil
						 'string=)
			 nil nil 'string=)))
    (cl-assert pwd-db nil "Could not parse strong-box") ;; pwd-db should be non-nil
    (cl-assert pwd nil "Password was not found in strong-box")
    (cl-assert kdbx-sec nil "Keepass security filepath not found in strong-box")
    (dbus-call-method :session "org.keepassxc.KeePassXC.MainWindow"
		      "/keepassxc" "org.keepassxc.MainWindow" "openDatabase"
		      kdbx pwd kdbx-sec)))

;;*** resize your windows to the `golden-ratio'
;; (http://pragmaticemacs.com/emacs/resize-your-windows-to-the-golden-ratio/)
(use-package golden-ratio
  :ensure t
  :blackout t ; active buffer resizing is providing visual feedback
  :init
  (golden-ratio-mode 1))

;;*** uniquify buffer names when visiting same basenames
;; https://github.com/bbatsov/prelude
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward
      uniquify-separator "/"
      uniquify-after-kill-buffer-p t ; rename after killing uniquified
      uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

;;*** `uniquify-files' create uniq names when opening same-named project files
(use-package uniquify-files)


;;*** Global
(use-package popup-kill-ring
  :bind (("M-y" . popup-kill-ring)))

(use-package expand-region
  :straight t
  :demand t
  :bind (("C-=" . er/expand-region)))

;;*** `smartparens' handles paired punctuations The auhor recommends
;; initialization using (require 'smartparens-config), but we want to
;; do so using use-package. Found the solution 
;; [[https://www.wisdomandwonder.com/article/9897/use-package-smartparens-config-ensure-smartparens][here]]
(use-package smartparens
  ;; :ensure t
  :defer 5.3
  :blackout t
  :config
  (setq sp-show-pair-from-inside nil)
  (require 'smartparens-config)
  (smartparens-global-mode))

(use-package highlight-parentheses
  :defer 5.3
  :blackout t
  :config (global-highlight-parentheses-mode))

;; is this necessary if we use highlight-parentheses?
;; (defvar show-paren-delay 0)
;; (show-paren-mode t)

;;*** `helpful' is an enhanced help
(use-package helpful
  :bind
  ("C-h f" . #'helpful-callable)
  ("C-h v" . #'helpful-variable)
  ("C-h k" . #'helpful-key)
  ("C-c C-d" .  #'helpful-at-point)
  ("C-h C" . #'helpful-command) 	; override describe-coding-system
  )

;;*** `ace-window' and `ace-link' 
(use-package ace-window
  :custom
  (aw-keys '(?a ?o ?e ?u ?h ?t ?n ?s))
  )

(use-package ace-link
  :defer 4.1
  :config (ace-link-setup-default))

;;*** `recently' for accessing recently visited files 
(use-package recently
  :blackout t
  :custom
  (recently-max 300)
  :bind
  ("C-x C-r" . #'recently-show)
  :config
  (message "turning on recently-mode")
  (recently-mode +1))

;; Switch to recent file. A simple function to list recent files using Selectrum. I bind to C-x C-r. (cf raxod502)
;; (defun recentf-open-files+ ()
;;   "Use `completing-read' to open a recent file."
;;   (interactive)
;;   (let ((files (mapcar 'abbreviate-file-name recentf-list)))
;;     (find-file (completing-read "Find recent file: " files nil t))))


;; (use-package multiple-cursors
;;   :straight t
;;   :defer t)

(use-package avy
  :straight t
  :defer t
  :custom
  (avy-keys '(?a ?o ?e ?u ?h ?t ?n ?s)))

;;*** `dired' 
(use-feature dired
  ;; :hook (dired-mode . dired-omit-mode)
  :bind (:map dired-mode-map
	      ("<return>" . dired-find-alternate-file)
	      ("C-<return>" . dired-find-file)
	      ("<dead-circumflex>" . dired-up-directory)
	      ("E" . image-dired)
	      ("J" . #'dired-up-directory) ; better than '^'
	      ;; ("J" . dired-omit-mode)  ; don't know if this is useful
	      ))

;;*** `dired-x'
(use-feature dired-x
  :after dired
  ;; :hook (dired-mode . dired-omit-mode)
  :config
  (setq dired-omit-verbose nil)
  (setq dired-omit-files
	"^\\..+$"))

(use-package peep-dired
  :straight t
  :after dired
  :bind (:map dired-mode-map
	      ("P" . 'peep-dired)))

(use-package dired-rainbow
  :straight t
  :after dired
  :config
  (dired-rainbow-define-chmod directory "#6cb2eb" "d.*")
  (dired-rainbow-define html "#eb5286" ("css" "less" "sass" "scss" "htm" "html"
					"jhtm" "mht" "eml" "mustache" "xhtml"))
  (dired-rainbow-define xml "#f2d024" ("xml" "xsd" "xsl" "xslt" "wsdl" "bib"
				       "json" "msg" "pgn" "rss" "yaml" "yml" "rdata"))
  (dired-rainbow-define document "#9561e2" ("docm" "doc" "docx" "odb" "odt"
					    "pdb" "pdf" "ps" "rtf" "djvu" "epub"
					    "odp" "ppt" "pptx"))
  (dired-rainbow-define markdown "#ffed4a" ("org" "etx" "info" "markdown"
					    "md" "mkd" "nfo" "pod" "rst"
					    "tex" "textfile" "txt"))
  (dired-rainbow-define database "#6574cd" ("xlsx" "xls" "csv" "accdb" "db" "mdb"
					    "sqlite" "nc"))
  (dired-rainbow-define media "#de751f" ("mp3" "mp4" "MP3" "MP4" "avi" "mpeg" "mpg"
					 "flv" "ogg" "mov" "mid" "midi" "wav" "aiff" "flac"))
  (dired-rainbow-define image "#f66d9b" ("tiff" "tif" "cdr" "gif" "ico"
					 "jpeg" "jpg" "png" "psd" "eps" "svg"))
  (dired-rainbow-define log "#c17d11" ("log"))
  (dired-rainbow-define shell "#f6993f" ("awk" "bash" "bat" "sed" "sh" "zsh" "vim"))
  (dired-rainbow-define interpreted "#38c172" ("py" "ipynb" "rb" "pl" "t" "msql" "mysql"
					       "pgsql" "sql" "r" "clj" "cljs" "scala" "js"))
  (dired-rainbow-define compiled "#4dc0b5" ("asm" "cl" "lisp" "el" "c" "h" "c++"
					    "h++" "hpp" "hxx" "m" "cc" "cs" "cp" "cpp"
					    "go" "f" "for" "ftn" "f90" "f95" "f03" "f08"
					    "s" "rs" "hi" "hs" "pyc" ".java"))
  (dired-rainbow-define executable "#8cc4ff" ("exe" "msi"))
  (dired-rainbow-define compressed "#51d88a" ("7z" "zip" "bz2" "tgz" "txz" "gz"
					      "xz" "z" "Z" "jar" "war" "ear" "rar"
					      "sar" "xpi" "apk" "xz" "tar"))
  (dired-rainbow-define packaged "#faad63" ("deb" "rpm" "apk" "jad" "jar" "cab" "pak"
					    "pk3" "vdf" "vpk" "bsp"))
  (dired-rainbow-define encrypted "#ffed4a" ("gpg" "pgp" "asc" "bfe" "enc" "signature"
					     "sig" "p12" "pem"))
  (dired-rainbow-define fonts "#6cb2eb" ("afm" "fon" "fnt" "pfb" "pfm" "ttf" "otf"))
  (dired-rainbow-define partition "#e3342f" ("dmg" "iso" "bin" "nrg" "qcow" "toast"
					     "vcd" "vmdk" "bak"))
  (dired-rainbow-define vc "#0074d9" ("git" "gitignore" "gitattributes" "gitmodules"))
  (dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*"))

(use-package eshell-z
  :after eshell
  :custom
  (eshell-z-freq-dir-hash-table-file-name
   (concat eshell-directory-name "z-dir-table")))

;; `eshell' utilities
;; other eshell related packxages
;; eshell-autojump  the command j to list common directories and to jump to them.
;; `eshell-bookmark' a simple package integrating eshell with bookmark.el.
;; `eshell-fixedprompt' Minor mode to restrict eshell to use a single fixed prompt
;;     wherein the prev command output isremoved (or scrolled out of sightG)uu;
;; `eshell-outline' Enhanced outline-mode for Eshell
;; `eshell-syntaxhighlighting'
;; `eshell-up' navigating to a specific parent directory in eshell without typing ../.. etc
(use-package eshell
  ;; :bind (:map eshell-mode-map
  ;;             ([remap eshell-pcomplete] . helm-esh-pcomplete)
  ;;             ("M-r" . helm-eshell-history)
  ;;             ("M-s f" . helm-eshell-prompts-all))
  :custom
  (eshell-banner-message "")
  ;; (eshell-scroll-to-bottom-on-input t)
  (eshell-error-if-no-glob t)
  (eshell-hist-ignoredups t)
  (eshell-save-history-on-exit t)
  (eshell-prefer-lisp-functions nil)
  ;; addding history-references to input-functions enable the use of !foo:n
  ;; to insert the nth arg of last command beg with foo
  ;; or !?foo:n for last command containing foo
  (add-to-list 'eshell-expand-input-functions 'eshell-expand-history-references)
  ;; (eshell-destroy-buffer-when-process-dies t)
  ;; (eshell-highlight-prompt nil)

  :config
  (setenv "PAGER" "cat")
  (require 'eshell-z)
  )

(use-package esh-autosuggest
  :after eshell
  :hook (eshell-mode . esh-autosuggest-mode))

;;** my customization
(when (file-exists-p custom-file) (load custom-file))

(setq

 ;; save frame realestate by not displaying calc-trail window
 calc-display-trail nil

 ;; always select help window when it appears
 help-window-select t
 ;; To avoid slowdown due to fonts and font-lock reliant packages the following is recommended
 ;; by https://github.com/integral-dw/org-superstar-mode
 inhibit-compacting-font-caches t

 visible-bell 1
 read-process-output-max (* 1024 1024))

;; super(mapped to windows key by X) key was an option I was considering for use as mod-singlekey accelerators. But Windows
;; has it's own shortcuts for it and shadows what we could have used them for emacs.
;; so following back to options using C or M modifiers to keep it portable across windows, linux
;;
;; |command           | origbind | newbind |
;; |kill-ring-save    | m-w      | c-c c   |
;; |yank              | c-y      | c-c v   |
;; |save some buffers | c-x s .  | c-c s . |
;; |ace-window        | C-x o    | C-c w   |
;; |consult-buffer    | C-x b    | C-c u   |

(bind-keys
 ;; :map default is global which is what we want
 ("C-c c" . kill-ring-save)
 ("C-c v" . yank)
 ("C-c s" . save-some-buffers) ; use . to save only current buffer, ! to save all. see info for other options
 ("C-c w" . ace-window)
 ("C-c u" . consult-buffer)
 )

;; (global-set-key (kbd "TAB") 'self-insert-command)
;; (global-set-key (kbd "\C-c h") 'highlight-symbol-at-point)

;; dired will open file or directory in existing buffer with key 'a'
(put 'dired-find-alternate-file 'disabled nil)
(global-visual-line-mode)


;; (defvar ispell-program-name "aspell")

;; Only y/n answers 
(defalias 'yes-or-no-p 'y-or-n-p)

;; For versions >= 27, this is done in early-init.el
(when (< emacs-major-version 27)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1))

(require 'my-ux) ;; my user interface
(my-set-appearance)

(require 'my-utils)

;; need to load it after theme is loaded because ryo-modal-cursor-color is defined as defconst
;; and initialized to 'cursor background'. Before theme is loaded, this value is black and
;; after theme is loaded, it is #FFD5BE. If it takes black value, it disappears
(load "my-ryo-modal")
;; we add some more hydratic bindings using hercules into ryo-modal-mode-map
(load "my-hercules")

(add-to-list 'safe-local-variable-values
	     '(eval add-hook 'after-save-hook
		    (lambda () (org-babel-tangle))
		    nil t))

(setq
 gc-cons-threshold 100000000
 gc-cons-percentage 0.1)
