;;; mbk-utils.el --- personal macros and functions   -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Samuel Barreto
;; Copyright (C) 2020  Milind Kamble

;; Author: Milind Kamble <milindbkamble@gmail.com>
;; Original Author: Samuel Barreto <samuel.barreto8@gmail.com>
;; Keywords: functions, elisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

;; get-string-from... got from http://ergoemacs.org/emacs/elisp_read_file_content.html
;; Read File Content into a String
(defun my--get-string-from-file (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

;; Read File Content as List of Lines
(defun my--read-lines (filePath)
  "Return a list of lines of a file at filePath."
  (with-temp-buffer
    (insert-file-contents filePath)
    (split-string (buffer-string) "\n" t)))

(defun my--group (source n)
  "Divide SOURCE list in N groups and stack together the last
elements.
"
  (if (zerop n) (error "Zero length"))
  (cl-labels ((rec (source acc)
                   (let ((rest (nthcdr n source)))
                     (if (consp rest)
                         (rec rest (cons (cl-subseq source 0 n) acc))
                       (nreverse (cons source acc))))))
    (if source (rec source nil) nil)))

(defmacro my--defaliases (&rest alias)
  (declare (indent 0))
  `(progn
     ,@(mapcar
        (lambda (pair)
          `(defalias ,@pair))
        (mbk--group alias 2))))

(defmacro my--hash (var docstring &rest kv-pairs)
  "Create a new hash table named VAR documented by DOCSTRING that
maps key to value for each key-value pair in KV-PAIRS."
  (declare (indent defun) (doc-string 2))
  (let ((s (/ (length kv-pairs) 2)))
    `(progn
       (defvar ,var (make-hash-table :size ,s :test 'equal) ,docstring)
       (mbk-with-args2 pair
         (puthash (car pair) (cadr pair) ,var)
         ,@kv-pairs))))

(defun my--add-to-list (lst &rest args)
  (mapcar
   (lambda (el) (add-to-list lst el))
   args))

(defmacro my--with-args2 (name body &rest args)
  (declare (indent defun))
  `(progn
     ,@(mapcar
        (lambda (g)
          `(cl-symbol-macrolet ((,name ',g))
             ,body))
        (mbk--group args 2))))

(defmacro my--set-custom (&rest args)
  (declare (indent 0))
  `(progn
     ,@(mapcar
        (lambda (pair) `(custom-set-variables '(,@pair)))
        (mbk--group args 2))))

(defun my--side-buffer (buffer &optional params)
  "Display BUFFER in a side window with parameters PARAMS."
  (declare (indent 2))
  (display-buffer-in-side-window
   buffer params)
  (select-window (get-buffer-window buffer)))

(defmacro my--define-after-save-hook-mode (name fun &optional lighter docstring)
  "Define a minor mode named NAME-after-save-mode that will run
FUN each time buffer is saved.

Minor mode has lighter LIGHTER and is documented by DOCSTRING."
    (declare (doc-string 4))
    (let ((toggler (intern (format "%s--toggle" name)))
          (mmode   (intern (format "%s-after-save-mode" name))))
      `(progn
         (defun ,toggler (toggle)
           (pcase toggle
             (:on  (add-hook    'after-save-hook ,fun nil t))
             (:off (remove-hook 'after-save-hook ,fun t    ))))

         (define-minor-mode ,mmode
           ,docstring
           nil ,lighter nil
           (if ,mmode (,toggler :on) (,toggler :off))))))

(defun my--append-string-to-file (string file)
  "Add STRING to end of FILE"
  (with-current-buffer (find-file-noselect file)
    (goto-char (point-max))
    (insert string)
    (save-buffer)
    (kill-current-buffer)))

(defun mbk--read-table (file)
  "Return contents of FILE as list of string, one element per
line."
  (with-current-buffer (find-file-noselect file)
    (unwind-protect
        (split-string
         (buffer-substring-no-properties (point-min) (point-max)))
      (kill-current-buffer))))

(defsubst my--concat (seq sep)
  "Concatenate sequence SEQ using SEP as separator"
  (mapconcat #'identity seq sep))

(defsubst my--add-to-hook (hook &rest funs)
  "Add FUNS to HOOK in a single run."
  (declare (indent 1))
  (mapcar (lambda (f) (add-hook hook f)) funs))

;; from https://stackoverflow.com/questions/3480173/show-keys-in-emacs-keymap-value
;; show user-friendly key-names instead of integers
(defun my--describe-keymap (keymap)
  "Describe a keymap using `substitute-command-keys'."
  (interactive
   (list (completing-read
          "Keymap: " (let (maps)
                       (mapatoms (lambda (sym)
                                   (and (boundp sym)
                                        (keymapp (symbol-value sym))
                                        (push sym maps))))
                       maps)
          nil t)))
  (with-output-to-temp-buffer (format "*keymap: %s*" keymap)
    (princ (format "%s\n\n" keymap))
    (princ (substitute-command-keys (format "\\{%s}" keymap)))
    (with-current-buffer standard-output ;; temp buffer
      (setq help-xref-stack-item (list #'mbk-describe-keymap keymap)))))

;; use counsel-find-library to visit source file for any feature/library
(defun my--find-library-name (arg)
  "Show the full path of package or library"
  (interactive "Mpackage/library: ")
  (message (find-library-name arg)))


;; Function to check if a packages exist in the load path. This may be used to preempt the installation of ELPA versions of packages whose source may already be found in the load path.
;; source: http://eschulte.github.io/emacs24-starter-kit/
(defun starter-kit-loadable-p (package)
  "Check if PACKAGE is loadable from a directory in `load-path'."
  (let ((load-file (concat (symbol-name package) ".el")))
    (catch 'file-found
      (dolist (dir load-path)
        (let ((path (expand-file-name load-file dir)))
          (when (file-exists-p path)
            (throw 'file-found path)))))))

;; Ubiquitous Packages which should be loaded on startup rather than autoloaded on demand since they are likely to be used in every session.
;; source: http://eschulte.github.io/emacs24-starter-kit/
;; (require 'cl)
;; (require 'cl-lib)
;; (require 'saveplace)
;; (require 'ffap)
;; (require 'uniquify)
;; (require 'ansi-color)
;; (require 'recentf)

;; from: https://www.emacswiki.org/emacs/basic-edit-toolkit.el
;; (defun kill-syntax-forward (&optional arg)
;;   "Kill ARG set of syntax characters after point."
;;   (interactive "p")
;;   (let ((arg (or arg 1))
;;         (inc (if (and arg (< arg 0)) 1 -1))
;;         (opoint (point)))
;;     (while (or                          ;(not (= arg 0)) ;; This condition is implied.
;;             (and (> arg 0) (not (eobp)))
;;             (and (< arg 0) (not (bobp))))
;;       (if (> arg 0)
;;           (skip-syntax-forward (string (char-syntax (char-after))))
;;         (skip-syntax-backward (string (char-syntax (char-before)))))
;;       (setq arg (+ arg inc)))
;;     (if (and (> arg 0) (eobp))
;;         (message "End of buffer"))
;;     (if (and (< arg 0) (bobp))
;;         (message "Beginning of buffer"))
;;     (kill-region opoint (point))))

;; (defun kill-syntax-backward (&optional arg)
;;   "Kill ARG set of syntax characters preceding point."
;;   (interactive "p")
;;   (kill-syntax-forward (- 0 (or arg 1))))

;;; use of dash filter function
;; (-filter #'consp (-flatten (lookup-key outline-minor-mode-map outline-minor-mode-prefix)))


;; from https://emacs.stackexchange.com/questions/653/how-can-i-find-out-in-which-keymap-a-key-is-bound
;; How can I find out in which keymap a key is bound?
(defun my--lookup-key (key)
  "Search for KEY in all known keymaps."
  (with-output-to-temp-buffer "mbktmp"
    (mapatoms (lambda (ob)
		(when (and (boundp ob) (keymapp (symbol-value ob)))
		  (when (functionp (lookup-key (symbol-value ob) key))
		    (princ ob) (princ "\n"))))
	      obarray)))
(defun my--lookup-key-prefix (key)
  "Search for KEY as prefix in all known keymaps."
  (with-output-to-temp-buffer "mbktmp"
    (mapatoms (lambda (ob)
		(when (and (boundp ob) (keymapp (symbol-value ob)))
		  (when (let ((m (lookup-key (symbol-value ob) key)))
			  (and m (or (symbolp m) (keymapp m))))
		    (princ ob) (princ "\n"))))
	      obarray)))
;; (lookup-key global-map (kbd "C-c"))
;; (lookup-key ryo-modal-mode-map (kbd "SPC"))

(defun my--get-keymaps ()
  (interactive)
  (mapatoms
   (lambda (x)
     (and (keymapp x)
	  (message (symbol-name x))))))

;;; from https://github.com/darkstego/wakib-keys/blob/master/wakib-keys.el
;; (defun wakib-dynamic-binding (key)
;;   "Act as KEY in the current context.
;; This uses an extended menu item's capability of dynamically computing a
;; definition.  This idea came from general.el"
;;   `(menu-item
;; 	 ,""
;; 	 nil
;; 	 :filter
;; 	 ,(lambda (&optional _)
;;         (wakib-key-binding key))))
;; '(define-key keymap (kbd "C-d") (wakib-dynamic-binding "C-c")))

(require 'dash)

(defun my--ispell-word-then-abbrev (p)
  "Call `ispell-word'. Then create an abbrev for the correction made.
With prefix P, create local abbrev. Otherwise it will be global."
  (interactive "P")
  (let ((before (downcase (or (thing-at-point 'word) "")))
        after)
    (call-interactively 'ispell-word)
    (setq after (downcase (or (thing-at-point 'word) "")))
    (unless (string= after before)
      (define-abbrev
        (if p local-abbrev-table global-abbrev-table) before after))
    (message "\"%s\" now expands to \"%s\" %sally."
             before after (if p "loc" "glob"))))

;; (define-key ctl-x-map (kbd "C-i") 'ispell-word-then-abbrev)


;; show the name of the keymap corresponding to the prefix key ARG in the given keymap

;; using dash command to print (index, car) of each element of minor-mode-map-alist
;; similar to enumerate of python
(defun my--enumerate-alist (alist)
  (let (l)
    (--each-indexed minor-mode-map-alist
      (push (list (car it) it-index) l))
    (mapc (lambda (i) (princ i t)) l)))

;; (defun my-enumerate-minor-mode-map-alist ()
;;   (require 'dash)
;;   (let (l)
;;     (--each-indexed minor-mode-map-alist
;;       (push (list (car it) it-index) l))
;;     (mapc (lambda (i) (princ i t)) l)))


;; a utility to print bindings of all C-a, C-b,...C-Z and corresponding M- in any buffer
(defun my--print-bindings (bufferstr modstr)
  "print C- and M- bindings of all alpha (lower and upper case) keys"
  (with-current-buffer bufferstr
    (describe-key-briefly
     (-map (lambda (c) (cons (kbd (format "%s%c" modstr c))
			(kbd (format "%s%c" modstr c))))
	   (number-sequence ?A ?z)))))

;; print C- and M- bindings of a keymap (outline-mode-prefix-map)
;; (let (el em)
;;   (-each (number-sequence ?a ?z)
;;     (lambda (x)
;;       (push (format "C-%c %s" x
;; 		    (lookup-key outline-mode-prefix-map
;; 				(kbd (format "C-%c" x))))
;; 	    el)
;;       (push (format "M-%c %s" x
;; 		    (lookup-key outline-mode-prefix-map
;; 				(kbd (format "ESC %c" x))))
;; 	    em)))
;;   (append  (reverse el) (reverse em)))

;; manipulate which-key-replacement-alist to eliminate display of certain keys eg. C-a to C-d
;; which-key-replacement-alist is of the form : list of (MATCH CONS . REPLACEMENT)
;; where each is (KEY REGEXP . BINDING REGEXP). if BINDING REGEXP is non-nil but not a cons, it is not displayd by which-key
;; (setq my--wk-rep-alist which-key-replacement-alist) ; backup copy
;; (setq which-key-replacement-alist (-snoc which-key-replacement-alist (cons (cons "C-[a-d]" nil) t)))
;; (setq which-key-replacement-alist (-snoc which-key-replacement-alist '(("C-[a-d]" . nil) . t)))


;; example of creating a sparse keymap with inheritence
;; instead of using setq, we use define-prefix-command in the production version
;; (setq my--ol-prefix-map
;; 	(let ((map (make-sparse-keymap)))
;; 	  (set-keymap-parent map outline-mode-prefix-map)
;; 	  ;; for a-z bind a to what C-a binds and A to what M-a binds
;; 	  (-each (number-sequence ?a ?z)
;; 	    (lambda (x)
;; 	      (bind-key (format "%c" x)
;; 			(lookup-key map (kbd (format "C-%c" x))) map nil)
;; 	      (bind-key (format "%c" (upcase x))
;; 			(lookup-key map (kbd (format "M-%c" x))) map nil)))
;; 	  map))

;; I was erroneously using hercules-def to create a hercules for single-invocation commands and doing a topsy-turvy disabling of the hercules by defining hide-funs for all the commands!. We don't need hercules for that
;; we can just bind the my--apps-map to a key and which key will display it
;; (hercules-def
;;  :show-funs #'my--apps-herc
;;  :hide-funs (-keep (lambda (x) (and (listp x ) (cdr x))) (cdr (-flatten my--apps-map)))
;;  :keymap 'my--apps-map
;;  :flatten t
;;  :transient t)

(provide 'my-utils)

