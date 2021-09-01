;; personal configuration       -*- lexical-binding: t; -*-

;;** Custom

(defgroup my nil
  "Customization for my personnal variables and functions."
  :group 'convenience
  :version 1.0
  :prefix "my-")

(defcustom my-font "Victor Mono" ;; "Iosevka"
  "Font for coding situations."
  :group 'my
  :type 'string)

;; see https://zzamboni.org/post/beautifying-org-mode-in-emacs/
(defcustom my-variable-pitch-font  "Source Sans Pro"
  "Font for text"
  :group 'my
  :type 'string)

(defcustom my-theme 'poet-dark
  "Default theme for my emacs session"
  :group 'my
  :type 'theme)

(defcustom my-use-variable-pitch-font t
  "Whether to use a variable pitch font for non-coding situations or not.

Defaults to t."
  :group 'my
  :type 'boolean)

;;** Code

;;*** blackout some minor modes 
(blackout 'visual-line-mode)
(blackout 'auto-revert-mode)
(blackout 'eldoc-mode)

;;*** setup font and theme 
(defun my-set-appearance ()
  "Set the default theme and fonts"
  (when my-use-variable-pitch-font
    (set-face-attribute
     'variable-pitch
     nil
     :family my-variable-pitch-font
     :height 120
     :weight 'light)
    (add-hook 'text-mode-hook
              (lambda () (variable-pitch-mode 1))))

  (when window-system
    ;; increase space between lines
    (setq-default line-spacing 0)

    ;; change default font for current frame
    (add-to-list 'default-frame-alist `(font . ,my-font))
    (add-to-list 'default-frame-alist `(:height . 120))
    (set-face-attribute 'default nil :font my-font :height 120)
    (set-face-attribute 'fixed-pitch nil :font my-font :height 120)

    ;; use icon from all-the-icons for major mode instead of boring string
    ;; achieved by changing value of mode-name
    )
  (unless (eq my-theme 'default)
    (load-theme my-theme t)))

;;*** mode-line customization
;; default value of mode-line-format is ("%e" mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification mode-line-buffer-identification "   " mode-line-position (vc-mode vc-mode) "  " mode-line-modes mode-line-misc-info mode-line-end-spaces)
;; we can create a new value from scratch (see radian for example). But instead lets override the above variables to our liking
;; mode-line-front-space is spc for graphic display, '-' otherwise
;; mode-line-mule-info shows current-input-method. We may tweak it in future. For now, just set it to nil
(setq-default mode-line-mule-info nil)
;; mode-line-client-mode is ':' for normal frames, @ for emacsclient frames
;;**** major mode indicator is constructed by 'mode-name'
;; we can change the font to italic by customizing mode-line-buffer-id
(defun my-modeline-mode-name ()
  (let* ((icon (all-the-icons-icon-for-mode major-mode))
	 (face-prop (and (stringp icon) (get-text-property 0 'face icon))))
    (when (and (stringp icon) (not (string= major-mode icon)) face-prop)
      (setq mode-name (propertize icon 'display '(:ascent center))))))
(add-hook 'after-change-major-mode-hook #'my-modeline-mode-name)

;;**** customize modeline-modified indicator using chain icon 
;; mode-line-modified is a mode line construct for displaying whether current buffer is modified.
;; we beautify it with faicon (fa means fontawesome family)
(defun my-modeline-modified ()
  (let* ((config-alist
          '(("*" all-the-icons-faicon-family all-the-icons-faicon
             "chain-broken" :height 1.2 :v-adjust -0.0)
            ("-" all-the-icons-faicon-family all-the-icons-faicon
             "link" :height 1.2 :v-adjust -0.0)
            ("%" all-the-icons-octicon-family all-the-icons-octicon
             "lock" :height 1.2 :v-adjust 0.1)))
         (result (cdr (assoc (format-mode-line "%*") config-alist))))

    (propertize (format "%s" (apply (cadr result) (cddr result)))
                'face `(:family ,(funcall (car result)) :inherit ))))
(setq-default mode-line-modified '(:eval (my-modeline-modified)))

;; mode-line-remote is - for local buffer, @ for remote buffer

;;**** customize mode-line-position
;; from simple-modeline. displays linenumber and colnumber and optionally chars in region
;; ensure line number and column number modes are enabled
(line-number-mode +1)
(column-number-mode +1)
(defun simple-modeline-position ()
  "Displays the current cursor position in the mode-line.
If line and column enabled, display LNUM:CNUM. If only linenum enabled, display L<num>. If only colnum enabled
display C<num>. C<num> is ether 0 based on 1 based
Also display percentage of total and if active region, then stats about region
"
  `((line-number-mode
    ((column-number-mode
      (column-number-indicator-zero-based
       (8 " %l:%c")
       (8 " %l:%C"))
      (5 " L%l")))
    ((column-number-mode
      (column-number-indicator-zero-based
       (5 " C%c")
       (5 " C%C")))))
    (-4 " %p") 		        ; display percentage
    ,(if (region-active-p) 	; display region stats
         (propertize (format " +%s"
                             (apply #'+ (mapcar
					 (lambda (pos)
                                           (- (cdr pos)
                                              (car pos)))
					 (region-bounds))))
                     'font-lock-face 'font-lock-variable-name-face))))
(setq-default mode-line-position '(:eval (simple-modeline-position)))

(provide 'my-ux)
