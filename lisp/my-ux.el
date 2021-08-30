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

(blackout 'visual-line-mode)
(blackout 'auto-revert-mode)
(blackout 'eldoc-mode)


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
    )
  (unless (eq my-theme 'default)
    (load-theme my-theme t))
  )

(provide 'my-ux)
