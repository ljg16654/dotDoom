(setq user-full-name "Jigang Li"
      user-mail-address "ljg16654@sjtu.edu.cn"
)

(setq doom-font (font-spec :family "Monaco for Powerline" :size 40 :weight 'light)
      doom-variable-pitch-font (font-spec :family "Sans Serif" :size 40)
      )
(setq-default line-spacing 0.3)

(use-package! org-fancy-priorities
  :hook
  (org-mode . org-fancy-priorities-mode)
  :config
  (setq org-fancy-priorities-list '("⚡" "⬆" "⬇" "☕")))

(after! org
  (setq org-todo-keywords '((sequence "TODO(t)" "INPROGRESS(i)" "WAITING(w)" "|" "DONE(d!)" "CANCELLED(c!)"))
        org-priority-faces '((?A :foreground "#e45649")
                             (?B :foreground "#da8548")
                             (?C :foreground "#0098dd"))
        ;; org-latex-minted-options '(("frame" "lines") ("linenos=true"))
        org-journal-file-format "%Y-%m-%d.org"
        org-format-latex-options (plist-put org-format-latex-options :scale 4.0)
        org-agenda-files (apply 'append
			        (mapcar
			         (lambda (directory)
				   (directory-files-recursively
				    directory org-agenda-file-regexp))
			         '("~/org/")))
        )
  (add-to-list 'org-modules 'org-habit)
  )

(require 'helm-eshell)
(add-hook 'eshell-mode-hook
          #'(lambda ()
              (define-key eshell-mode-map (kbd "M-l")  'helm-eshell-history)))

(setq company-idle-delay 0.2)
(use-package company-box
  :hook (company-mode . company-box-mode))

(require 'emms-setup)
(emms-all)
(emms-default-players) ;; set up the list of the default players
;; where my music is?
(setq emms-source-file-default-directory "~/Music/")
;; shortcuts for emms
(after! emms
  (map! :desc "Select playlist" :ne "SPC a p" #'emms-add-playlist)
  (map! :desc "emms" :ne "SPC e m" #'emms)
  (append emms-player-mplayer-parameters (list "-novideo"))
  )

(use-package parinfer
  :ensure t
  :bind
  (("C-," . parinfer-toggle-mode))
  :init
  (progn
    (setq parinfer-extensions
          '(defaults       ; should be included.
            pretty-parens  ; different paren styles for different modes.
            evil           ; If you use Evil.
            lispy          ; If you use Lispy. With this extension, you should install Lispy and do not enable lispy-mode directly.
            ;; paredit        ; Introduce some paredit commands.
            smart-tab      ; C-b & C-f jump positions and smart shift with tab & S-tab.
            smart-yank))   ; Yank behavior depend on mode.
    (add-hook 'clojure-mode-hook #'parinfer-mode)
    (add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
    (add-hook 'common-lisp-mode-hook #'parinfer-mode)
    (add-hook 'scheme-mode-hook #'parinfer-mode)
    (add-hook 'lisp-mode-hook #'parinfer-mode)))

;; configuration of input method pyim
(use-package! pyim
  :ensure nil
  :demand t
  :config
  (use-package pyim-basedict
    :ensure nil
    :config (pyim-basedict-enable))
  (setq default-input-method "pyim")
  (setq pyim-default-scheme 'quanpin)
  (setq pyim-page-tooltip 'posframe)
  )

(defun my-nov-font-setup ()
  (face-remap-add-relative 'variable-pitch :family "Liberation Serif"
                           :height 1.5))
(defun enlarge-left-fringe ()
  (setq left-fringe-width 30))
(add-hook 'nov-mode-hook 'my-nov-font-setup)
(add-hook 'nov-mode-hook 'enlarge-left-fringe)

;; prot
(use-package dired
  :hook (dired-mode-hook . dired-hide-details-mode)
  :config
  (setq delete-by-moving-to-trash t)
  )

(defun goto-downloads () (interactive)
  "Open Downloads folder."
  (find-file "~/Downloads"))

(defun xah-open-in-external-app (&optional @fname)
  "Open the current file or dired marked files in external app.
The app is chosen from your OS's preference.

When called in emacs lisp, if @fname is given, open that.

URL `http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html'
Version 2019-11-04"
  (interactive)
  (let* (
         ($file-list
          (if @fname
              (progn (list @fname))
            (if (string-equal major-mode "dired-mode")
                (dired-get-marked-files)
              (list (buffer-file-name)))))
         ($do-it-p (if (<= (length $file-list) 5)
                       t
                     (y-or-n-p "Open more than 5 files? "))))
    (when $do-it-p
      (cond
       ((string-equal system-type "windows-nt")
        (mapc
         (lambda ($fpath)
           (w32-shell-execute "open" $fpath)) $file-list))
       ((string-equal system-type "darwin")
        (mapc
         (lambda ($fpath)
           (shell-command
            (concat "open " (shell-quote-argument $fpath))))  $file-list))
       ((string-equal system-type "gnu/linux")
        (mapc
         (lambda ($fpath) (let ((process-connection-type nil))
                            (start-process "" nil "xdg-open" $fpath))) $file-list))))))

(map! :desc "Open in external app" :ne "SPC e o" #'xah-open-in-external-app)

(defun transparency (value)
  "sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "ntransparency value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))

(unless (display-graphic-p)
        (require 'evil-terminal-cursor-changer)
        (evil-terminal-cursor-changer-activate) ; or (etcc-on)
        )

(setq module-file-suffix t)

(keyfreq-mode 1)
(keyfreq-autosave-mode 1)

;disable backup
(setq backup-inhibited t)
;disable auto save
(setq auto-save-default nil)

(defun nolinum ()
  (display-line-numbers-mode 0)
  (olivetti-mode 1)
  )

(defun viper-lisp-mode ()
  (display-line-numbers-mode 0)
  (olivetti-mode 1)
  (paren-face-mode 1)
  (rainbow-delimiters-mode 0)
  (parinfer-mode 1)
  (lispy-mode 0))
(add-hook 'org-mode-hook 'nolinum)
(add-hook 'lisp-mode-hook 'viper-lisp-mode)
(add-hook 'emacs-lisp-mode-hook 'viper-lisp-mode)

(require 'ivy-posframe)
;; display at `ivy-posframe-style'
(setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-window-center)))
(setq ivy-posframe-parameters
      '((left-fringe . 8)
        (right-fringe . 8)))
(ivy-posframe-mode 1)

(map! :desc "ace-window" :ne "SPC v" #'ace-window)

(use-package emacs
  :config
  (defvar prot/window-configuration nil
    "Current window configuration.
Intended for use by `prot/window-monocle'.")

  (define-minor-mode prot/window-single-toggle
    "Toggle between multiple windows and single window.
This is the equivalent of maximising a window.  Tiling window
managers such as DWM, BSPWM refer to this state as 'monocle'."
    :lighter " [M]"
    :global nil
    (if (one-window-p)
        (when prot/window-configuration
          (set-window-configuration prot/window-configuration))
      (setq prot/window-configuration (current-window-configuration))
      (delete-other-windows)))
  :bind ("H-s" . prot/window-single-toggle))

(setq initial-buffer-choice "~/.doom.d/Splash.org")

(map! :desc "line-number" :ne "SPC l n" #'display-line-numbers-mode)
(map! :desc "goto-downloads" :ne "SPC d d" #'goto-downloads)