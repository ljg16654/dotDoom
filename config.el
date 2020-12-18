(setq user-full-name "Jigang Li"
      user-mail-address "ljg16654@sjtu.edu.cn"
)

(setq doom-font (font-spec :family "Monaco for Powerline" :size 34 :weight 'light)
      doom-variable-pitch-font (font-spec :family "GentiumAlt" :size 38)
      )
(setq-default line-spacing 0.5)

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
        org-agenda-files (apply #'append
			        (mapcar
			         (lambda (directory)
				        (directory-files-recursively directory org-agenda-file-regexp))
			            '("~/org/")))
        )
  (add-to-list 'org-modules 'org-habit)
  )

(defun org-latex-delete-cache () (interactive)
       (delete-directory "~/.emacs.d/.local/cache/org-latex" :RECURSIVE t))

(map! :desc "capture!" :ne "C-c c" #'org-capture)

(setq org-capture-templates
        '(("t" "Personal t" entry
           (file+headline +org-capture-todo-file "Inbox")
              "* TODO [%^{Select the urgency|A|B|C}] %?\n%i\n%a\n" :prepend t)

          ("n" "Personal notes" entry
           (file+headline +org-capture-notes-file "Inbox")
              "* %u %?\n%i\n%a" :prepend t)

          ;; declare root node j
          ("j" "Journal")

          ("ja" "Journal arbitrary recording" entry
           (file+olp+datetree +org-capture-journal-file)
              "* %?\n%u\n%i" :tree-type week)

          ("jc" "journal clock into something new" entry
           (file+olp+datetree +org-capture-journal-file)
              "* %?" :clock-in t :clock-keep t :tree-type week)

          ("jn" "journal edit the task currently clocked in" plain
           (clock) "%?" :unnarrowed t)

          ("r" "read later" checkitem
           (file (concat org-directory "/read-later.org"))
              "[ ] %? ")
))

(setq org-roam-directory "~/org-roam")

(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

(require 'helm-eshell)
(add-hook 'eshell-mode-hook
          #'(lambda ()
              (define-key eshell-mode-map (kbd "M-l")  'helm-eshell-history)))

(setq read-process-output-max 1048576)

(use-package lsp-python-ms
  :init (setq lsp-python-ms-auto-install-server t)
  :hook (python-mode . (lambda ()
                          (require 'lsp-python-ms)
                          (lsp))))  ; or lsp-deferred

(setq company-idle-delay 0.05)
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

;; prot
(setq-default dired-hide-details-mode t)
(use-package dired
  :hook (dired-mode-hook . dired-hide-details-mode)
  :config
  (setq delete-by-moving-to-trash t)
  )

(evil-define-key 'normal peep-dired-mode-map (kbd "j") 'peep-dired-next-file
                                             (kbd "k") 'peep-dired-prev-file)
(add-hook 'peep-dired-hook 'evil-normalize-keymaps)

(defun goto-downloads () (interactive)
  "Open Downloads folder."
  (find-file "~/Downloads"))

(defun go-home () (interactive)
       "Open home directory."
       (find-file "~/"))
(map! :desc "goto-downloads" :ne "SPC d d" #'goto-downloads)
(map! :desc "goto-home" :ne "SPC d h" #'go-home)
(map! :desc "peep-dired" :ne "SPC d p" #'peep-dired)

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
  :bind ("s-s" . prot/window-single-toggle))

(map! :desc "ace-window" :ne "SPC j" #'evil-switch-to-windows-last-buffer)

(setq browse-url-browser-function 'browse-url-firefox)

(load-theme 'doom-gruvbox-light t)

(defun my-nov-font-setup ()
  (face-remap-add-relative 'variable-pitch :family "Liberation Serif"
                           :height 1.5))
(defun enlarge-left-fringe ()
  (setq left-fringe-width 30))
(add-hook 'nov-mode-hook 'my-nov-font-setup)
(add-hook 'nov-mode-hook 'enlarge-left-fringe)

(map! :desc "next buffer" :ne "s-j" #'next-buffer)
(map! :desc "prev buffer" :ne "s-k" #'previous-buffer)
(map! :desc "other window" :ne "s-o" #'other-window)
(map! :desc "prev window" :ne "s-O" #'(lambda () (interactive) (other-window -1)))

(defun prot/make-frame-floating-with-current-buffer ()
  (interactive)
  (make-frame '((name . "脱出")
              (window-system . x)
              (minibuffer . nil)))
  (delete-window))

  (map! :desc "make floating frame" :ne "H-f" #'prot/make-frame-floating-with-current-buffer)

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

(map! :desc "line-number" :ne "SPC l n" #'display-line-numbers-mode)

(defun nolinum ()
  (display-line-numbers-mode 0)
  (olivetti-mode 1)
  )

(defun viper-lisp-mode ()
  (display-line-numbers-mode 0)
  (olivetti-mode 1)
  (paren-face-mode 1)
  (rainbow-delimiters-mode 0)
  (paredit-mode)
  (lispy-mode))
(add-hook 'org-mode-hook 'nolinum)
(add-hook 'lisp-mode-hook 'viper-lisp-mode)
(add-hook 'emacs-lisp-mode-hook 'viper-lisp-mode)

(setq matlab-shell-command "/usr/local/MATLAB/R2020a/bin/matlab")

(map! :desc "toggle olivetti-mode" :ne "SPC o v" #'olivetti-mode)

(setq-default global-hl-line-mode nil)

(display-time-mode)
(display-battery-mode)
