;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Jigang Li"
      user-mail-address "ljg16654@sjtu.edu.cn")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
;;
(use-package! org-fancy-priorities
  :hook
  (org-mode . org-fancy-priorities-mode)
  :config
  (setq org-fancy-priorities-list '("⚡" "⬆" "⬇" "☕")))
(map! :desc "Create Sparse Tree" :ne "SPC / s" #'org-sparse-tree)
(map! :desc "Create Sparse Tree for Tags" :ne "SPC / t" #'org-tags-sparse-tree)
(map! :desc "Open in external app" :ne "SPC e o" #'xah-open-in-external-app)
;;(setq org-format-latex-options (plist-put org-format-latex-options :scale 3.8))
;;(setq org-format-latex-options (org-format-latex-options :scale 3.5))
(setq projetile-project-search-path '("~/notes" "~/catkin_ws" "~/Desktop"))
(yas-global-mode t)
(after! org
  (setq org-todo-keywords '((sequence "TODO(t)" "INPROGRESS(i)" "WAITING(w)" "|" "DONE(d!)" "CANCELLED(c!)"))
        org-priority-faces '((?A :foreground "#e45649")
                             (?B :foreground "#da8548")
                             (?C :foreground "#0098dd"))
        org-latex-minted-options '(("frame" "lines") ("linenos=true"))
        org-journal-file-format "%Y-%m-%d.org"
        )
  ;;       org-latex-listings 'minted
  ;;       org-latex-packages-alist '(("" "minted"))
  ;;       org-latex-pdf-process
  ;;     '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
  ;;       "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f")
  (add-to-list 'org-modules 'org-habit t)
  )


(defun toggle-transparency ()
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
                    ((numberp (cdr alpha)) (cdr alpha))
                    ;; Also handle undocumented (<active> <inactive>) form.
                    ((numberp (cadr alpha)) (cadr alpha)))
              100)
         '(85 . 50) '(100 . 100)))))
(global-set-key (kbd "C-c t") 'toggle-transparency)


; set transparency of emacs
(defun transparency (value)
  "sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "ntransparency value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))

;; (use-package! cmake-ide
;;   (cmake-ide-setup))(global-set-key (kbd "M-p") 'ace-window)

(display-battery-mode)

;; quicker switch between windows
;; https://emacs.stackexchange.com/questions/3458/how-to-switch-between-windows-quickly
(use-package! ace-window)
(global-set-key (kbd "M-p") 'ace-window)
;; emms
(require 'emms-setup)
(emms-all)
(emms-default-players) ;; set up the list of the default players
;; where my music is?
(setq emms-source-file-default-directory "~/Music/")
;; shortcuts for emms
(after! emms
  (map! :desc "Select playlist" :ne "SPC a p" #'emms-add-playlist)
  (map! :desc "emms" :ne "SPC e m" #'emms)
  )
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
  (pyim-isearch-mode 1)
  (setq pyim-page-tooltip 'posframe)
  )
;; use xdg-open to open file from within emacs -nw
;; https://emacs.stackexchange.com/questions/20494/open-buffer-in-external-application-using-xdg-open bs
(defun counsel-locate-action-extern (x)
  "Use xdg-open shell command on X."
  (call-process shell-file-name nil
                nil nil
                shell-command-switch
                (format "%s %s"
                        (if (eq system-type 'darwin)
                            "open"
                          "xdg-open")
                        (shell-quote-argument x))))

;; change cursor shape (for evil-mode in emacs -nw)
(unless (display-graphic-p)
        (require 'evil-terminal-cursor-changer)
        (evil-terminal-cursor-changer-activate) ; or (etcc-on)
        )


;; open file in external app
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

(setq module-file-suffix t)
