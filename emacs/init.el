;;
;; This is the template Emacs configuration file for CS 3110!
;;
;; The included packages are:
;;   Tuareg Mode -- Major mode for editing Caml code.
;;   AucTeX Mode -- A package for writing and formatting TeX files.
;;   Color Theme -- A package for customizing the color theme of Emacs.
;;
;; You can add additional packages to this configuration by the following
;; (approximate) process
;;
;; 1. Download the desired package and move the directory to
;;    ~/.emacs.d/<package-name>
;; 2. Add the line:
;;      (add-to-list 'load-path "~/.emacs.d/<package-name>)
;;    To this file after the other similar lines below.
;; 3. Add the line
;;      (require '<package-name>)
;;    to this file.
;; 4. Add any additional configuration options for that package that you want
;;    to this file. (optional)
;;
;; If the above steps don't work, check the documentation for the package you
;; want on the Emacs wiki: http://www.emacswiki.org. (Usually googling the
;; package name will point you here).

(add-to-list 'load-path "~/.emacs.d/tuareg-2.0.6")
(add-to-list 'load-path "~/.emacs.d/auctex-11.87")
(add-to-list 'load-path "~/.emacs.d/auctex-11.87/preview")
(add-to-list 'load-path "~/.emacs.d/color-theme-6.6.0/")

;; Sets the PATH environment variable.
(setq path
      "/bin:/usr/bin:/sbin:/usr/sbin:/usr/local/bin:/usr/local/mysql/bin")
(setenv "PATH" path)
(setq exec-path (append exec-path '("/usr/local/bin")))

;; This line adds the path to ocamlc to your Emacs PATH variable. However, it assumes
;; that you installed OCaml throught the OPAM package management system. If the line
;; below does not work correctly, type
;;   $ which ocamlc
;;   <desired-path-name>/ocamlc
;;   $
;; into your terminal and replace: "/Users/`whoami`/.opam/4.0.1/bin" with
;; "<desired-path-name>".
(setq exec-path (append exec-path '("/Users/`whoami`/.opam/4.0.1/bin")))

;; Initialize Tuareg-mode for OCAML
(autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
(autoload 'camldebug "camldebug" "Run the Caml debugger" t)
(autoload 'tuareg-imenu-set-imenu "tuareg-imenu"
  "Configuration of imenu for tuareg" t)
(setq auto-mode-alist
      (append '(("\\.ml[ily]?$" . tuareg-mode)
                ("\\.topml$" . tuareg-mode))
              auto-mode-alist))

;; Adds /usr/texbin to the path
(getenv "PATH")
  (setenv "PATH"
  (concat
  "/usr/texbin" ":"
 (getenv "PATH")))
 (setq latex-run-command "pdflatex")

;; Configures LaTeX environment
(require 'tex-site)

(load "auctex.el" nil t t)
(require 'tex-mik)

;; sets default compiler to pdflatex
(setq TeX-engine 'pdflatex)

;; enables preview-latex mode
(add-hook 'LaTeX-mode-hook #'LaTeX-preview-setup)
(load "preview-latex.el" nil t t)

;; sets default variables
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-save-query nil)
(setq TeX-PDF-mode t)

;; Initialized COLOR THEME MODE
(require 'color-theme)
(color-theme-initialize)
;; To view the avaialable color themes type M-x color-theme <TAB>
;; To choose the color theme type M-x color-theme-<color-theme-name> <RET>
;; When you decide on a color theme that suits you, you can set it to the
;; default by adding the line:
;;   (color-theme-<color-theme-name>)
;; To this file.

;;;;;;;;;;;;;;;;;;;;;;;;;; ADDITIONAL CONFIGURATIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Here are a collection of available additional customizations that you can
;; enable or disable as you desire! Enable by uncommenting, or disable by
;; commenting out the line (the 'begin comment' character is ';').

;; Enables line numbers
(global-linum-mode 1)

;; Enables column numbers
(setq column-number-mode t)

;; Set window size to 80 characters
(add-to-list 'default-frame-alist '(width . 81))

;; Sets spaces to tabs
(setq-default indent-tabs-mode nil)

;; Don't scroll when pointer reaches end of screen
(setq auto-window-vscroll nil)
(setq backup-inhibited t)

;; Speed up closing-parenthesis blink
(setq blink-matching-delay 0.1)

;; Prevent autoscroll from jumping
(setq scroll-conservatively 10000)

;; Sets the initial prompt
(setq
 initial-scratch-message
 ";; If you want to succeed in this class, your code must compile. - mgn29")

; Don't wrap lines
(setq-default truncate-lines t)

;; Yanks go into system clipboard
(setq x-select-enable-clipboard t)

;; Let user change their own files
(setq wdired-allow-to-change-permissions t)

;; Accept 'y' and 'n' as answers to yes/no questions
(defalias 'yes-or-no-p 'y-or-n-p)
(setq dabbrev-case-distinction nil)
(setq dabbrev-case-fold-search nil)
(setq windmove-wrap-around t)
(setq echo-keystrokes 0.1)
(setq delete-active-region nil)

;; Stop saving automatically
;;(setq auto-save-default nil)

;; Turn off start message
;; (setq inhibit-startup-message t)

;; Turns off the menu bar.
;; (menu-bar-mode -1)

;; Turns off the tool bar.
;; (tool-bar-mode -1)

;; Turn off scroll bar
;; (scroll-bar-mode -1)

;; Get cursor to stop blinking
;; (blink-cursor-mode -1)

;; Turn word 'lambda' into a lambda symbol
;; (setq lambda-symbol (string (make-char 'greek-iso8859-7 107)))
;; (mapatoms (lambda (sym)
;;             (if (get sym 'disabled)
;;                 (put sym 'disabled nil))))

; Jump to matching parenthesis
;; (defun match-paren (arg)
;;   "Go to the matching paren if on a paren; otherwise insert %."
;;   (interactive "p")
;;   (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
;;         ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
;;         (t (self-insert-command (or arg 1)))))
;; (global-set-key (kbd "%") 'match-paren)
