;; This file contains any customizations to the installed packages in
;; the file 3110-packages.el.
;;
;; If you want to add additional package customizations, for example
;; if you installed a new package, you should do so in this
;; file. Common package customization options can be found at the
;; Emacs wiki online.n

(load "$HOME/.emacs.d/packages.el")

;; Sets the PATH environment variable
(require 'exec-path-from-shell)
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "OCAML_TOPLEVEL_PATH"))

;; Initializes tuareg-mode for OCaml
(autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
(autoload 'camldebug "camldebug" "Run the Caml debugger" t)
(autoload 'tuareg-imenu-set-imenu "tuareg-imenu"
  "Configuration of imenu for tuareg" t)
(setq auto-mode-alist
      (append '(("\\.ml[ily]?$" . tuareg-mode)
                ("\\.topml$" . tuareg-mode))
              auto-mode-alist))

;; Custom OCaml hook. This variable sets additional minor modes to run
;; when editing a .ml file.
(add-hook 'tuareg-mode-hook
          '(lambda ()
             ;; pressing <RET> also indents
             (local-set-key (kbd "RET") 'newline-and-indent)

             ;; Cleans us whitespace on save
             (require 'whitespace)
             (make-local-variable 'before-save-hook)
             (add-hook 'before-save-hook 'whitespace-cleanup)

             ;; ;; (OPTIONAL) Turns the "fun" keyword into a lambda.
             ;; (font-lock-add-keywords
             ;;  nil `(("(\\(fun\\>\\)"
             ;;         (0 (progn (compose-region (match-beginning 1) (match-end 1)
             ;;                                   ,(make-char 'greek-iso8859-7 235))
             ;;                   nil)))))
))

;; Sets the color theme
(require 'color-theme)
(color-theme-intitialize)
;; To set the color theme do
;; (color-theme-<desired-theme>)
;; I like zenburn-theme. You can add this to your packages by doing
;; M-x package-install <RET> zenburn-theme
;; then add (load 'zenburn-theme t)

;; Starts up AucTeX mode
(require 'tex-site)
;; Customize TeX variables
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(setq TeX-PDF-mode t)
;; Customize TeX hook
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)
