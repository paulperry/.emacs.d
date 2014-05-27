;;; .emacs --- Paul O. Perry .emacs file or .init.el file
;;; Commentary: from https://github.com/AndreaCrotti/minimal-emacs-configuration 
;; Other useful sources are: https://github.com/AndreaCrotti/Emacs-configuration
;; and https://github.com/jhamrick/emacs
;; 
;;; Code:

;; (let ((path (shell-command-to-string ". ~/.bash_login; echo -n $PATH")))
;;   (setenv "PATH" path)
;;   (setq exec-path 
;;         (append
;;          (split-string-and-unquote path ":")
;;          exec-path)))

;; Requisites: Emacs >= 24
(when (>= 24 emacs-major-version)
  ;; evaluate version 24 code
  (require 'package)
  (package-initialize)

  (add-to-list 'package-archives
	       '("melpa" . "http://melpa.milkbox.net/packages/"))
  (add-to-list 'package-archives
	       '("marmalade" . "http://marmalade-repo.org/packages/") t)

  (package-refresh-contents)

  (defun install-if-needed (package)
    (unless (package-installed-p package)
      (package-install package)))

  ;; make more packages available with the package installer
  (setq to-install
	'(python-mode magit yasnippet jedi auto-complete autopair find-file-in-repository flycheck))

  (mapc 'install-if-needed to-install)

  (require 'magit)
  (global-set-key "\C-xg" 'magit-status)

  (require 'auto-complete)
  (require 'autopair)
  (require 'yasnippet)
  (require 'flycheck)
  (global-flycheck-mode t)
  (global-auto-complete-mode t)

  (global-set-key [f7] 'find-file-in-repository)

					; auto-complete mode extra settings
  (setq
   ac-auto-start 2
   ac-override-local-map nil
   ac-use-menu-map t
   ac-candidate-limit 20)


  ;; Python mode settings
  (require 'python-mode)
  (add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
  (setq py-electric-colon-active t)
  (add-hook 'python-mode-hook 'autopair-mode)
  (add-hook 'python-mode-hook 'yas-minor-mode)
  (add-hook 'python-mode-hook 'hs-minor-modenn)
  (add-hook 'python-mode-hook 'imenu-add-menubar-index)

  ;; Jedi settings
  (require 'jedi)
  ;; It's also required to run "pip install --user jedi" and "pip
  ;; install --user epc" to get the Python side of the library to work
  ;; correctly with the same interpreter you're using.

  ;; if you need to change your python intepreter, do it here
					;(setq jedi:server-command
					;      '("python" "/Users/Paul/.emacs.d/elpa/jedi-20140321.1323/jediepcserver.py"))

  (add-hook 'python-mode-hook
	    (lambda ()
	      (jedi:setup)
	      (jedi:ac-setup)
	      (local-set-key "\C-cd" 'jedi:show-doc)
	      (local-set-key (kbd "M-SPC") 'jedi:complete)
	      (local-set-key (kbd "M-.") 'jedi:goto-definition)))

  (add-hook 'python-mode-hook 'auto-complete-mode)

;;; IPython Setup
  ;;
  (setq
   python-shell-interpreter "ipython"
   python-shell-interpreter-args ""
   python-shell-prompt-regexp "In \\[[0-9]+\\]: "
   python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
   python-shell-completion-setup-code
   "from IPython.core.completerlib import module_completion"
   python-shell-completion-module-string-code
   "';'.join(module_completion('''%s'''))\n"
   python-shell-completion-string-code
   "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

  (require 'ein)

  (add-hook 'ein:connect-mode-hook 'ein:jedi-setup)

  (ido-mode t)

  ;; -------------------- extra nice things --------------------
  ;; use shift to move around windows
  (windmove-default-keybindings 'shift)
					;(show-paren-mode t)
					; Turn beep off
  (setq visible-bell nil)

  (setq user-mail-address "pop@paulperry.net")

  ;; Are we on a mac?
  (setq is-mac (equal system-type 'darwin))

  ;; Setup environment variables from the user's shell.
					;(when is-mac
					;  (require-package 'exec-path-from-shell)
					;  (exec-path-from-shell-initialize))

  (setq default-directory (concat (getenv "HOME") "/"))

  ;; Set standard indent to 2 rather that 4 
  (setq standard-indent 2)

  ;; Color Themes
  (add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
  (load-theme 'deeper-blue t)
  )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("6981a905808c6137dc3a3b089b9393406d2cbddde1d9336bb9d372cbc204d592" "cd70962b469931807533f5ab78293e901253f5eeb133a46c2965359f23bfb2ea" "f0ea6118d1414b24c2e4babdc8e252707727e7b4ff2e791129f240a2b3093e32" default)))
 '(default-directory (concat (getenv "HOME") "/"))
 '(show-paren-mode t)
 '(inhibit-startup-screen t)
 '(mac-command-key-is-meta t)
 '(column-number-mode t)
 '(tool-bar-mode nil)
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(set-cursor-color "red")
 )
