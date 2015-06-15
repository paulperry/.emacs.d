;;; .emacs --- Paul O. Perry .emacs file or .init.el file
;;; Commentary: from https://github.com/AndreaCrotti/minimal-emacs-configuration 
;; Other useful sources are: https://github.com/AndreaCrotti/Emacs-configuration
;; and https://github.com/jhamrick/emacs
;; 
;;; Code:

;; Requires: Emacs >= 24
(when (>= emacs-major-version 24)
  ;; evaluate version 24 code
  (require 'package)
  (package-initialize)

  (add-to-list 'package-archives
	       '("melpa" . "http://melpa.milkbox.net/packages/"))
  (add-to-list 'package-archives
	       '("marmalade" . "http://marmalade-repo.org/packages/") t)
  (add-to-list 'load-path "~/.emacs.d/site-lisp")
  (progn (cd "~/.emacs.d/site-lisp")
	 (normal-top-level-add-subdirs-to-load-path))

  (when (not package-archive-contents)
    (package-refresh-contents))

  (defun install-if-needed (package)
    (unless (package-installed-p package)
      (package-install package)))

  ;; make more packages available with the package installer
  (setq to-install
	'(python-mode markdown-mode magit yasnippet jedi direx
	  auto-complete autopair find-file-in-repository flycheck)) ;;  pydoc-info))

  (mapc 'install-if-needed to-install)

  (require 'magit)
  (global-set-key "\C-xg" 'magit-status)

  (require 'auto-complete)
  (global-auto-complete-mode t)
  (require 'autopair)
  (require 'yasnippet)
  (require 'flycheck)
  (global-flycheck-mode t)
;  (require 'pydoc-info)
  
  (global-set-key [f7] 'find-file-in-repository)
  (add-hook 'after-init-hook #'global-flycheck-mode)

  (require 'popwin)
  (popwin-mode 1)

  (require 'direx)
  (push '(direx:direx-mode :position left :width 25 :dedicated t)
	popwin:special-display-config)
  (global-set-key (kbd "C-x C-j") 'direx:jump-to-directory-other-window)
  
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
  (add-hook 'python-mode-hook 'hs-minor-mode)
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
;	      (jedi:ac-setup)
	      (local-set-key "\C-cd" 'jedi:show-doc)
	      (local-set-key (kbd "M-SPC") 'jedi:complete)
	      (local-set-key (kbd "M-.") 'jedi:goto-definition)
	      ))

;  (jedi:use-shortcuts t)

  (add-hook 'python-mode-hook 'auto-complete-mode)

  (require 'jedi-direx)
  (eval-after-load "python"
    '(define-key python-mode-map "\C-cx" 'jedi-direx:pop-to-buffer))
  (add-hook 'jedi-mode-hook 'jedi-direx:setup)

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

;; set transparency 
(defun transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))

(transparency '90)
(global-set-key (kbd "C-c t") 'transparency)

(defun djcb-opacity-modify (&optional dec)
  "modify the transparency of the emacs frame; if DEC is t,
    decrease the transparency, otherwise increase it in 10%-steps"
  (let* ((alpha-or-nil (frame-parameter nil 'alpha)) ; nil before setting
          (oldalpha (if alpha-or-nil alpha-or-nil 100))
          (newalpha (if dec (- oldalpha 10) (+ oldalpha 10))))
    (when (and (>= newalpha frame-alpha-lower-limit) (<= newalpha 100))
      (modify-frame-parameters nil (list (cons 'alpha newalpha))))))

 ;; C-8 will increase opacity (== decrease transparency)
 ;; C-9 will decrease opacity (== increase transparency)
 ;; C-0 will returns the state to normal
(global-set-key (kbd "C-8") '(lambda()(interactive)(djcb-opacity-modify)))
(global-set-key (kbd "C-9") '(lambda()(interactive)(djcb-opacity-modify t)))
(global-set-key (kbd "C-0") '(lambda()(interactive)
                               (modify-frame-parameters nil `((alpha . 100)))))


;; save history between emacs sessions
(savehist-mode 1)

;; save cursor position in file
(setq-default save-place t)
(require 'saveplace)


  ;; -------------------- extra nice things --------------------
  ;; use shift to move around windows
  (windmove-default-keybindings 'shift)
  ;(show-paren-mode t)
  
  (setq visible-bell nil) ; Turn beep off

  (setq user-mail-address "pop@paulperry.net")

  ;; Are we on a mac?
  (defun system-is-mac ()
    (interactive)
    (string-equal system-type "darwin"))

  (defun system-is-linux ()
    (interactive)
    (string-equal system-type "gnu/linux"))

  (when (memq window-system '(mac ns))
    (install-if-needed 'exec-path-from-shell)
    (require 'exec-path-from-shell)
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-env "PYTHONPATH")
    )

  (let ((path (shell-command-to-string ". ~/.bashrc; echo -n $PATH")))
    (setenv "PATH" path)
    (setq exec-path 
	  (append
	   (split-string-and-unquote path ":")
	   exec-path)))

  ;; Setup environment variables from the user's shell.
  ;; (when is-mac
  ;;   (require 'exec-path-from-shell)
  ;;   (exec-path-from-shell-initialize))

  (setq default-directory (concat (getenv "HOME") "/"))

  ;; Set standard indent to 2 rather that 4 
  (setq standard-indent 4)

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
 '(py-pychecker-command "pychecker")
 '(py-pychecker-command-args (quote ("")))
 '(python-check-command "pychecker")
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(set-cursor-color "red")
 )

(setq magit-last-seen-setup-instructions "1.4.0")
