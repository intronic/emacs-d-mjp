;;; package --- Init script
;;; Commentary:
;;; Code:

;; http://melpa.org/#/getting-started
(require 'package) ;; You might already have this line

(setq load-prefer-newer t
      package-enable-at-startup nil
      package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("org" . "http://orgmode.org/elpa/")
        ("melpa" . "https://melpa.org/packages/")))
(package-initialize) ;; You might already have this line

;; init stuff taken from: https://github.com/andschwa/.emacs.d/blob/master/init.el

;; https://github.com/jwiegley/use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-verbose t
      use-package-always-ensure t)

(eval-when-compile
  (require 'use-package))

;; 100 MB
(setq large-file-warning-threshold (* 100 1000 1000))

(desktop-save-mode 1)
(set-face-font 'default "Hack-11")

;; set auto revert of buffers if file is changed externally
(global-auto-revert-mode)


;; recent files
(setq recentf-max-saved-items 256
      recentf-max-menu-items 16)
(recentf-mode)

;; packages used in init
(use-package dash)
(use-package f)

;;; system specific packages
;; load Linux configuration
(use-package linux
  :ensure nil
  :load-path "lisp/"
  :if (eq system-type 'gnu/linux))

;; load OS X configurations
(use-package osx
  :ensure nil
  :load-path "lisp/"
  :if (eq system-type 'darwin))

;; load Windows configurations
(use-package windows
  :ensure nil
  :load-path "lisp/"
  :if (eq system-type 'windows-nt))

;; Solarized
;; https://github.com/sellout/emacs-color-theme-solarized/pull/187
(setq color-themes '())
(use-package color-theme-solarized
  :config
  (load-theme 'solarized t)
  :init
  (add-hook 'after-make-frame-functions
      (lambda (frame)
        (let ((mode (if (display-graphic-p frame) 'light 'dark))
              (set-frame-parameter frame 'background-mode mode)
              (set-terminal-parameter frame 'background-mode mode))
          (enable-theme 'solarized)))))

;; This can set the theme light if using windows and dark if in terminal

;; https://github.com/purcell/exec-path-from-shell
;;(when (memq window-system '(mac ns x))
;;  (exec-path-from-shell-initialize))

;; save point in buffer
(save-place-mode 1)

;; y/n for yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

;; save kill ring
(use-package savekill)

;; tabs are truly evil
(setq-default indent-tabs-mode nil)

;; fortune
(setq inhibit-startup-screen t)
(use-package fortune-cookie
  :config
  (setq fortune-cookie-fortune-args " -c "
        fortune-cookie-cowsay-enable t
        fortune-cookie-cowsay-args " -f tux -s")
  (fortune-cookie-mode))

;; http://www.flycheck.org/
(use-package flycheck
  :init (global-flycheck-mode))

;; company "complete anything" https://github.com/andschwa/.emacs.d/blob/master/init.el
(use-package company
  :diminish company-mode
  :commands (company-complete company-mode)
  :init
  (add-hook 'after-init-hook 'global-company-mode))

;; skip dante, use intero, but use intero inside nix:-
;; https://github.com/NixOS/nixpkgs/issues/21495

;; https://github.com/jyp/dante
(use-package dante
  :after haskell-mode
  :commands 'dante-mode
  :init
  (add-hook 'haskell-mode-hook 'dante-mode)
  (add-hook 'haskell-mode-hook 'flycheck-mode))

;; add this when haskell-hlint sorted
;; (add-hook 'dante-mode-hook
;;    '(lambda () (flycheck-add-next-checker 'haskell-dante
;;                 '(warning . haskell-hlint))))


;; https://github.com/chrisdone/structured-haskell-mode
;; 1. go to elisp dir in the git repo, and 'make all'
;; 2. install haskell binaries with nix:
;; nix-env -f "<nixpkgs>" -i -A haskellPackages.structured-haskell-mode/src/structured-haskell-mode/elisp
;; nix-env -f "<nixpkgs>" -i -A haskellPackages.hindent
(use-package shm
  :after haskell-mode
  :load-path "../src/structured-haskell-mode/elisp/"
  :init
  (add-hook 'haskell-mode-hook 'structured-haskell-mode))

(use-package magit
  :bind ("C-x g" . magit-status))

;; paredit for parinfer
(use-package paredit)

(use-package parinfer
  :bind
  (("C-," . parinfer-toggle-mode))
  :init
  (progn
    (setq parinfer-extensions
     '(defaults       ; should be included.
       pretty-parens  ; different paren styles for different modes.
       ;;evil           ; If you use Evil.
       ;;lispy          ; If you use Lispy. With this extension, you should install Lispy and do not enable lispy-mode directly.
       paredit        ; Introduce some paredit commands.
       smart-tab      ; C-b & C-f jump positions and smart shift with tab & S-tab.
       smart-yank))   ; Yank behavior depend on mode.
    (add-hook 'clojure-mode-hook #'parinfer-mode)
    (add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
    (add-hook 'common-lisp-mode-hook #'parinfer-mode)
    (add-hook 'scheme-mode-hook #'parinfer-mode)
    (add-hook 'lisp-mode-hook #'parinfer-mode)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(frame-background-mode (quote dark))
 '(package-selected-packages
   (quote
    (paredit parinfer magit fortune-cookie savekill company dante flycheck use-package f color-theme-solarized))))
(custom-set-faces)
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 

(provide 'init)
;;; init.el ends here
