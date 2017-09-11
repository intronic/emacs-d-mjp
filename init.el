;; http://melpa.org/#/getting-started
(require 'package) ;; You might already have this line

;;(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
;;                    (not (gnutls-available-p))))
;;       (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
;;  (add-to-list 'package-archives (cons "melpa" url) t))
;;(when (< emacs-major-version 24)
;;  ;; For important compatibility libraries like cl-lib
;;  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
;;(package-initialize) ;; You might already have this line


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


;; 100 MB
(setq large-file-warning-threshold (* 100 1000 1000))

;; recent files
(setq recentf-max-saved-items 256
      recentf-max-menu-items 16)
(recentf-mode)

;; set auto revert of buffers if file is changed externally
(global-auto-revert-mode)

;; Solarized
;; https://github.com/sellout/emacs-color-theme-solarized/pull/187
(setq color-themes '())
(use-package color-theme-solarized
  :config
  (customize-set-variable 'frame-background-mode 'dark)
  (load-theme 'solarized t))

;; This can set the theme light if using windows and dark if in terminal
;; (add-hook 'after-make-frame-functions
;;           (lambda (frame)
;;             (let ((mode (if (display-graphic-p frame) 'light 'dark)))
;;               (set-frame-parameter frame 'background-mode mode)
;;               (set-terminal-parameter frame 'background-mode mode))
;;             (enable-theme 'solarized)))

;; https://github.com/purcell/exec-path-from-shell
;;(when (memq window-system '(mac ns x))
;;  (exec-path-from-shell-initialize))

;; save point in buffer
(save-place-mode 1)

;; y/n for yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

;; save kill ring
(use-package savekill)

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
  :ensure t
  :init (global-flycheck-mode))

;; company "complete anything" https://github.com/andschwa/.emacs.d/blob/master/init.el
(use-package company
  :diminish company-mode
  :commands (company-complete company-mode)
  :init
  (add-hook 'after-init-hook 'global-company-mode))

;; https://github.com/jyp/dante
(use-package dante
  :ensure t
  :after haskell-mode
  :commands 'dante-mode
  :init
  (add-hook 'haskell-mode-hook 'dante-mode)
  (add-hook 'haskell-mode-hook 'flycheck-mode))

;; add this when haskell-hlint sorted
;; (add-hook 'dante-mode-hook
;;    '(lambda () (flycheck-add-next-checker 'haskell-dante
;;                 '(warning . haskell-hlint))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(frame-background-mode (quote dark))
 '(package-selected-packages
   (quote
    (fortune-cookie savekill company dante flycheck use-package f color-theme-solarized))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
