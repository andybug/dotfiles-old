;; initial emacs configuration
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(tool-bar-mode 0)
(scroll-bar-mode 0)
(menu-bar-mode 0)
(setq inhibit-startup-screen t)
(setq initial-major-mode 'text-mode)
(setq initial-scratch-message (concat "\
#   ___ _ __ ___   __ _  ___ ___
#  / _ \\ '_ ` _ \\ / _` |/ __/ __|
# |  __/ | | | | | (_| | (__\\__ \\
#  \\___|_| |_| |_|\\__,_|\\___|___/
#
# emacs started " (current-time-string) "
# let's do this shit
"))

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

;; require packages
(add-to-list 'load-path "~/.emacs.d/monokai-mode-line")

(setq evil-want-C-u-scroll t)
(setq evil-magit-state 'motion)

(require 'auto-complete)
(require 'base16-monokai-theme)
(require 'clojure-mode)
(require 'diff-hl)
(require 'evil)
(require 'evil-magit)
(require 'fill-column-indicator)
(require 'flx)
(require 'flx-ido)
(require 'flycheck)
(require 'hlinum)
(require 'indent-guide)
(require 'magit)
(require 'monokai-mode-line)
(require 'powerline)
(require 'rainbow-delimiters)
(require 'smooth-scrolling)

(when (eq system-type 'darwin)
  (set-face-attribute 'default nil :family "mononoki")
  ;; default font size (point * 10)
  (set-face-attribute 'default nil :height 140)
)

(load-theme 'base16-monokai t)
(monokai-mode-line)

(global-diff-hl-mode)
(global-hl-line-mode 1)
(hlinum-activate)
(column-number-mode 1)
(show-paren-mode 1)
(smooth-scrolling-mode 1)

(setq backup-inhibited t)
(setq auto-save-default nil)

(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)

(setq-default fill-column 80)
(setq diff-switches '("-u"))

(put 'erase-buffer 'disabled nil)

;; create new vertical split and switch
(defun split-window-right-other ()
  (interactive)
  (split-window-right)
  (other-window 1))

(global-unset-key (kbd "C-x C-c")) ;; close
(global-unset-key (kbd "C-x C-d")) ;; list directory
(global-unset-key (kbd "C-x d"))   ;; dired
(global-unset-key (kbd "C-w"))     ;; kill-region

(global-set-key (kbd "C-x C-d") 'dired)
(global-set-key (kbd "C-x C-b") 'buffer-menu)
(global-set-key (kbd "C-x g") 'magit-status)

(windmove-default-keybindings)
(global-set-key (kbd "C-w h")   'windmove-left)
(global-set-key (kbd "C-w j")   'windmove-down)
(global-set-key (kbd "C-w k")   'windmove-up)
(global-set-key (kbd "C-w l")   'windmove-right)
(global-set-key (kbd "C-w o")   'other-window)
(global-set-key (kbd "C-w C-w") 'other-window)
(global-set-key (kbd "C-w 0")   'delete-window)
(global-set-key (kbd "C-w 1")   'delete-other-windows)
(global-set-key (kbd "C-w 2")   'split-window-below)
(global-set-key (kbd "C-w 3")   'split-window-right)
(global-set-key (kbd "C-w 4")   'split-window-right-other)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; org-mode
(setq org-log-done 'time)
(setq org-use-tag-inheritance '0)
(setq org-todo-keywords '((type "TODO" "STARTED" "WAITING" "|" "DONE" "CANCELED")))
(global-set-key "\C-ca" 'org-agenda)

;; use linux-style indenting by default in c-mode
(setq c-default-style "linux")

;; treat _ as word char in c-mode and python-mode
(add-hook 'c-mode-common-hook '(lambda () (modify-syntax-entry ?_ "w")))
(add-hook 'python-mode-hook '(lambda () (modify-syntax-entry ?_ "w")))

;; disable line highlighting in certain modes
(add-hook 'term-mode-hook (lambda () (setq-local global-hl-line-mode nil)))
(add-hook 'erc-mode-hook (lambda () (setq-local global-hl-line-mode nil)))

;; setup clojure-mode
(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook #'indent-guide-mode)
(add-hook 'clojure-mode-hook #'show-paren-mode)
(add-hook 'clojure-mode-hook '(lambda () (modify-syntax-entry ?- "w")))

;; setup java-mode
(add-hook 'java-mode-hook (lambda () (c-set-style "java")))
(add-hook 'java-mode-hook (lambda () (setq c-basic-offset 4)))
(add-hook 'java-mode-hook (lambda () (setq indent-tabs-mode nil)))

;; setup javascript-mode
(setq js-indent-level 2)

;; disable linum-mode for certain major modes
(define-global-minor-mode my-global-linum-mode linum-mode
  (lambda ()
    (when (not (memq major-mode
		     (list 'term-mode)))
      (linum-mode 1))))

(my-global-linum-mode 1)


(evil-mode 1)
