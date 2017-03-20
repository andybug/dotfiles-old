;; initial emacs configuration  -*- indent-tabs-mode: nil; -*-

(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(tool-bar-mode 0)
(scroll-bar-mode 0)
(menu-bar-mode 0)
(setq inhibit-startup-screen t)
(setq initial-major-mode 'text-mode)
(setq initial-scratch-message "\

                                     `.--:-`
                                -:-++/-...-+o+-
                             ..oyo-       `/o+-
                            /yy:-         :yy/
                          `-ys`    `:/:-://h/-.
                         `os.`  `/+:.::.` :/+ody:
                          o:  `++`-///o       `:+o.`
                          s  :o/+o+oyy/            `...`
                     .:///+`+syssdyy:oy+`      ```   `.-`
                .:+oso+:::.`+h:`h:ss` s+s.              ..
             -+so/-.``-:o/`-s/``/y-:osh+.                `-.`
          -+s+-` .-+oo+//s -ho` `/s+:-`                    `:.
        :o++:..---.`    o- `h//::`             `  ````````:-`
      :o+o-``          `y   -o       ``````             --`
     +y+-              :+    :o://///:::-::/+:`      .:-`
                       y`      ``           /o`-----:+`
                      :+                     s+
                     `y`                    `+dy`
                     s-                      `hdd/
                    :s     `                   oo
                   `y:     `                --  h.
                   +y.                      .ho h`
                  `s:   `                    .hyy
                  ++    `                  `` .h`
                 -y:    `                   /: d`
                 +o                         .d+h
                .d/-.``-```.-               /mh:
                  .-//+/++/:h-              -ds
                            s:              .d`
                            o/              ++

                             _|            _|
     _|_|_|  _|_|_|      _|_|_|  _|    _|  _|_|_|    _|    _|    _|_|_|
   _|    _|  _|    _|  _|    _|  _|    _|  _|    _|  _|    _|  _|    _|
   _|    _|  _|    _|  _|    _|  _|    _|  _|    _|  _|    _|  _|    _|
     _|_|_|  _|    _|    _|_|_|    _|_|_|  _|_|_|      _|_|_|    _|_|_|
                                       _|                            _|
                                   _|_|                          _|_|
")

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
(require 'key-chord)
(require 'magit)
(require 'monokai-mode-line)
(require 'powerline)
(require 'rainbow-delimiters)
(require 'smooth-scrolling)

(when (eq system-type 'darwin)
  (require 'exec-path-from-shell)
  (set-face-attribute 'default nil :family "mononoki")
  ;; default font size (point * 10)
  (set-face-attribute 'default nil :height 140)
  (exec-path-from-shell-initialize)
  ;; setup dired for mac
  (setq insert-directory-program "/usr/local/bin/gls")
  (setq dired-listing-switches "-al --group-directories-first")
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

;; jump to the file at the top of the org-agenda-files list
(defun switch-to-top-org-agenda-file ()
  (interactive)
  (let ((agenda-file (car org-agenda-files)))
    (if agenda-file
        (let ((buffer (get-file-buffer agenda-file)))
          (if buffer
              (switch-to-buffer buffer)
            (message (format "could not find %s" agenda-file))))
      (message "no agenda file"))))

;; jump to the notes file; open it if not already
(defun switch-to-org-notes-file ()
  (interactive)
  (find-file org-default-notes-file))

;; jump to andybug-term; create if it doesn't exist
(defun switch-to-andybug-term ()
  (interactive)
  ;;(require 'term)
  (let ((buffer (get-buffer "*andybug-term*")))
    (if buffer
        (switch-to-buffer buffer)
      (ansi-term "/bin/zsh" "andybug-term"))))


(global-unset-key (kbd "C-x C-c")) ;; close
(global-unset-key (kbd "C-x C-d")) ;; list directory
(global-unset-key (kbd "C-x d"))   ;; dired
(global-unset-key (kbd "C-w"))     ;; kill-region

;; global key bindings
(global-set-key (kbd "C-c W") 'whitespace-cleanup)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c f") 'find-dired)
(global-set-key (kbd "C-c g") 'magit-status)
(global-set-key (kbd "C-c n") 'switch-to-org-notes-file)
(global-set-key (kbd "C-c o") 'switch-to-top-org-agenda-file)
(global-set-key (kbd "C-c r") 'rgrep)
(global-set-key (kbd "C-c t") 'switch-to-andybug-term)
(global-set-key (kbd "C-c w") 'whitespace-mode)
(global-set-key (kbd "C-x C-b") 'buffer-menu)
(global-set-key (kbd "C-x C-d") 'dired)

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

(let ((custom-file-path "~/.emacs.d/custom.el"))
  (setq custom-file (symbol-value 'custom-file-path))
  (if (file-exists-p (symbol-value 'custom-file-path))
    (load custom-file)
    (message "custom file '%s' does not exist" (symbol-value 'custom-file-path))))

;; org-mode
(setq org-directory "~/Documents/org")
(setq org-default-notes-file (format "%s/notes.org" org-directory))
(setq org-log-done 'time)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-use-tag-inheritance '0)
(setq org-todo-keywords '((type "TODO" "STARTED" "BLOCKED" "|" "DONE" "CANCELED")))
(setq org-columns-default-format "%80ITEM(Task) %10Effort(Estimate){:} %10CLOCKSUM(Actual)")

(setq org-capture-templates
      (quote (("t" "todo" entry (file+headline org-default-notes-file "Tasks")
               "* TODO %?\n  Created: %U\n")
              ("n" "note" entry (file+datetree org-default-notes-file)
               "* %?\n  Created: %U\n"))))

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
;; (add-hook 'java-mode-hook (lambda () (c-set-style "java")))
;; (add-hook 'java-mode-hook (lambda () (setq c-basic-offset 4)))
;; (add-hook 'java-mode-hook (lambda () (setq indent-tabs-mode nil)))

;; setup javascript-mode
(setq js-indent-level 2)

;; setup nxml-mode
(add-hook 'nxml-mode-hook (lambda () (setq nxml-child-indent 2)))
(add-hook 'nxml-mode-hook (lambda () (setq nxml-attribute-indent 2)))
(add-hook 'nxml-mode-hook (lambda () (setq indent-tabs-mode nil)))

;; setup emacs-lisp-mode
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)

;; disable linum-mode for certain major modes
(define-global-minor-mode my-global-linum-mode linum-mode
  (lambda ()
    (when (not (memq major-mode
                     (list 'term-mode)))
      (linum-mode 1))))

;; key-chord-mode
(setq key-chord-two-keys-delay 0.3)
(key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
(key-chord-mode 1)

;; diff-hl
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

(my-global-linum-mode 1)
(evil-mode 1)
