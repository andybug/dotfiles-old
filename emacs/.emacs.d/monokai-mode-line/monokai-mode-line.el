(require 'evil)
(require 'powerline)

(defcustom monokai-mode-line-tag-style 'visual-expanded
  "The style to use for displaying the evil state tag.

Valid Values: standard, verbose, visual-expanded"
  :group 'powerline
  :type '(choice (const standard)
                 (const verbose)
                 (const visual-expanded)))

(defface monokai-mode-line-base-face
  '((t (:foreground "#49483E" :inherit mode-line)))
  "Base face for powerline evil faces."
  :group 'powerline)

(defface monokai-mode-line-normal-face
  '((t (:background "#A6E22E" :inherit monokai-mode-line-base-face)))
  "Powerline face for evil NORMAL state."
  :group 'powerline)

(defface monokai-mode-line-insert-face
  '((t (:background "#FD971F" :inherit monokai-mode-line-base-face)))
  "Powerline face for evil INSERT state."
  :group 'powerline)

(defface monokai-mode-line-visual-face
  '((t (:background "#F92672" :inherit monokai-mode-line-base-face)))
  "Powerline face for evil VISUAL state."
  :group 'powerline)

(defface monokai-mode-line-operator-face
  '((t (:background "#AE81FF" :inherit monokai-mode-line-operator-face)))
  "Powerline face for evil OPERATOR state."
  :group 'powerline)

(defface monokai-mode-line-replace-face
  '((t (:background "#FD971F" :inherit monokai-mode-line-base-face)))
  "Powerline face for evil REPLACE state."
  :group 'powerline)

(defface monokai-mode-line-motion-face
  '((t (:background "#AE81FF" :inherit monokai-mode-line-base-face)))
  "Powerline face for evil MOTION state."
  :group 'powerline)

(defface monokai-mode-line-emacs-face
  '((t (:background "#66D9EF" :inherit monokai-mode-line-base-face)))
  "Powerline face for evil EMACS state."
  :group 'powerline)


;;;###autoload
(defun monokai-mode-line-face ()
  "Function to select appropriate face based on `evil-state'."
  (let* ((face (intern (concat "monokai-mode-line-" (symbol-name evil-state) "-face"))))
    (if (facep face) face nil)))

(defun monokai-mode-line-tag ()
  "Get customized tag value for current evil state."
  (let* ((visual-block (and (evil-visual-state-p)
                            (eq evil-visual-selection 'block)))
         (visual-line (and (evil-visual-state-p)
                           (eq evil-visual-selection 'line))))
    (cond ((eq monokai-mode-line-tag-style 'visual-expanded)
           (cond (visual-block " +V+ ")
                 (visual-line " -V- ")
                 (t evil-mode-line-tag)))
          ((eq monokai-mode-line-tag-style 'verbose)
           (upcase (concat (symbol-name evil-state)
                           (cond (visual-block " BLOCK")
                                 (visual-line " LINE")))))
          (t evil-mode-line-tag))))


(defun monokai-mode-line ()
  (interactive)
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (mode-line (if active 'mode-line 'mode-line-inactive))
                          (face1 (if active 'powerline-active1 'powerline-inactive1))
                          (face2 (if active 'powerline-active2 'powerline-inactive2))
                          (evil-face (monokai-mode-line-face))
                          (separator-left (intern (format "powerline-%s-%s"
							  (powerline-current-separator)
                                                          (car powerline-default-separator-dir))))
                          (separator-right (intern (format "powerline-%s-%s"
                                                           (powerline-current-separator)
                                                           (cdr powerline-default-separator-dir))))
                          (lhs (list
                                     (powerline-raw (symbol-name evil-state) evil-face 'l)
                                     (powerline-raw " " evil-face 'l)
                                     (funcall separator-left evil-face nil)
                                     (powerline-raw "%*" nil 'l)
                                     (when powerline-display-buffer-size
                                       (powerline-buffer-size nil 'l))
                                     (when powerline-display-mule-info
                                       (powerline-raw mode-line-mule-info nil 'l))
                                     (powerline-buffer-id nil 'l)
                                     (when (and (boundp 'which-func-mode) which-func-mode)
                                       (powerline-raw which-func-format nil 'l))
                                     (powerline-raw " ")
                                     (funcall separator-left mode-line face1)
                                     (when (and (boundp 'erc-track-minor-mode) erc-track-minor-mode)
                                       (powerline-raw erc-modified-channels-object face1 'l))
                                     (powerline-major-mode face1 'l)
                                     (powerline-process face1)
                                     (powerline-minor-modes face1 'l)
                                     (powerline-narrow face1 'l)
                                     (powerline-raw " " face1)
                                     (funcall separator-left face1 face2)
                                     (powerline-vc face2 'r)
                                     (when (bound-and-true-p nyan-mode)
                                       (powerline-raw (list (nyan-create)) face2 'l))))
                          (rhs (list (powerline-raw global-mode-string face2 'r)
                                     (funcall separator-right face2 face1)
				     (unless window-system
				       (powerline-raw (char-to-string #xe0a1) face1 'l))
				     (powerline-raw "%4l" face1 'l)
				     (powerline-raw ":" face1 'l)
				     (powerline-raw "%3c" face1 'r)
				     (funcall separator-right face1 mode-line)
				     (powerline-raw " ")
				     (powerline-raw "%6p" nil 'r)
                                     (when powerline-display-hud
                                       (powerline-hud face2 face1)))))
		     (concat (powerline-render lhs)
			     (powerline-fill face2 (powerline-width rhs))
			     (powerline-render rhs)))))))

(provide 'monokai-mode-line)
