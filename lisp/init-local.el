;;; set default font and size
(add-to-list 'default-frame-alist '(font . "Source Code Pro-14"))

;;; enable line number mode
(global-linum-mode t)

;;; avy
(require-package 'avy)
(global-set-key (kbd "C-:") 'avy-goto-char)
(global-set-key (kbd "C-x C-;") 'avy-goto-line)

;;; swiper
(require-package 'swiper)
(global-set-key (kbd "C-c r") 'ivy-resume)
(global-set-key (kbd "C-x b") 'ivy-switch-buffer)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x C-m") 'counsel-M-x)
(global-set-key (kbd "C-s") 'swiper)
(eval-after-load 'swiper
  '(progn
     (define-key swiper-map (kbd "M-.")
       (lambda ()
         (interactive)
         (insert
          (format
           "\\<%s\\>"
           (with-ivy-window
             (thing-at-point 'symbol))))))
     (define-key swiper-map (kbd "C-.")
       (lambda ()
         (interactive)
         (insert
          (format
           "\\<%s\\>"
           (with-ivy-window
             (thing-at-point 'word))))))
     (setq ivy-height 10)
     (setq ivy-use-virtual-buffers t)
     (setq ivy-count-format "(%d/%d) ")
     (setq ivy-display-style 'fancy)
     (setq enable-recursive-minibuffers t)
     ))

;;; projectile
(require-package 'projectile)
(require-package 'projectile-rails)
(add-hook 'projectile-mode-hook 'projectile-rails-on)
(setq projectile-rails-keymap-prefix (kbd "C-c C-p C-r"))
(setq projectile-keymap-prefix (kbd "C-c C-p"))
(projectile-global-mode)
(setq projectile-completion-system 'ivy)

;;; rails
(eval-after-load 'rinari
  '(progn (setq rinari-tags-file-name "GTAGS"))
  )

;;; rvm
(require-package 'rvm)
(rvm-use-default)

;;; ggtags
(require-package 'ggtags)
(add-hook 'prog-mode-hook
          '(lambda ()
             (when (derived-mode-p 'ruby-mode 'js2-mode)
               (ggtags-mode 1))))
(eval-after-load 'ggtags
  '(progn
     (define-key ggtags-mode-map (kbd "M-,") 'pop-tag-mark)
     (define-key ggtags-mode-map (kbd "M-.") 'ggtags-find-tag-dwim)
     (define-key ggtags-mode-map (kbd "C-c g s") 'ggtags-find-other-symbol)
     (define-key ggtags-mode-map (kbd "C-c g h") 'ggtags-view-tag-history)
     (define-key ggtags-mode-map (kbd "C-c g r") 'ggtags-find-reference)
     (define-key ggtags-mode-map (kbd "C-c g f") 'ggtags-find-file)
     ))

;;; Mac keybindings
(when *is-a-mac*
  (setq mac-option-modifier 'super) ; make opt key do Super
  (setq ns-function-modifier 'control)
  ;; (setq ns-function-modifier 'hyper)  ; make Fn key do Hyper
  )

;; start git difftool
(defun gdt ()
  "start difftool"
  (interactive)
  (shell-command "git difftool")
  )

(defun my-select-word-in-quote ()
  "Select text between nearest left and right delimiters."
  (interactive)
  (let (p1 p2 (skipChars "^\"<>(){}[]\'"))
    (skip-chars-backward skipChars)
    (setq p1 (point))
    (skip-chars-forward skipChars)
    (setq p2 (point))
    (set-mark p1)
    )
  )
(global-set-key (kbd "C-c s q") 'my-select-word-in-quote)

;;; rspec
(require-package 'rspec-mode)
(add-hook 'ruby-mode-hook 'rspec-mode)
(add-hook 'after-init-hook 'inf-ruby-switch-setup)
(eval-after-load 'rspec-mode
  '(progn
     (setq rspec-command-options "--fail-fast --color")
     ))

;;; window number
(require-package 'window-numbering)
(window-numbering-mode)

(provide 'init-local)
