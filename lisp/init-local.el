;;**************************************
;; Determine OS environment
;;**************************************
(defvar system-type-as-string (prin1-to-string system-type))
(defvar on_windows (string-match "windows-nt" system-type-as-string))
(defvar on_mac     (string-match "darwin" system-type-as-string))
(defvar on_linux   (string-match "gnu/linux" system-type-as-string))
(defvar on_cygwin  (string-match "cygwin" system-type-as-string))
(defvar on_solaris (string-match "usg-unix-v" system-type-as-string))

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
     (define-key swiper-map (kbd "C-.")
       (lambda ()
         (interactive)
         (insert
          (format
           "\\<%s\\>"
           (with-ivy-window
             (thing-at-point 'symbol))))))
     (define-key swiper-map (kbd "M-.")
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
(setq projectile-keymap-prefix (kbd "C-c C-p"))
(setq projectile-rails-keymap-prefix (kbd "C-c C-p r"))
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
(cond (on_mac
       (setq mac-command-modifier 'meta) ; sets the Command key as Meta
       (setq mac-option-modifier 'super) ; make opt key do Super
       (setq ns-function-modifier 'control)
       ;; (setq ns-function-modifier 'hyper)  ; make Fn key do Hyper
       ))

;; start git difftool
(defun gdt ()
  "start difftool"
  (interactive)
  (shell-command "git difftool")
  )

(provide 'init-local)
