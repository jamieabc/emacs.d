;;; set default font and size
(add-to-list 'default-frame-alist '(font . "Source Code Pro-14"))

;;; enable line number mode
(global-linum-mode t)

;;; avy
(global-set-key (kbd "C-:") 'avy-goto-char)
(global-set-key (kbd "C-x C-;") 'avy-goto-line)

;;; swiper
(global-set-key (kbd "C-c r") 'ivy-resume)
(global-set-key (kbd "C-c b") 'ivy-switch-buffer)
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
(setq projectile-keymap-prefix (kbd "C-c C-p"))
(projectile-global-mode)
(setq projectile-completion-system 'ivy)

(provide 'init-local)
