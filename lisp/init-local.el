;;; set default font and size
(add-to-list 'default-frame-alist '(font . "Source Code Pro-14"))

;;; enable line number mode
(global-linum-mode t)

;;; avy
(global-set-key (kbd "C-:") 'avy-goto-char)
(global-set-key (kbd "C-x C-;") 'avy-goto-line)

;;; projectile
(setq projectile-keymap-prefix (kbd "C-c C-p"))
(projectile-global-mode)
(setq projectile-completion-system 'ivy)

;;; swiper
(ivy-mode 1)
(require 'swiper)
(setq ivy-use-virtual-buffers )
(setq ivy-count-format "(%d/%d) ")
(setq ivy-display-style 'fancy)
(setq enable-recursive-minibuffers t)
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "C-c r") 'ivy-resume)
(global-set-key (kbd "C-c b") 'ivy-switch-buffer)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x C-m") 'counsel-M-x)
(define-key swiper-map (kbd "C-.")
  (lambda () (interactive) (insert (format "\\<%s\\>" (with-ivy-window (thing-at-point 'symbol))))))
(define-key swiper-map (kbd "M-.")
  (lambda () (interactive) (insert (format "\\<%s\\>" (with-ivy-window (thing-at-point 'word))))))

(provide 'init-local)
