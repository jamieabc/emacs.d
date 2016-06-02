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
(setq ivy-use-virtual-buffers t)
(global-set-key (kbd "C-c s") 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "C-c b") 'ivy-switch-buffer)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x C-m") 'counsel-M-x)

(provide 'init-local)
