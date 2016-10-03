;;;  redmine related functions start
(defun get-ticket-number ()
  "Get ticket number from each line."
  (interactive)
  (setq line-start-point (line-beginning-position))
  (setq line-end-point (line-end-position))
  (setq current-line (buffer-substring-no-properties line-start-point line-end-point))
  (setq ticket-number-end-point (string-match "[ ]+" current-line))
  (buffer-substring-no-properties (+ line-start-point 1) (+ line-start-point ticket-number-end-point))
  )

(defun redmine ()
  "Open my redmine tickets."
  (interactive)
  (if (get-buffer "redmine")
      (kill-buffer "redmine"))
  (with-current-buffer
      (get-buffer-create "redmine")
    (insert (shell-command-to-string "redmine i -m")))
  (switch-to-buffer "redmine")
  (delete-trailing-whitespace)
  (setq buffer-read-only t)
  (local-set-key (kbd "o") 'redmine-open-issue)
  (local-set-key (kbd "d") 'redmine-develop-issue)
  (local-set-key (kbd "r") 'redmine-resolve-issue)
  (local-set-key (kbd "g") 'redmine)
  (local-set-key (kbd "s") 'redmine-add-subtask)
  (local-set-key (kbd "c") 'redmine-add-task)
  (local-set-key (kbd "q") 'redmine-kill-buffer)
  (local-set-key (kbd "C") 'redmine-close-issue)
  )

(defun lredmine ()
  "Redmine custom query."
  (interactive)
  (if (get-buffer "redmine")
      (kill-buffer "redmine"))
  (with-current-buffer
      (get-buffer-create "redmine")
    (insert (shell-command-to-string "redmine l -q 60")))
  (switch-to-buffer "redmine")
  (delete-trailing-whitespace)
  (setq buffer-read-only t)
  (local-set-key (kbd "o") 'redmine-open-issue)
  (local-set-key (kbd "d") 'redmine-develop-issue)
  (local-set-key (kbd "r") 'redmine-resolve-issue)
  (local-set-key (kbd "g") 'redmine)
  (local-set-key (kbd "s") 'redmine-add-subtask)
  (local-set-key (kbd "c") 'redmine-add-task)
  (local-set-key (kbd "q") 'redmine-kill-buffer)
  (local-set-key (kbd "C") 'redmine-close-issue)
  )

(defun redmine-kill-buffer ()
  "Delete redmine buffer"
  (interactive)
  (kill-buffer "redmine")
  )

(defun redmine-add-subtask (subject)
  "Create subtask under current ticket"
  (interactive "sPlease enter subject:")
  (shell-command (format "redmine ci -a 72 -t Task -p %s 1 '%s'" (get-ticket-number) subject))
  )

(defun redmine-add-task (subject)
  "Create redmine task"
  (shell-command (format "redmine ci -a 72 -t Task 1 '%s'" subject))
  )

(defun redmine-open-issue ()
  "Open redmine issue"
  (interactive)
  (shell-command (format "redmine open %s" (get-ticket-number)))
  )

(defun redmine-develop-issue (yes-or-no)
  "Develop redmine issue"
  (interactive "sDevelop this issue?")
  (if (equal yes-or-no "y")
      (shell-command (format "redmine ui -a 72 -s 'In Progress' %s" (get-ticket-number))))
  )

(defun redmine-resolve-issue (yes-or-no)
  "Resolve redmine issue"
  (interactive "sResolve this issue?")
  (if (equal yes-or-no "y")
      (shell-command (format "redmine ui -a 72 -r 100 -s Resolved %s" (get-ticket-number))))
  )

(defun redmine-close-issue (yes-or-no)
  "Close redmine issue"
  (interactive "sClose this issue?")
  (if (equal yes-or-no "y")
      (shell-command (format "redmine ui -a 72 -r 100 -s Closed %s" (get-ticket-number))))
  )
;;; redmine related functions end

;;; set default font and size
(add-to-list 'default-frame-alist '(font . "Source Code Pro-16"))

;;; enable line number mode
(global-linum-mode t)

;;; avy
(require-package 'avy)
(global-set-key (kbd "C-:") 'avy-goto-char)
(global-set-key (kbd "C-x C-;") 'avy-goto-line)

;;; swiper start
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
;;; swiper end

;;; rails
(eval-after-load 'rinari
  '(progn (setq rinari-tags-file-name "GTAGS"))
  )

;;; rvm
(require-package 'rvm)
(rvm-use-default)

;;; ggtags start
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

;;; ggtags end

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

;;; select words in quote
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

;;; ido-vertical-mode
(require-package 'ido-vertical-mode)
(ido-vertical-mode t)
(setq ido-vertical-show-count t)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)
(global-set-key (kbd "C-,") 'imenu)

;;; find file in project start
(require-package 'find-file-in-project)
(defun my-setup-find-file-in-project ()
  (interactive)
  ;; interested filetypes
  (setq-local ffip-patterns '("*.rb" "*.js" "*.yml" "*.css" "*.scss" "*.xml" "*.tmpl" "*.json" "*.md" "*.lock" "*.sh" "*.example" "*.txt" "*.el" ""))
  ;; exclude below directories and files
  (setq-local ffip-prune-patterns '("*/.git/*" "*/node_modules/*" "*/dist/*"))
  )
(add-hook 'prog-mode-hook 'my-setup-find-file-in-project)
(add-hook 'markdown-mode-hook 'my-setup-find-file-in-project)
(global-set-key (kbd "C-c C-p f") 'find-file-in-project)
(global-set-key (kbd "C-c C-p d") 'find-file-in-current-directory)
(global-set-key (kbd "C-c C-p i") 'ffip-show-diff)
;;; find file in project end

;;; comment whole line or add tail
(defun comment-whole-line-or-add-tail (&optional arg)
  "Comment the current line with region selected or at the beginning of line,
    otherwise, add comment at tail."
  "Replacement for the comment-dwim command.
   If no region is selected and current line is not blank and we are not at
   the end of the line, then comment current line.
   Replaces default behaviour of comment-dwim, when it inserts comment at the
   end of the line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg))
  )
(global-set-key (kbd "M-;") 'comment-whole-line-or-add-tail)

;;; key frequency start
(require-package 'keyfreq)
(setq keyfreq-excluded-commands
      '(self-insert-command
        abort-recursive-edit
        avy-goto-line
        avy-goto-char
        avy-goto-word-or-subword-1
        backward-char
        backward-kill-word
        backward-word
        browse-kill-ring-insert-and-quit
        browse-kill-ring-forward
        browse-kill-ring-quit
        clipboard-kill-ring-save
        company-complete-common
        company-complete-number
        company-complete-selection
        company-ignore
        comint-send-input
        comint-previous-input
        delete-backward-char
        describe-variable
        erase-message-buffer
        eval-buffer
        exit-minibuffer
        ffip
        forward-char
        forward-word
        my-setup-develop-environment
        gnus
        gnus-summary-next-page
        gnus-summary-scroll-up
        gnus-topic-select-group
        gnus-summary-exit
        goto-line
        pwd
        ido-complete
        ido-delete-backward-updir
        ido-exit-minibuffer
        ido-switch-buffer
        indent-new-comment-line
        imenus
        isearch-abort
        isearch-other-meta-char
        isearch-other-control-char
        isearch-backward-regexp
        isearch-cancel
        isearch-delete-char
        isearch-exit
        isearch-forward-regexp
        isearch-printing-char
        isearch-repeat-forward
        isearch-ring-retreat
        ispell-minor-check
        js-mode
        js2-line-break
        keyboard-escape-quit
        keyboard-quit
        keyfreq-mode
        keyfreq-save-now
        keyfreq-show
        kill-sentence
        left-char
        minibuffer-complete
        minibuffer-complete-and-exit
        minibuffer-keyboard-quit
        move-beginning-of-line
        move-end-of-line
        mwheel-scroll
        newline-and-indent
        next-history-element
        next-line
        hippie-expand
        org-beginning-of-line
        org-ctrl-c-ctrl-c
        org-cycle
        org-end-of-line
        org-force-self-insert
        org-return
        org-self-insert-command
        org-todo
        package-menu-execute
        paredit-doublequote
        paredit-backward-delete
        paredit-backward-kill-word
        paredit-close-round
        paredit-newline
        paredit-open-round
        paredit-semicolon
        pcomplete
        previous-history-element
        previous-line
        push-button
        quit-window
        right-char
        save-buffer
        save-buffers-kill-terminal
        web-mode
        web-mode-jshint
        web-mode-test
        web-mode-reload
        web-mode-reveal
        web-mode-complete
        web-mode-navigate
        web-mode-surround
        web-mode-tag-beginning
        web-mode-part-beginning
        scroll-down-command
        scroll-up-command
        select-window-0
        select-window-1
        select-window-2
        select-window-3
        select-window-4
        select-window-5
        select-window-6
        select-window-7
        select-window-8
        select-window-9
        self-insert-command
        smarter-move-beginning-of-line
        smex
        suspend-frame
        swiper
        term-send-raw
        turnon-keyfreq-mode
        undefined ;; lambda function
        undo-tree-redo
        undo-tree-undo
        w3m-goto-url
        w3m-next-anchor
        w3m-view-this-url
        yas-compile-directory
        yas-expand
        yas-next-field-or-maybe-expand
        yank))
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)
;;; key frequency end

;;; turn off linum-mode when file over 2000 lines
(add-hook 'prog-mode-hook
          (lambda ()
            (if (and (> (buffer-size)
                        (* 2000 80)))
                (linum-mode -1))))

;;; git-timemachine start
(require-package 'git-timemachine)
(defun my-git-timemachine-show-selected-revision ()
  "Show last (current) revision of file."
  (interactive)
  (let (collection)
    (setq collection
          (mapcar (lambda (rev)
                    ;; re-shape list for the ivy-read
                    (cons (concat (substring (nth 0 rev) 0 7) "|" (nth 5 rev) "|" (nth 6 rev)) rev))
                  (git-timemachine--revisions)))
    (ivy-read "commits:"
              collection
              :action (lambda (rev)
                        (git-timemachine-show-revision rev)))))

(defun my-git-timemachine ()
  "Open git snapshot with the selected version.  Based on ivy-mode."
  (interactive)
  (unless (featurep 'git-timemachine)
    (require 'git-timemachine))
  (git-timemachine--start #'my-git-timemachine-show-selected-revision))
;;; git-timemachine end

;;; yasnippet start
(require-package 'yasnippet)
(yas-global-mode 1)
;;; yasnippet end

;;; go to last change
(global-set-key (kbd "C-x C-\\") 'session-jump-to-last-change)

(provide 'init-local)
;;; init-local.el ends here
