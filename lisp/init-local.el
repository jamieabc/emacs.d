;;;  redmine related functions
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
  "List my redmine tickets."
  (interactive)
  (if (get-buffer "redmine")
      (kill-buffer "redmine"))
  (with-current-buffer
      (get-buffer-create "redmine")
    (insert (shell-command-to-string "redmine i -m")))
  (switch-to-buffer "redmine")
  (setq truncate-lines t)
  (delete-trailing-whitespace)
  (setq buffer-read-only t)
  (local-set-key (kbd "o") 'redmine-open-issue)
  (local-set-key (kbd "d") 'redmine-develop-issue)
  (local-set-key (kbd "r") 'redmine-resolve-issue)
  (local-set-key (kbd "g") 'redmine)
  (local-set-key (kbd "s") 'redmine-add-subtask)
  (local-set-key (kbd "v") 'redmine-add-verify-subtask)
  (local-set-key (kbd "c") 'redmine-add-task)
  (local-set-key (kbd "q") 'redmine-kill-buffer)
  (local-set-key (kbd "C") 'redmine-close-issue)
  (local-set-key (kbd "b") 'redmine-get-branch)
  )

(defun lredmine ()
  "List redmine custom query taipier f2e teams."
  (interactive)
  (if (get-buffer "redmine")
      (kill-buffer "redmine"))
  (with-current-buffer
      (get-buffer-create "redmine")
    (insert (shell-command-to-string "redmine l -q 60")))
  (switch-to-buffer "redmine")
  (setq truncate-lines t)
  (delete-trailing-whitespace)
  (setq buffer-read-only t)
  (local-set-key (kbd "o") 'redmine-open-issue)
  (local-set-key (kbd "d") 'redmine-develop-issue)
  (local-set-key (kbd "r") 'redmine-resolve-issue)
  (local-set-key (kbd "g") 'lredmine)
  (local-set-key (kbd "s") 'redmine-add-subtask)
  (local-set-key (kbd "v") 'redmine-add-verify-subtask)
  (local-set-key (kbd "c") 'redmine-add-task)
  (local-set-key (kbd "q") 'redmine-kill-buffer)
  (local-set-key (kbd "C") 'redmine-close-issue)
  )

(defun credmine ()
  "Redmine custom query my not closed tasks/bugs."
  (interactive)
  (if (get-buffer "redmine")
      (kill-buffer "redmine"))
  (with-current-buffer
      (get-buffer-create "redmine")
    (insert (shell-command-to-string "redmine l -q 210")))
  (switch-to-buffer "redmine")
  (setq truncate-lines t)
  (delete-trailing-whitespace)
  (setq buffer-read-only t)
  (local-set-key (kbd "o") 'redmine-open-issue)
  (local-set-key (kbd "d") 'redmine-develop-issue)
  (local-set-key (kbd "r") 'redmine-resolve-issue)
  (local-set-key (kbd "g") 'credmine)
  (local-set-key (kbd "s") 'redmine-add-subtask)
  (local-set-key (kbd "v") 'redmine-add-verify-subtask)
  (local-set-key (kbd "c") 'redmine-add-task)
  (local-set-key (kbd "q") 'redmine-kill-buffer)
  (local-set-key (kbd "C") 'redmine-close-issue)
  )

(defun jredmine ()
  "List jira redmine issues."
  (interactive)
  (if (get-buffer "redmine")
      (kill-buffer "redmine"))
  (with-current-buffer
      (get-buffer-create "redmine")
    (insert (shell-command-to-string "redmine l -q 214")))
  (switch-to-buffer "redmine")
  (setq truncate-lines t)
  (delete-trailing-whitespace)
  (setq buffer-read-only t)
  (local-set-key (kbd "o") 'redmine-open-issue)
  (local-set-key (kbd "d") 'redmine-develop-issue)
  (local-set-key (kbd "r") 'redmine-resolve-issue)
  (local-set-key (kbd "g") 'jredmine)
  (local-set-key (kbd "s") 'redmine-add-subtask)
  (local-set-key (kbd "v") 'redmine-add-verify-subtask)
  (local-set-key (kbd "c") 'redmine-add-task)
  (local-set-key (kbd "q") 'redmine-kill-buffer)
  (local-set-key (kbd "C") 'redmine-close-issue)
  )

(defun bredmine ()
  "List beta redmine tickets."
  (interactive)
  (if (get-buffer "redmine")
      (kill-buffer "redmine"))
  (with-current-buffer
      (get-buffer-create "redmine")
    (insert (shell-command-to-string "redmine l -q 211")))
  (switch-to-buffer "redmine")
  (setq truncate-lines t)
  (delete-trailing-whitespace)
  (setq buffer-read-only t)
  (local-set-key (kbd "o") 'redmine-open-issue)
  (local-set-key (kbd "d") 'redmine-develop-issue)
  (local-set-key (kbd "r") 'redmine-resolve-issue)
  (local-set-key (kbd "g") 'bredmine)
  (local-set-key (kbd "s") 'redmine-add-subtask)
  (local-set-key (kbd "v") 'redmine-add-verify-subtask)
  (local-set-key (kbd "c") 'redmine-add-task)
  (local-set-key (kbd "q") 'redmine-kill-buffer)
  (local-set-key (kbd "C") 'redmine-close-issue)
  (local-set-key (kbd "b") 'redmine-get-branch)
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

(defun redmine-add-verify-subtask ()
  "Create verify subtask under current ticket"
  (interactive)
  ;; (interactive "sPlease enter subject: please verify ")
  (shell-command (format "redmine ci -a 72 -t Task -p %s 1 'please verify %s'" (get-ticket-number) (get-ticket-number)))
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

(defun redmine-get-branch ()
  "Get branch naming from ticket"
  (interactive)
  (shell-command (format "i -b %s" (get-ticket-number)))
  )
;;; redmine related functions

;;; set font size
(add-to-list 'default-frame-alist '(font . "Fira Code-16"))

;;; Fira code
;; This works when using emacs --daemon + emacsclient
(add-hook 'after-make-frame-functions (lambda (frame) (set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code Symbol")))
;; This works when using emacs without server/client
(set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code Symbol")
;; I haven't found one statement that makes both of the above situations work, so I use both for now

(defconst fira-code-font-lock-keywords-alist
  (mapcar (lambda (regex-char-pair)
            `(,(car regex-char-pair)
              (0 (prog1 ()
                   (compose-region (match-beginning 1)
                                   (match-end 1)
                                   ;; The first argument to concat is a string containing a literal tab
                                   ,(concat "	" (list (decode-char 'ucs (cadr regex-char-pair)))))))))
          '(("\\(www\\)"                   #Xe100)
            ("[^/]\\(\\*\\*\\)[^/]"        #Xe101)
            ("\\(\\*\\*\\*\\)"             #Xe102)
            ("\\(\\*\\*/\\)"               #Xe103)
            ("\\(\\*>\\)"                  #Xe104)
            ("[^*]\\(\\*/\\)"              #Xe105)
            ("\\(\\\\\\\\\\)"              #Xe106)
            ("\\(\\\\\\\\\\\\\\)"          #Xe107)
            ("\\({-\\)"                    #Xe108)
            ("\\(\\[\\]\\)"                #Xe109)
            ("\\(::\\)"                    #Xe10a)
            ("\\(:::\\)"                   #Xe10b)
            ("[^=]\\(:=\\)"                #Xe10c)
            ("\\(!!\\)"                    #Xe10d)
            ("\\(!=\\)"                    #Xe10e)
            ("\\(!==\\)"                   #Xe10f)
            ("\\(-}\\)"                    #Xe110)
            ("\\(--\\)"                    #Xe111)
            ("\\(---\\)"                   #Xe112)
            ("\\(-->\\)"                   #Xe113)
            ("[^-]\\(->\\)"                #Xe114)
            ("\\(->>\\)"                   #Xe115)
            ("\\(-<\\)"                    #Xe116)
            ("\\(-<<\\)"                   #Xe117)
            ("\\(-~\\)"                    #Xe118)
            ("\\(#{\\)"                    #Xe119)
            ("\\(#\\[\\)"                  #Xe11a)
            ("\\(##\\)"                    #Xe11b)
            ("\\(###\\)"                   #Xe11c)
            ("\\(####\\)"                  #Xe11d)
            ("\\(#(\\)"                    #Xe11e)
            ("\\(#\\?\\)"                  #Xe11f)
            ("\\(#_\\)"                    #Xe120)
            ("\\(#_(\\)"                   #Xe121)
            ("\\(\\.-\\)"                  #Xe122)
            ("\\(\\.=\\)"                  #Xe123)
            ("\\(\\.\\.\\)"                #Xe124)
            ("\\(\\.\\.<\\)"               #Xe125)
            ("\\(\\.\\.\\.\\)"             #Xe126)
            ("\\(\\?=\\)"                  #Xe127)
            ("\\(\\?\\?\\)"                #Xe128)
            ("\\(;;\\)"                    #Xe129)
            ("\\(/\\*\\)"                  #Xe12a)
            ("\\(/\\*\\*\\)"               #Xe12b)
            ("\\(/=\\)"                    #Xe12c)
            ("\\(/==\\)"                   #Xe12d)
            ("\\(/>\\)"                    #Xe12e)
            ("\\(//\\)"                    #Xe12f)
            ("\\(///\\)"                   #Xe130)
            ("\\(&&\\)"                    #Xe131)
            ("\\(||\\)"                    #Xe132)
            ("\\(||=\\)"                   #Xe133)
            ("[^|]\\(|=\\)"                #Xe134)
            ("\\(|>\\)"                    #Xe135)
            ("\\(\\^=\\)"                  #Xe136)
            ("\\(\\$>\\)"                  #Xe137)
            ("\\(\\+\\+\\)"                #Xe138)
            ("\\(\\+\\+\\+\\)"             #Xe139)
            ("\\(\\+>\\)"                  #Xe13a)
            ("\\(=:=\\)"                   #Xe13b)
            ("[^!/]\\(==\\)[^>]"           #Xe13c)
            ("\\(===\\)"                   #Xe13d)
            ("\\(==>\\)"                   #Xe13e)
            ("[^=]\\(=>\\)"                #Xe13f)
            ("\\(=>>\\)"                   #Xe140)
            ("\\(<=\\)"                    #Xe141)
            ("\\(=<<\\)"                   #Xe142)
            ("\\(=/=\\)"                   #Xe143)
            ("\\(>-\\)"                    #Xe144)
            ("\\(>=\\)"                    #Xe145)
            ("\\(>=>\\)"                   #Xe146)
            ("[^-=]\\(>>\\)"               #Xe147)
            ("\\(>>-\\)"                   #Xe148)
            ("\\(>>=\\)"                   #Xe149)
            ("\\(>>>\\)"                   #Xe14a)
            ("\\(<\\*\\)"                  #Xe14b)
            ("\\(<\\*>\\)"                 #Xe14c)
            ("\\(<|\\)"                    #Xe14d)
            ("\\(<|>\\)"                   #Xe14e)
            ("\\(<\\$\\)"                  #Xe14f)
            ("\\(<\\$>\\)"                 #Xe150)
            ("\\(<!--\\)"                  #Xe151)
            ("\\(<-\\)"                    #Xe152)
            ("\\(<--\\)"                   #Xe153)
            ("\\(<->\\)"                   #Xe154)
            ("\\(<\\+\\)"                  #Xe155)
            ("\\(<\\+>\\)"                 #Xe156)
            ("\\(<=\\)"                    #Xe157)
            ("\\(<==\\)"                   #Xe158)
            ("\\(<=>\\)"                   #Xe159)
            ("\\(<=<\\)"                   #Xe15a)
            ("\\(<>\\)"                    #Xe15b)
            ("[^-=]\\(<<\\)"               #Xe15c)
            ("\\(<<-\\)"                   #Xe15d)
            ("\\(<<=\\)"                   #Xe15e)
            ("\\(<<<\\)"                   #Xe15f)
            ("\\(<~\\)"                    #Xe160)
            ("\\(<~~\\)"                   #Xe161)
            ("\\(</\\)"                    #Xe162)
            ("\\(</>\\)"                   #Xe163)
            ("\\(~@\\)"                    #Xe164)
            ("\\(~-\\)"                    #Xe165)
            ("\\(~=\\)"                    #Xe166)
            ("\\(~>\\)"                    #Xe167)
            ("[^<]\\(~~\\)"                #Xe168)
            ("\\(~~>\\)"                   #Xe169)
            ("\\(%%\\)"                    #Xe16a)
            ;; ("\\(x\\)"                   #Xe16b) This ended up being hard to do properly so i'm leaving it out.
            ("[^:=]\\(:\\)[^:=]"           #Xe16c)
            ("[^\\+<>]\\(\\+\\)[^\\+<>]"   #Xe16d)
            ("[^\\*/<>]\\(\\*\\)[^\\*/<>]" #Xe16f))))

(defun add-fira-code-symbol-keywords ()
  (font-lock-add-keywords nil fira-code-font-lock-keywords-alist))

(add-hook 'prog-mode-hook
          #'add-fira-code-symbol-keywords)

;;; enable line number mode
(global-linum-mode t)

;;; avy
(require-package 'avy)
(global-set-key (kbd "C-c C-;") 'avy-goto-char)
(global-set-key (kbd "C-x C-;") 'avy-goto-line)
;;; avy

;;; ag
(global-set-key (kbd "C-c k") 'counsel-ag)
;;; ag

;;; convert word between snake-case or camel-case
(defun my-toggle-between-camel-case-and-snake-case ()
  "Convert string into CAMEL-CASE."
  (interactive)
  (save-excursion (let* ((bounds (if (use-region-p)
                                     (cons (region-beginning) (region-end))
                                   (bounds-of-thing-at-point 'symbol)))
                         (text (buffer-substring-no-properties (car bounds) (cdr bounds)))
                         (snake-case (string-match-p "_" text)))
                    (when bounds (delete-region (car bounds) (cdr bounds))
                          (if snake-case
                              (insert (s-lower-camel-case text))
                            (insert (s-snake-case text))
                            )))))

(global-set-key (kbd "s-s") #'my-toggle-between-camel-case-and-snake-case)
;;; convert word between snake-case or camel-case

;;; swiper
(require-package 'swiper)
(ivy-mode)
(global-set-key (kbd "C-c r") 'ivy-resume)
(global-set-key (kbd "C-x b") 'ivy-switch-buffer)
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
;;; swiper

;;; ivy frame
(require-package 'ivy-posframe)
(setq ivy-display-function #'ivy-posframe-display)
(ivy-posframe-enable)
;;; ivy frame

;;; rails
(eval-after-load 'rinari
  '(progn (setq rinari-tags-file-name "GTAGS"))
  )
;;; rails

;;; ggtags
;; (require-package 'ggtags)
;; (add-hook 'prog-mode-hook
;;           '(lambda ()
;;              (when (derived-mode-p 'ruby-mode 'js2-mode)
;;                (ggtags-mode 1))))
;; (eval-after-load 'ggtags
;;   '(progn
;;      (define-key ggtags-mode-map (kbd "M-,") 'pop-tag-mark)
;;      (define-key ggtags-mode-map (kbd "C-c .") 'ggtags-find-tag-dwim)
;;      (define-key ggtags-mode-map (kbd "C-c g d") ' ggtags-find-definition)
;;      (define-key ggtags-mode-map (kbd "C-c g s") 'ggtags-find-other-symbol)
;;      (define-key ggtags-mode-map (kbd "C-c g h") 'ggtags-view-tag-history)
;;      (define-key ggtags-mode-map (kbd "C-c g r") 'ggtags-find-reference)
;;      (define-key ggtags-mode-map (kbd "C-c g x") 'ggtags-find-tag-regexp)
;;      (define-key ggtags-mode-map (kbd "C-c g k") 'ggtags-kill-file-buffers)
;;      ))
;;; ggtags

;;; Mac keybindings
(when *is-a-mac*
  (setq mac-option-modifier 'super) ; make opt key do Super
  (setq ns-function-modifier 'control)
  ;; (setq ns-function-modifier 'hyper)  ; make Fn key do Hyper
  )
;;; Mac keybindings

;; git difftool
(defun gdt ()
  "start difftool"
  (interactive)
  (shell-command "git difftool")
  )
;; git difftool

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
;;; select words in quote

;;; ruby

;;; rvm
(require-package 'rvm)
;; (rvm-use "ruby-2.3.3" "dsp")
(rvm-use-default)
;;; rvm

;;; rspec
(require-package 'rspec-mode)
(add-hook 'ruby-mode-hook 'rspec-mode)
(add-hook 'after-init-hook 'inf-ruby-switch-setup)
(add-hook 'rspec-mode-hook (lambda () (local-set-key (kbd "C-c , s") 'rspec-verify-single)))
(eval-after-load 'rspec-mode
  '(progn
     (setq rspec-command-options "--fail-fast --color")
     ))

(defadvice rspec-compile (around rspec-compile-around)
  "Use BASH shell for running the specs because of ZSH issues."
  (let ((shell-file-name "/bin/bash"))
    ad-do-it))

(ad-activate 'rspec-compile)
;;; rspec

;;; rubocop
(require-package 'rubocop)
(add-hook 'ruby-mode-hook #'rubocop-mode)
;;; rubocop

;;; ruby

;;; ido-vertical-mode
(require-package 'ido-vertical-mode)
(ido-vertical-mode t)
(setq ido-vertical-show-count t)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)
;;; ido-vertical-mode

;;; js2-mode
(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . js2-jsx-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-jsx-mode))
;;; js2-mode

;;; xref-js2
(global-set-key (kbd "C-c x d") 'xref-find-definitions)
(global-set-key (kbd "C-c x r") 'xref-find-references)
(global-set-key (kbd "C-c x p") 'xref-pop-marker-stack)
;;; xref-js2
;;; find file in project
(require-package 'find-file-in-project)
(defun my-setup-find-file-in-project ()
  (interactive)
  ;; interested filetypes
  (setq-local ffip-patterns '("*.rb" "*.js" "*.yml" "*.css" "*.scss" "*.xml"
                              "*.tmpl" "*.json" "*.md" "*.lock" "*.sh" "*.java"
                              "*.example" "*.txt" "*.el" "*.hdl" "*.tst" "*.cmp"
                              "*.erb" "*.php" "*"))
  ;; exclude below directories and files
  (setq-local ffip-prune-patterns '("*/.git/*" "*/node_modules/*" "*/dist/*"))
  )
(add-hook 'prog-mode-hook 'my-setup-find-file-in-project)
(add-hook 'markdown-mode-hook 'my-setup-find-file-in-project)
(add-hook 'java-mode-hook 'my-setup-find-file-in-project)
(global-set-key (kbd "C-c p f") 'find-file-in-project)
(global-set-key (kbd "C-c p r") 'find-file-with-similar-name)
(global-set-key (kbd "C-c p s") 'find-file-in-project-by-selected)
(global-set-key (kbd "C-c p i") 'ffip-show-diff)
;;; find file in project

;;; comment whole line or add tail
;; (defun comment-whole-line-or-add-tail (&optional arg)
;;   "Replacement for the comment-dwim command.
;;    If no region is selected and current line is not blank and we are not at
;;    the end of the line, then comment current line.
;;    Replaces default behaviour of comment-dwim, when it inserts comment at the
;;    end of the line."
;;   (interactive "*P")
;;   (comment-normalize-vars)
;;   (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
;;       (comment-or-uncomment-region (line-beginning-position) (line-end-position))
;;     (comment-dwim arg))
;;   )
;; (global-set-key (kbd "M-;") 'comment-whole-line-or-add-tail)
;;; comment whole line or add tail

;;; key frequency
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

;;; for large file(over 2000 lines), make some modification for performace
(add-hook 'prog-mode-hook
          (lambda ()
            (when (> (buffer-size) (* 2000 80))
              (setq buffer-read-only t)
              (buffer-disable-undo)
              (fundamental-mode)
              (linum-mode -1))))

;;; git-timemachine
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

;;; yasnippet
(require-package 'yasnippet)
(yas-global-mode 1)

(defun remove-final-newline ()
  "Remove new line."
  (set (make-local-variable 'require-final-newline) nil))
(add-hook 'js2-mode-hook 'remove-final-newline)
(add-hook 'ruby-mode-hook 'remove-final-newline)
;;; yasnippet

;;; multi-term
(require-package 'multi-term)
(add-to-list 'load-path "~/.emacs.d/site-lisp/multi-term-plus")
(require 'multi-term-config)            ;sh -c "$(curl -fsSL https://raw.github.com/aborn/multi-term-plus/master/scripts/install.sh)"
(setq multi-term-program "/bin/bash")
(setq multi-term-buffer-name "mterm")  ;; term buffer name setting.
(setq system-uses-terminfo nil) ;; Use Emacs terminfo, not system terminfo, for mac OS 4m
(global-set-key (kbd "C-M-{") 'multi-term-find)
(global-set-key (kbd "C-M-,") 'multi-term)
(add-hook 'term-mode-hook
          (lambda ()
            (setq term-buffer-maximum-size 5000) ;limit 5000 lines
            (add-to-list 'term-bind-key-alist '("M-[" . multi-term-prev))
            (add-to-list 'term-bind-key-alist '("M-]" . multi-term-next))
            (add-to-list 'term-bind-key-alist '("C-a" . multi-term-move-beginning-of-line))
            (add-to-list 'term-bind-key-alist '("C-e" . multi-term-move-end-of-line))
            (add-to-list 'term-bind-key-alist '("C-k" . multi-term-kill-line))
            (add-to-list 'term-bind-key-alist '("C-y" . term-paste))
            (setq show-trailing-whitespace nil)
            (setq yas-dont-activate t)  ;disable yasnippet, tab should be working
            (setq multi-term-recovery-p nil)
            )
          )
(multi-term-plus-init)
;;; multi-term

;;; go to last change
(global-set-key (kbd "C-x C-\\") 'session-jump-to-last-change)

;;; save & jump between positions start
(global-set-key (kbd "C-c C-<SPC>") 'point-to-register)
(global-set-key (kbd "C-c C-c C-<SPC>") 'jump-to-register)
;;; save & jump between positions end

;;; hdl mode
(add-to-list 'auto-mode-alist '("\\.hdl?\\'" . vhdl-mode))
;;; hdl mode

;;; emacs abbrev
(clear-abbrev-table global-abbrev-table)
(define-abbrev-table 'global-abbrev-table
  '(
    ;; phrase
    ("btw" "by the way" )
    ("jd" "[Deliver]")

    ;; programing
    ("eeq" "==" )
    ("eqq" "===" )
    ("ret" "return" )

    ;; regex
    ("uaz" "\\([A-Za-z0-9]+\\)" )
    ("ubracket" "\\[\\([^]]+?\\)\\]" )
    ("ucurly" "“\\([^”]+?\\)”" )
    ("ud" "\\([0-9]+\\)" )
    ("udate" "\\([0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]\\)" )
    ("udot" "\\(.\\)" )
    ("ustr" "\\([^\"]+?\\)" )
    ("utag" "\\([</>=\" A-Za-z0-9]+\\)" )

    ;; unicode
    ("md" "—" )
    ("uascii" "!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~" )

    ;; code
    ("uutf8" "-*- coding: utf-8 -*-" )
    ))

(set-default 'abbrev-mode t)
(setq save-abbrevs nil)
;;; emacs abbrev

;;; reveal current file in finder
(require-package 'reveal-in-osx-finder)

;;; symbol-overly: highlight symbol
(require-package 'symbol-overlay)
(after-load 'symbol-overlay
  (define-key symbol-overlay-mode-map (kbd "M-I") 'symbol-overlay-put)
  (define-key symbol-overlay-mode-map (kbd "<f8>") 'symbol-overlay-remove-all)
  (define-key symbol-overlay-mode-map (kbd "M-W") 'symbol-overlay-save-symbol)
  )

;;; editing
(defun insert-i18n-text-at-region (begin end)
  "Convert region text into i18n format."
  (interactive "r")
  (if (use-region-p)
      (progn (save-excursion
               (goto-char end)
               (insert "')}")
               (goto-char begin)
               (insert "{i18n.t(':::")
               )
             (goto-char (+ begin 9)))))

(defun remove-i18n-text-at-region (begin end)
  "Remove i18n format."
  (interactive "r")
  (if (use-region-p)
      (progn (save-excursion
               (let* ((str (buffer-substring begin end))
                      (target (replace-regexp-in-string "i18n.t([[:ascii:][:nonascii:]]*\\([\"']\\).*::\\(.*\\)[\"'][[:ascii:][:nonascii:]]*)" "\\1\\2\\1" str)))
                 (delete-region begin end)
                 (insert target))
               ))))
;;; editing

;;; editorconfig
;; (require-package 'editoconfig)
;; (editorconfig-mode 1)
;;; editorconfig

;; ;; ;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;;; hackernews
(require-package 'hackernews)
;;; hackernews

;;; copy file path & name to clipboard
(defun copy-file-name-or-path-to-clipboard (path)
  "Copy the current buffer file name and path to clipboard."
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (message "path is %s" path)
    (when filename
      (if path (kill-new filename)
        (kill-new (replace-regexp-in-string "\\/.*\\/"  "" filename)))
      ;; (kill-new filename)
      (message "Copied buffer '%s' to clipboard." filename))))

(defun copy-file-name-to-clipboard ()
  "Copy the current file name and path to clipbpard."
  (interactive)
  (copy-file-name-or-path-to-clipboard nil))

(defun copy-file-path-to-clipboard ()
  "Copy the current file name to clipbpard."
  (interactive)
  (copy-file-name-or-path-to-clipboard t))

(global-set-key (kbd "C-c p p") #'copy-file-path-to-clipboard)
(global-set-key (kbd "C-c p n") #'copy-file-name-to-clipboard)
;;;

;;; remove trailing whitespace
(add-to-list 'before-save-hook #'delete-trailing-whitespace)
;;; remove trailing whitespace

;;; flow
;; (load-file "~/.emacs.d/site-lisp/flow-for-emacs/flow.el")
(require-package 'flycheck-flow)
;; (flycheck-add-next-checker 'javascript-eslint 'javascript-flow)
(require-package 'flow-minor-mode)
(add-hook 'js2-mode-hook 'flow-minor-enable-automatically)
;;; flow

;;; company
(setq company-dabbrev-downcase nil)     ;not to downcase
;;; company

;;; open line above
(defun vi-open-line-above ()
  "Insert a newline above the current line and put point at beginning."
  (interactive)
  (unless (bolp)
    (beginning-of-line))
  (newline)
  (forward-line -1)
  (indent-according-to-mode))

(defun vi-open-line-below ()
  "Insert a newline below the current line and put point at beginning."
  (interactive)
  (unless (eolp)
    (end-of-line))
  (newline-and-indent))

(global-set-key (kbd "S-<return>") 'vi-open-line-above)
(global-set-key (kbd "C-o") 'vi-open-line-below)
;;; open line above

;;; emacs line
(setq whitespace-line-column 100) ;; limit line length
(setq whitespace-style '(face lines-tail))

(add-hook 'prog-mode-hook 'whitespace-mode)
;; (global-whitespace-mode +1) ;; enable this line if globally set line limit to 90 characters
;;; emacs line

;;; projectile
(require-package 'projectile)
(require-package 'counsel-projectile)
(global-set-key (kbd "M-?") 'counsel-projectile-ag)
;;; projectile

;;; cucumber
(require-package 'feature-mode)
(add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))
;;; cucumber

;;; wgrep
;;; use C-c C-o: to make counsel-ag to grep buffer
;;; use C-x C-q: enter wgrep mode
;;; use C-c C-c: finish edit and apply change
;;; use C-c C-d: Mark as delete to current line (including newline).
;;; use C-c C-r: Remove the changes in the region (these changes are not applied to the
;;;              files. Of course, the remaining changes can still be applied to the files.)
;;; use C-c C-k: Discard all changes and exit.
(require-package 'wgrep)
(setq wgrep-auto-save-buffer t)
;;; wgrep

;;; switch between frame
(global-set-key (kbd "s-o") 'ns-next-frame)
;;; switch between frame

;;; switch to previous buffer
(defun switch-to-previous-buffer ()
  "Switch to previously open buffer. Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))
;;; switch to previous buffer

;;; key-chord
(load-file "~/.emacs.d/site-lisp/key-chord.el")
(require 'key-chord)
(key-chord-define-global "jj" 'ns-next-frame)
(key-chord-define-global "JJ" 'switch-to-previous-buffer)
(key-chord-mode t)
;;; key-chord

;;; crux - Collection of Ridiculously Useful eXtensions
(require-package 'crux)
(require 'crux)
(global-set-key [remap kill-whole-line] #'crux-kill-whole-line)
(global-set-key (kbd "C-c o") #'crux-open-with)
(global-set-key (kbd "C-c M-d") #'crux-duplicate-and-comment-current-line-or-region)
(global-set-key (kbd "C-c n") #'crux-cleanup-buffer-or-region)
;;;; crux

;;; imenu
(require-package 'imenu-anywhere)
(global-set-key (kbd "s-,") #'imenu-anywhere)
(global-set-key (kbd "C-,") #'imenu)
;;; imenu anywhere

;;; dumb jump
(require-package 'dumb-jump)
(setq dumb-jump-selector 'ivy)
(define-key dumb-jump-mode-map (kbd "C-M-g") nil)
(define-key dumb-jump-mode-map (kbd "C-M-p") nil)
(define-key dumb-jump-mode-map (kbd "C-M-q") nil)
(define-key dumb-jump-mode-map (kbd "C-M-g g") 'dumb-jump-go)
(define-key dumb-jump-mode-map (kbd "C-M-g b") 'dumb-jump-back)
(define-key dumb-jump-mode-map (kbd "C-M-g q") 'dumb-jump-quick-look)
(define-key dumb-jump-mode-map (kbd "C-M-g o") 'dumb-jump-go-other-window)
(define-key dumb-jump-mode-map (kbd "C-M-g p") 'dumb-jump-go-prompt)
(dumb-jump-mode)
;;; dumb jump

;;; prettier
(add-hook 'js2-mode-hook 'prettier-js-mode)
(setq prettier-js-args '(
                         "--trailing-comma" "none"
                         "--bracket-spacing" "true"
                         "--print-width" "100"
                         "--tab-with" "2"
                         "--single-quote" "false"
                         "--jsx-bracket-same-line" "false"
                         ))
;;; prettier

;;; auto pair
(setq electric-pair-preserve-balance nil)
;;; auto pair

;;; magit
(eval-after-load 'magit
  '(progn
     (define-key magit-log-mode-map (kbd "C-c C-w") #'magit-copy-section-value)
     ))
(global-set-key (kbd "C-x G") #'counsel-git-change-worktree)
;;; magit

;;; go
(require-package 'go-mode)
(require-package 'go-snippets)
(require-package 'go-guru)

;;; get GOPATH
(when (memq window-system '(mac ns))
  (exec-path-from-shell-copy-env "GOPATH"))

(defun my-go-mode-hook ()
  "Define function to call when go-mode load."
  (require-package 'go-guru)
  (require-package 'go-snippets)
  (require-package 'company-go)

  (set (make-local-variable 'company-backends) '(company-go))
  (company-mode)

  (add-hook 'before-save-hook 'gofmt-before-save) ; gofmt before every save

  (setq gofmt-command "goimports")                ; gofmt uses invokes goimports

  (if (not (string-match "go" compile-command))   ; set compile command default
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet"))

  ;; guru settings
  (go-guru-hl-identifier-mode)                    ; highlight identifiers

  ;; (setq-default indent-tabs-mode nil)
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 4)

  ;; Key bindings specific to go-mode
  (local-set-key (kbd "M-.") 'godef-jump)         ; Go to definition
  (local-set-key (kbd "M-*") 'pop-tag-mark)       ; Return from whence you came
  (local-set-key (kbd "M-p") 'compile)            ; Invoke compiler
  (local-set-key (kbd "M-P") 'recompile)          ; Redo most recent compile cmd
  (local-set-key (kbd "M-]") 'next-error)         ; Go to next error (or msg)
  (local-set-key (kbd "M-[") 'previous-error))    ; Go to previous error or msg

;; Connect go-mode-hook with the function we just defined
(add-hook 'go-mode-hook #'my-go-mode-hook)
;;; go

;;; vue
(require-package 'vue-mode)
(add-to-list 'auto-mode-alist '("\\.vue$" . vue-mode))
;;; vue

;;; org
(require-package 'org-bullets)
(require-package 'org-beautify-theme)
;;; org

(provide 'init-local)
;;; init-local.el ends here
