;;;packages
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

;;; add executable path
(dolist (path '("/usr/local/bin" "/Users/Aaron/gocode/bin" "/Library/TeX/texbin"))
  (add-to-list 'exec-path path))
(setenv "PATH"
        (concat "/usr/local/bin" ":"
                (concat (getenv "HOME") "/gocode/bin") ":"
                "/Library/TeX/texbin" ":"
                (getenv "PATH")))
;;; language server
(require-package 'lsp-mode)
(require-package 'lsp-ui)
(require-package 'lsp-vue)
(require-package 'company-lsp)
;;; language server

;;; nvm
(require-package 'nvm)
(nvm-use "v8.11.3")
;;; nvm

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

(defun redmine-kill-buffer ()
  "Delete redmine buffer."
  (interactive)
  (kill-buffer "redmine")
  )

(defun redmine-add-subtask (subject)
  "Create subtask under current ticket."
  (interactive "sPlease enter subject:")
  (shell-command (format "redmine ci -a 72 -t Task -p %s 1 '%s'" (get-ticket-number) subject))
  )

(defun redmine-add-verify-subtask ()
  "Create verify subtask under current ticket."
  (interactive)
  ;; (interactive "sPlease enter subject: please verify ")
  (shell-command (format "redmine ci -a 72 -t Task -p %s 1 'please verify %s'" (get-ticket-number) (get-ticket-number)))
  )

(defun redmine-add-task (subject)
  "Create redmine task."
  (shell-command (format "redmine ci -a 72 -t Task 1 '%s'" subject))
  )

(defun redmine-open-issue ()
  "Open redmine issue."
  (interactive)
  (shell-command (format "redmine open %s" (get-ticket-number)))
  )

(defun redmine-develop-issue (yes-or-no)
  "Develop redmine issue."
  (interactive "sDevelop this issue?")
  (if (equal yes-or-no "y")
      (shell-command (format "redmine ui -a 72 -s 'In Progress' %s" (get-ticket-number))))
  )

(defun redmine-resolve-issue (yes-or-no)
  "Resolve redmine issue."
  (interactive "sResolve this issue?")
  (if (equal yes-or-no "y")
      (shell-command (format "redmine ui -a 72 -r 100 -s Resolved %s" (get-ticket-number))))
  )

(defun redmine-close-issue (yes-or-no)
  "Close redmine issue."
  (interactive "sClose this issue?")
  (if (equal yes-or-no "y")
      (shell-command (format "redmine ui -a 72 -r 100 -s Closed %s" (get-ticket-number))))
  )

(defun redmine-get-branch ()
  "Get branch naming from ticket."
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

;;; avy
(require-package 'avy)
(global-set-key (kbd "C-c C-;") #'avy-goto-char-timer)
(global-set-key (kbd "C-x C-;") #'avy-goto-line)
(global-set-key (kbd "C-x ;") #'avy-goto-word-1)
;;; avy

;;; ag
(if (eq system-type 'darwin)
    (setq-default counsel-ag-base-command "/usr/local/bin/ag --vimgrep --nocolor --nogroup %s"))
(global-set-key (kbd "M-?") 'counsel-ag)
;;; ag

;;; convert word between snake-case or camel-case
(defun my-toggle-between-camel-case-and-snake-case ()
  "Convert string between camelCase and snake_case."
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

(defun my-toggle-between-camel-case-and-dash-words ()
  "Convert string betwen CamelCase and dash-words."
  (interactive)
  (save-excursion (let* ((bounds (if (use-region-p)
                                     (cons (region-beginning) (region-end))
                                   (bounds-of-thing-at-point 'symbol)))
                         (text (buffer-substring-no-properties (car bounds) (cdr bounds)))
                         (dashed-words (string-match-p "-" text)))
                    (when bounds (delete-region (car bounds) (cdr bounds))
                          (if dashed-words
                              (insert (s-upper-camel-case text))
                            (insert (s-dashed-words text))
                            )))))

(global-set-key (kbd "s-s") #'my-toggle-between-camel-case-and-snake-case)
(global-set-key (kbd "s-d") #'my-toggle-between-camel-case-and-dash-words)
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
;; (require-package 'ivy-posframe)
;; (setq ivy-display-function #'ivy-posframe-display)
;; (ivy-posframe-enable)
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
;;              (when (derived-mode-p 'ruby-mode 'js2-mode 'typescript-mode)
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

;;; tags
(require-package 'counsel-etags)
(eval-after-load 'counsel-etags
  '(progn
     ;; counsel-etags-ignore-directories does NOT support wildcast
     (add-to-list 'counsel-etags-ignore-directories "node_modules")
     (add-to-list 'counsel-etags-ignore-directories ".git")
     (add-to-list 'counsel-etags-ignore-directories "build")
     (add-to-list 'counsel-etags-ignore-directories "dest")
     ;; counsel-etags-ignore-filenames supports wildcast
     (add-to-list 'counsel-etags-ignore-filenames "TAGS")
     (add-to-list 'counsel-etags-ignore-filenames "GTAGS")
     (add-to-list 'counsel-etags-ignore-filenames "GRTAGS")
     (add-to-list 'counsel-etags-ignore-filenames "GPATH")
     (add-to-list 'counsel-etags-ignore-filenames "gatgs.files")
     ;; (add-to-list 'counsel-etags-ignore-filenames "*.json")
     )
  )
;;; tags

;;; Mac keybindings
(when *is-a-mac*
  (setq mac-option-modifier 'super) ; make opt key do Super
  (setq ns-function-modifier 'control)
  ;; (setq ns-function-modifier 'hyper)  ; make Fn key do Hyper
  )
;;; Mac keybindings

;; git difftool
(defun gdt ()
  "Start difftool."
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
(require-package 'js2-refactor)
(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . js2-jsx-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-jsx-mode))
(add-hook 'js2-mode-hook 'prettier-js-mode)
(setq prettier-js-args '(
                         "--trailing-comma" "none"
                         "--bracket-spacing" "true"
                         "--print-width" "90"
                         "--tab-width" "2"
                         "--single-quote" "false"
                         "--jsx-bracket-same-line" "false"
                         ))
(add-hook 'js2-mode-hook #'js2-refactor-mode)
(js2r-add-keybindings-with-prefix "C-c C-m")
(setq js2-skip-preprocessor-directives t)
;;; link https://github.com/magnars/js2-refactor.el
;;; js2-mode

;;; find file in project
(require-package 'find-file-in-project)
(defun my-setup-find-file-in-project ()
  "Find file in project."
  (interactive)
  ;; interested filetypes
  (setq-local ffip-patterns '("*.rb" "*.js" "*.yml" "*.css" "*.scss" "sass" "*.xml"
                              "*.tmpl" "*.json" "*.md" "*.lock" "*.sh" "*.java"
                              "*.example" "*.txt" "*.el" "*.hdl" "*.tst" "*.cmp"
                              "*.erb" "*.php" "*.m" "*.conf" "*.feature" "*.groovy"
                              "*"))
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
        avy-goto-word-1
        avy-goto-char-timer
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
        paredit-forward-kill-word
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
(setq system-uses-terminfo nil) ;; Use Emacs terminfo, not system terminfo, for mac OS
(global-set-key (kbd "C-M-{") 'multi-term-find)
(global-set-key (kbd "C-M-,") 'multi-term)
(add-hook 'term-mode-hook
          (lambda ()
            (setq term-buffer-maximum-size 5000) ;limit 5000 lines
            (setq show-trailing-whitespace nil)
            (setq yas-dont-activate t)  ;disable yasnippet, tab should be working
            (setq multi-term-recovery-p nil)
            (setq term-truncate-lines 1)
            (add-to-list 'term-bind-key-alist '("M-[" . multi-term-prev))
            (add-to-list 'term-bind-key-alist '("M-]" . multi-term-next))
            (add-to-list 'term-bind-key-alist '("C-a" . multi-term-move-beginning-of-line))
            (add-to-list 'term-bind-key-alist '("C-e" . multi-term-move-end-of-line))
            (add-to-list 'term-bind-key-alist '("C-k" . multi-term-kill-line))
            (add-to-list 'term-bind-key-alist '("C-w" . term-send-backward-kill-word))
            (add-to-list 'term-bind-key-alist '("C-y" . term-paste))))
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
    ("btw" "by the way")
    ("jd" "[Deliver]")

    ;; programing
    ("eeq" "===")
    ("eqq" "==")
    ("neq" "!==")
    ("nqq" "!=")
    ("xar" "=>")
    ("lar" "<-")
    ("ass" ":=")
    ("ret" "return")

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
    ("mdd" "—" )
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
  (define-key symbol-overlay-mode-map (kbd "M-I") #'symbol-overlay-put)
  (define-key symbol-overlay-mode-map (kbd "<f8>") #'symbol-overlay-remove-all)
  (define-key symbol-overlay-mode-map (kbd "M-W") #'symbol-overlay-save-symbol)
  )

;;; editorconfig
;; (require-package 'editoconfig)
;; (editorconfig-mode 1)
;;; editorconfig

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;;; hackernews
(require-package 'hackernews)
;;; hackernews

;;; copy file path & name to clipboard
(defun copy-file-name-or-path-to-clipboard (path)
  "Copy the current buffer file name and PATH to clipboard."
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
(add-to-list 'before-save-hook 'delete-trailing-whitespace)
;;; remove trailing whitespace

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
;; (require-package 'projectile)
;; (require-package 'counsel-projectile)
;; (global-set-key (kbd "M-?") 'counsel-projectile-ag)
;;; projectile

;;; cucumber
(require-package 'feature-mode)
(add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))
(setq feature-step-search-path "features/**/*steps.rb")
(setq feature-step-search-gems-path "gems/ruby/*/gems/*/**/*steps.rb")
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
  "Switch to previously open buffer.  Repeated invocations toggle between the two most recently open buffers."
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

;;; go-mode
(require-package 'go-mode)
(require-package 'go-snippets)
(require-package 'go-guru)
(require-package 'gotest)

(defun my-go-mode-hook ()
  "Define function to call when go-mode load."
  ;;; get go related environment variables
  (setenv "GOPATH" (concat (getenv "HOME") "/gocode"))
  (setenv "GOROOT" "/usr/local/opt/go/libexec")

  (setq flycheck-disabled-checkers '(go-vet)) ;fix for go-vet

  ;; (add-hook 'go-mode-hook (lambda () (paredit-everywhere-mode -1)))

  (setq gofmt-command "goimports")                ; gofmt to invokes goimports

  (add-hook 'before-save-hook #'gofmt-before-save) ; gofmt before every save

  (if (not (string-match "go" compile-command))   ; set compile command default
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet"))

  ;; guru settings
  (go-guru-hl-identifier-mode)                    ; highlight identifiers

  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 4)

  ;; Key bindings specific to go-mode
  (local-set-key (kbd "M-.") #'godef-jump)         ; Go to definition
  (local-set-key (kbd "M-*") #'pop-tag-mark)       ; Return from where you came
  (local-set-key (kbd "s-p") #'compile)            ; Invoke compiler
  (local-set-key (kbd "s-P") #'recompile)          ; Redo most recent compile cmd
  (local-set-key (kbd "s-c p") #'go-test-current-project)
  (local-set-key (kbd "s-c f") #'go-test-current-file)
  (local-set-key (kbd "M-]") #'next-error)         ; Go to next error (or msg)
  (local-set-key (kbd "M-[") #'previous-error)     ; Go to previous error or msg
  (local-set-key (kbd "RET") #'newline)
  )

;; Connect go-mode-hook with the function we just defined
(add-hook 'go-mode-hook #'my-go-mode-hook)
;;; go-mode

;;; vue
(require-package 'vue-mode)
(add-to-list 'auto-mode-alist '("\\.vue$" . vue-mode))
(defun my-vue-hook ()
  "Vue hook."
  (require 'lsp-vue)
  (require 'lsp-ui)
  (lsp-vue-mmm-enable))
(add-hook 'vue-mode-hook #'my-vue-hook)
;;; vue

;;; org
(require-package 'org-plus-contrib)
(require-package 'org-bullets)
(require-package 'org-beautify-theme)
(require 'org-crypt)
(require-package 'ob-go)
(require-package 'ob-http)
(add-hook 'org-mode-hook #'org-bullets-mode)
(add-hook 'org-mode-hook (lambda () (load-theme 'org-beautify)))
(add-hook 'org-mode-hook (lambda () (auto-fill-mode 1)))
(add-hook 'org-mode-hook (lambda () (setq auto-save-default nil)))
(add-hook 'org-mode-hook (lambda () (setq-default org-hide-leading-stars t)))
(add-hook 'org-mode-hook #'symbol-overlay-mode)

;;; encryption
(org-crypt-use-before-save-magic)       ;encrypt before save to disk
(setq encrypt-tag "crypt")
(setq org-crypt-tag-matcher encrypt-tag)  ;encrypt content with tag
(setq org-tags-exclude-from-inheritance (quote (encrypt-tag)))  ;avoid children double encrypted
(setq org-crypt-key nil)
(custom-set-variables
 '(org-directory "~/.emacs.d/org/"))
;;; encryption

;;; open org directory
(require-package 'ox-pandoc)
(defun org-dir-dired ()
  "Open Dired for Org files in and under `org-directory`."
  (interactive)
  (cd org-directory)
  (dired "*.org" "-lRF"))

(defun org-file-list ()
  "Open org file list at `org-directory`."
  (interactive)
  (let* ((cands (split-string
                 (shell-command-to-string (concat "find " org-directory "*.org")) "\n" t)))
    (ivy-read "File: " cands
              :action #'find-file
              :caller 'org-file)
    ))
(defun org-file-today ()
  "Open today org file at `org-directory`."
  (interactive)
  (find-file (concat "~/.emacs.d/org/" (shell-command-to-string "echo -n $(date +%Y-%m-%d)") ".org")))
(global-set-key (kbd "s-f") 'org-file-today)
(global-set-key (kbd "s-F") 'org-file-list)

;;; add <el for emacs-lisp expansion
(add-to-list 'org-structure-template-alist
             '("el" "#+BEGIN_SRC emacs-lisp\n\n#+END_SRC" "<src lang=\"?\">\n\n</src>"))
;;; add <el for emacs-lisp expansion

;; add <js for javascript expansion
(add-to-list 'org-structure-template-alist
             '("js" "#+BEGIN_SRC js\n\n#+END_SRC" "<src lang=\"?\">\n\n</src>"))
;; add <js for javascript expansion

;; add <ts for typescript expansion
(add-to-list 'org-structure-template-alist
             '("ts" "#+BEGIN_SRC typescript\n\n#+END_SRC" "<src lang=\"?\">\n\n</src>"))
;; add <ts for typescript expansion

;; add <sql for sql expansion
(add-to-list 'org-structure-template-alist
             '("sql" "#+BEGIN_SRC sql\n\n#+END_SRC" "<src lang=\"?\">\n\n</src>"))
;; add <sql for sql expansion

;; add <rb for ruby expansion
(add-to-list 'org-structure-template-alist
             '("rb" "#+BEGIN_SRC ruby\n\n#+END_SRC" "<src lang=\"?\">\n\n</src>"))
;; add <rb for ruby expansion

;; add <html for html expansion
(add-to-list 'org-structure-template-alist
             '("html" "#+BEGIN_SRC browser\n\n#+END_SRC" "<src lang=\"?\">\n\n</src>"))
;; add <html for html expansion

;; add <go for golang expansion
(add-to-list 'org-structure-template-alist
             '("go" "#+BEGIN_SRC go\n\n#+END_SRC" "<src lang=\"?\">\n\n</src>"))
;; add <go for golang expansion

;; add <sh for shell expansion
(add-to-list 'org-structure-template-alist
             '("sh" "#+BEGIN_SRC shell\n\n#+END_SRC" "<src lang=\"?\">\n\n</src>"))
;; add <sh for shell expansion

;; add <http for http expansion
(add-to-list 'org-structure-template-alist
             '("http" "#+BEGIN_SRC http\n\n#+END_SRC" "<src lang=\"?\">\n\n</src>"))
;; add <http for http expansion

;; add <gvy for groovy expansion
(add-to-list 'org-structure-template-alist
             '("gvy" "#+BEGIN_SRC groovy\n\n#+END_SRC" "<src lang=\"?\">\n\n</src>"))
;; add <gvy for groovy expansion

;;; org

;;; groovy
(require-package 'groovy-mode)
;;; groovy

;;; convert date of 03/04/1997 to 1997.04.03
(defun my-convert-date ()
  "Covert date of 03/04/1997 to 1997.04.03."
  (interactive)
  (let* ((bounds (if (use-region-p)
                     (cons (region-beginning) (region-end))))
         (text (buffer-substring-no-properties (car bounds) (cdr bounds))))
    (when bounds
      (delete-region (car bounds) (cdr bounds))
      (insert (replace-regexp-in-string "\\([0-9]+\\)\/\\([0-9]+\\)\/\\([0-9]+\\)" "\\3\.\\1\.\\2" text)))))
;;; convert date of 03/04/1997 to 1997.04.03

;;; highlight todos
(defun highlight-todos ()
  "Highlight some dev keywords."
  (font-lock-add-keywords
   nil
   '(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t))))
(add-hook 'prog-mode-hook #'highlight-todos)
;;; highlight todos

;;; chez scheme
(require-package 'geiser)
(setq geiser-active-implementations '(chez))
;;; chez scheme

;;; scss
(defun customize-scss ()
  "Customize scss settings."
  (and
   (set (make-local-variable 'css-indent-offset) 4))) ;indent to 4 spaces/level
(add-hook 'css-mode-hook '(lambda () (customize-scss)))
;;; scss

;;; html
(add-hook 'sgml-mode-hook (lambda () (tagedit-add-experimental-features)))
(add-hook 'sgml-mode-hook 'origami-mode)
;;; html

;; typescript
(require-package 'tern)
(add-hook 'typescript-mode-hook (lambda () (setq typescript-indent-level 2)))
(add-hook 'typescript-mode-hook (lambda () (tern-mode t)))
(defun delete-tern-process ()
  "Force restart of tern in new project."
  (interactive)
  (delete-process "Tern"))

(require-package 'tide)
(defun setup-tide ()
  "Setup typescript interactive env."
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1))

;; formats the buffer before saving
;; (add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide)

(add-hook 'typescript-mode-hook 'prettier-js-mode)
(setq prettier-js-args '(
                         "--trailing-comma" "none"
                         "--bracket-spacing" "true"
                         "--print-width" "90"
                         "--tab-width" "2"
                         "--single-quote" "true"
                         "--jsx-bracket-same-line" "false"
                         "--arrow-parens" "always"
                         ))
;; typescript

;;; js2-refactor
(setq js2-skip-preprocessor-directives t)
(js2r-add-keybindings-with-prefix "C-c C-m") ;;eg. extract function with `C-c C-m ef`.
;; (js2r-add-keybindings-with-modifier "C-s-") ;; eg. extract function with `C-s-e C-s-f`.
;;; js2-refactor

;;; markdown
(setq-default markdown-hide-markup t)
;;; markdown

;;; swift
(require-package 'swift-mode)
;;; swift

;;; wakatime
(require-package 'wakatime-mode)
(global-wakatime-mode)
;;; wakatime

;;; open url
(global-set-key (kbd "C-c C-u") #'browse-url-at-point)
;;; open url

;;; theme
(add-hook 'after-init-hook (lambda () (load-theme 'sanityinc-solarized-dark)))
;;; theme


(provide 'init-local)
;;; init-local.el ends here
