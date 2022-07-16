;; Package management
(require 'package) ;; You might already have this line
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize) ;; You might already have this line

(add-to-list 'load-path "~/.emacs.d/site-lisp/")

;; Keybonds
(global-set-key [(hyper a)] 'mark-whole-buffer)
(global-set-key [(hyper v)] 'yank)
(global-set-key [(hyper c)] 'kill-ring-save)
(global-set-key [(hyper s)] 'save-buffer)
(global-set-key [(hyper l)] 'goto-line)
(global-set-key [(hyper w)]
                (lambda () (interactive) (delete-frame)))
(global-set-key [(hyper z)] 'undo)
(global-set-key [(hyper n)] 'make-frame)
(global-set-key [(hyper m)] 'suspend-frame)
(global-set-key (kbd "H-`") 'other-frame)
(global-set-key (kbd "H-h") 'ns-do-hide-emacs)

(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'hyper)


;; General UI
(tool-bar-mode -1)

(setq-default line-spacing 3)

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

(global-set-key (kbd "C-c w") 'whitespace-mode)

(show-smartparens-global-mode +1)

(global-anzu-mode +1)


;; compile
(require 'xterm-color)
;; (add-hook 'compilation-start-hook
;;           (lambda (proc)
;;             ;; We need to differentiate between compilation-mode buffers
;;             ;; and running as part of comint (which at this point we assume
;;             ;; has been configured separately for xterm-color)
;;             (when (eq (process-filter proc) 'compilation-filter)
;;               ;; This is a process associated with a compilation-mode buffer.
;;               ;; We may call `xterm-color-filter' before its own filter function.
;;               (set-process-filter
;;                proc
;;                (lambda (proc string)
;;                  (funcall 'compilation-filter proc
;;                           (xterm-color-filter string)))))))

(global-set-key (kbd "C-.") 'recompile)

;; asm
(require 'asm-mode)
(add-hook 'asm-mode-hook (lambda ()
                           (setq indent-tabs-mode nil) ; use spaces to indent
                           (electric-indent-mode -1) ; indentation in asm-mode is annoying
                           (setq tab-stop-list (number-sequence 2 60 2))))


;; rust
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'rust-mode-hook #'flycheck-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)

(add-hook 'rust-mode-hook
          '(lambda ()
             ;; Use flycheck-rust in rust-mode
             (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
             ;; Key binding to auto complete and indent
             (local-set-key (kbd "TAB") #'company-indent-or-complete-common)))

(setq rust-format-on-save t)
(setq company-tooltip-align-annotations t)
(setq company-idle-delay nil)

;; Proof General
;; (load-file "~/.emacs.d/ProofGeneral/generic/proof-site.el")

;; Terminal
;; from https://www.reddit.com/r/emacs/comments/1zkj2d/advanced_usage_of_eshell/
(defun term-here ()
  "Opens up a new shell in the directory associated with the current buffer's file."
  (interactive)
  (let ((buffer-name (term-name)))
    (-if-let (buffer (get-buffer buffer-name))
        (if (eq buffer (current-buffer))
            ;; just go to end of buffer if we're already in the target eshell buffer
            (end-of-buffer)
          (-if-let (win (get-buffer-window buffer))
              (select-window win)
            (progn
              (split-window-vertically)
              (other-window 1)
              (switch-to-buffer buffer-name)
              (end-of-buffer))))
      (progn
        (split-window-vertically)
        (other-window 1)
        (eshell "new")
        (rename-buffer buffer-name)))))

(defun term-name ()
  "Makes up an appropriate name for an eshell."
  (let* ((parent
          (or (condition-case nil
                  (projectile-project-root)
                (error
                 (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory)))
              "/Users/osnr"))
         (parent-last (car (last (split-string parent "/" t))))
         (tramp-context (file-remote-p default-directory 'host))
         (prefix (if current-prefix-arg
                     (concat "*eshell " (prin1-to-string current-prefix-arg) " ") ; so it doesn't get renamed later
                   "*eshell: ")))
    (concat prefix (when tramp-context (concat tramp-context ":")) parent-last "*")))

(global-set-key (kbd "C-'") 'term-here)
(global-set-key (kbd "C-\"") 'eshell)

(add-hook 'eshell-mode-hook
          (lambda ()
            (setq xterm-color-preserve-properties t)
            (local-unset-key (kbd "C-c C-f"))))
(add-hook 'eshell-preoutput-filter-functions 'xterm-color-filter)

(fset 'xterm-color-unfontify-region 'font-lock-default-unfontify-region)
(progn (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter)
       (setq comint-output-filter-functions (remove 'ansi-color-process-output comint-output-filter-functions))
       (setq font-lock-unfontify-region-function 'xterm-color-unfontify-region))

(defun my-eshell-ido-complete-command-history ()
  (interactive)
  (eshell-kill-input)
  (insert
   (ido-completing-read "Run command: " (delete-dups (ring-elements eshell-history-ring))))
  (eshell-send-input))

(add-hook 'eshell-mode-hook
      (lambda ()
        (local-set-key (kbd "C-c h") #'my-eshell-ido-complete-command-history)))

(defun eshell-clear-buffer ()
  "Clear terminal"
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

(defun eshell/clear (&rest args)
  (eshell-clear-buffer))

(defun eshell/e (&rest args)
  (if (null args)
      ;; If I just ran "emacs", I probably expect to be launching
      ;; Emacs, which is rather silly since I'm already in Emacs.
      ;; So just pretend to do what I ask.
      (bury-buffer)
    ;; We have to expand the file names or else naming a directory in an
    ;; argument causes later arguments to be looked for in that directory,
    ;; not the starting directory
    (mapc #'find-file (mapcar #'expand-file-name (eshell-flatten-list (reverse args))))))

(add-hook 'eshell-directory-change-hook
          (defun term-update-title ()
            ;; Only rename for directory if this is a directory-oriented terminal.
            (when (string-match "eshell:" (buffer-name))
              (rename-buffer (term-name)))))

(defun bol-maybe-general-my (prompt &optional alt-bol-fcn)
  ""
  (interactive)
  (if (or (and (string-match (concat "^" (regexp-quote prompt)
                                     " *$")
                             (buffer-substring-no-properties
                              (line-beginning-position)
                              (point)))
               (not (bolp)))
          (not (equal (line-number-at-pos)
                      (count-lines (point-min) (point-max)))))
      (beginning-of-line)
    (if alt-bol-fcn
        (funcall alt-bol-fcn)
      (beginning-of-line)
      (search-forward-regexp prompt))))
(defun eshell-bol-maybe-my ()
  ""
  (interactive)
  (bol-maybe-general-my (funcall eshell-prompt-function)))
(add-hook 'eshell-mode-hook '(lambda ()
                               (local-set-key (kbd "C-a")
                                              'eshell-bol-maybe-my)))

(defun cr (&rest args)
  (eshell/cd (projectile-project-root)))

(defun eshell/next140 (&rest args)
  "Open a Pintos project. Lays out window."
  ;; open DESIGNDOC
  ;; open diff
  ;; open hw2.txt
  ;; open Ousterhout HW2

      (if (null args)
          ;; If I just ran "emacs", I probably expect to be launching
          ;; Emacs, which is rather silly since I'm already in Emacs.
          ;; So just pretend to do what I ask.
          (bury-buffer)
        ;; We have to expand the file names or else naming a directory in an
        ;; argument causes later arguments to be looked for in that directory,
        ;; not the starting directory
        (mapc #'find-file (mapcar #'expand-file-name (eshell-flatten-list (reverse args))))))

(require 'esh-module)
(add-to-list 'eshell-modules-list 'eshell-tramp)
(setq password-cache t) ; enable password caching
(setq password-cache-expiry 3600) ; for one hour (time in secs)

;; Git
(global-set-key (kbd "C-;") 'magit-status)
(setq vc-follow-symlinks t)
(with-eval-after-load 'magit
  (require 'forge))
(setq auth-sources '("~/.authinfo"))

;; Projectile
(projectile-global-mode)
(global-set-key (kbd "C-c C-f") 'projectile-find-file)
(global-set-key (kbd "C-c p f") 'projectile-find-file)


;; Ag
(global-set-key (kbd "C-,") 'projectile-ag)
(global-set-key (kbd "C-<") 'ag)


;; Window split
(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
	     (next-win-buffer (window-buffer (next-window)))
	     (this-win-edges (window-edges (selected-window)))
	     (next-win-edges (window-edges (next-window)))
	     (this-win-2nd (not (and (<= (car this-win-edges)
					 (car next-win-edges))
				     (<= (cadr this-win-edges)
					 (cadr next-win-edges)))))
	     (splitter
	      (if (= (car this-win-edges)
		     (car (window-edges (next-window))))
		  'split-window-horizontally
		'split-window-vertically)))
	(delete-other-windows)
	(let ((first-win (selected-window)))
	  (funcall splitter)
	  (if this-win-2nd (other-window 1))
	  (set-window-buffer (selected-window) this-win-buffer)
	  (set-window-buffer (next-window) next-win-buffer)
	  (select-window first-win)
	  (if this-win-2nd (other-window 1))))))

(global-set-key (kbd "C-x |") 'toggle-window-split)


;; Fuzzy search
(require 'ido)
(ido-mode t)

(require 'smex)
(global-set-key (kbd "M-x") 'smex)


;; Acme emulation
;; (require 'acme-search)
;; (global-set-key [(hyper mouse-1)] 'acme-search-forward)


;; Notes
(require 'deft)
(setq deft-extensions '("org"))
(setq deft-default-extension "org")
(global-set-key (kbd "C-`") 'deft)
(setq deft-use-filename-as-title t)
(setq deft-use-filter-string-for-filename t)
(setq deft-text-mode 'org-mode)

(defun newsletter-insert-image (url &optional link-url)
  (let* ((date-folder (file-name-as-directory (file-name-sans-extension (buffer-file-name))))
         (target-name (concat date-folder
                              (read-string (concat "Image name for "
                                                   (file-name-nondirectory url)
                                                   ": ")))))
    ;; create date-folder for newsletter if doesn't exist
    (unless (file-exists-p date-folder) (make-directory date-folder))

    (url-copy-file url target-name 1) ; copy image to date-folder. will prompt if overwriting
    (magit-stage-file target-name)
    (insert (format "#+CAPTION: %s\n[[%s]]" (or link-url target-name) target-name))
    (org-display-inline-images t t)))
(defun newsletter-buffer-p ()
  (string-prefix-p "/Users/osnr/Code/newsletters" (expand-file-name (buffer-file-name))))

(defun org-insert-image (url &optional link-url)
  (let* ((parent-folder (file-name-directory (buffer-file-name)))
         (file-name (read-string (concat "Image name for "
                                                   (file-name-nondirectory url)
                                                   ": ")))
         (target-name (concat parent-folder
                              file-name)))
    
    (url-copy-file url target-name 1) ; copy image to parent-folder. will prompt if overwriting
    (magit-stage-file target-name)
    (insert (format "#+CAPTION: %s\n[[./%s]]" (or link-url target-name)
                    file-name))
    (org-display-inline-images t t)))
(defun org-image-dnd-protocol (url action)
  (when (and (derived-mode-p 'org-mode)
             (string-match "\\(png\\|jp[e]?g\\)\\>" url))
    (x-focus-frame nil) ; do i need this?

    (when (string-prefix-p "file:///var/folders/" url)
      ;; it's from Screenotate; Esc out of Screenotate
      (shell-command "osascript -e 'tell application \"System Events\" to key code 53'"))

    (if (newsletter-buffer-p)
        (newsletter-insert-image url)
      (org-insert-image url))))


(defun newsletter-embed-tweet ()
  (defun tabfs-eval (tab-path expr &optional all-frames)
    (let ((eval-path (concat (file-name-as-directory tab-path)
                             (if all-frames
                                 "evals/emacs.all-frames.js"
                               "evals/emacs.js"))))
      (write-region expr nil eval-path)
      (with-temp-buffer
        (insert-file-contents (concat eval-path ".result"))
        (buffer-string))))
  
  (interactive)

  ;; ensure that browser is focused on Publish Tweet
  (unless (with-temp-buffer
            (insert-file-contents "/Users/osnr/t/tabs/last-focused/title.txt")
            (string-prefix-p "Twitter Publish" (buffer-string)))
    (error "please focus on publish tweet dude"))

  ;; remove "Tweet your reply", "Share" buttons
  (let* ((tab-path "/Users/osnr/t/tabs/last-focused"))
    ;; should run in all frames (we only care about iframe w embed in it)
    (tabfs-eval tab-path "
      [...document.querySelectorAll('[role=link]')]
        .find(el => el.innerText.includes('Tweet your reply'))
        ?.parentElement
        ?.remove();
      document.querySelector('[aria-label=Share]')?.remove();
      " t))

  ;; get coords of tweet embed
  ;; FIXME: not having JSON.stringify crashes Safari???
  (let* ((tab-path "/Users/osnr/t/tabs/last-focused")

         (tweet-url-expr "document.querySelector('.EmbedCode-code').innerText.match('a href=.(https:..twitter.com.+?).>')[1]")
         (tweet-url (json-read-from-string (tabfs-eval tab-path tweet-url-expr)))

         (rect-expr "JSON.stringify(document.querySelector('iframe').getBoundingClientRect())")
         (rect-string-string (tabfs-eval tab-path rect-expr))
         (rect-string (json-read-from-string rect-string-string))
         (rect (json-read-from-string rect-string))

         (tab-screenshot-path (make-temp-file "tab-screenshot-" nil ".png"))
         (tweet-screenshot-path (make-temp-file "tweet-screenshot-" nil ".png")))
    ;; todo fuzz
    (shell-command (concat "cp /Users/osnr/t/tabs/last-focused/window/visible-tab.png "
                           tab-screenshot-path))
    (shell-command
     (let-alist rect
       (format "convert %s -crop %fx%f+%f+%f -bordercolor white -border 1 -fill none -fuzz 10%% -draw 'color 0,0 floodfill' -shave 1x1 %s"
               tab-screenshot-path
               (* 2 .width) (+ (* 2 .height) 2) (* 2 .left) (* 2 .top)
               tweet-screenshot-path)))

    (newsletter-insert-image (concat "file://" tweet-screenshot-path)
                             tweet-url)))

(add-hook 'org-mode-hook
          (lambda ()
            ;; (turn-on-org-cdlatex)
            (turn-on-auto-fill)
            (turn-on-visual-line-mode)
            (local-unset-key (kbd "C-'"))
            (local-unset-key (kbd "C-,"))

            (make-local-variable 'dnd-protocol-alist)
            (add-to-list 'dnd-protocol-alist '("\\(png\\|jp[e]?g\\)\\>" . org-image-dnd-protocol))))

(defun newsletter-export-markdown ()
  (defun new-tab (url) (write-region url nil "~/Code/tabfs/fs/mnt/tabs/create"))
  (defun activate-tab (tab-path) (write-region "true" nil (concat (file-name-as-directory tab-path) "active")))
  (defun focus-window (window-path) (write-region "true" nil (concat (file-name-as-directory window-path) "focused")))
  (defun find-gh-tab-path ()
    (car (file-expand-wildcards "~/Code/tabfs/fs/mnt/tabs/by-title/GitHub_Sponsors_dashboard*Updates*")))

  (interactive)

  (let ((title (car (plist-get (org-export-get-environment) ':title))))
    (org-md-export-as-markdown)
    
    (mark-whole-buffer)
    (unfill-paragraph) ; why doesn't this work in markdown-mode?
    (deactivate-mark)

    (markdown-mode)

    ;; remove # Footnotes
    (goto-char (point-min))
    (search-forward "# Footnotes" nil t)
    (replace-match "---")

    ;; TODO: convert images, upload?
    ;; TODO: I need a faster upload strategy

    ;; convert image URLs
    (goto-char (point-min))
    (while (search-forward "/Users/osnr/Code/newsletters/" nil t)
      (replace-match "https://omar.website/newsletters/"))
    (goto-char (point-min))
    (while (search-forward "file://https://" nil t)
      (replace-match "https://"))

    ;; caption->URL, width
    (goto-char (point-min))
    (while (re-search-forward (concat "!\\[img\\](\\([^ ]+\\)\\(?: \"<?\\([^)>]+\\)>?\"\\)?)"
                                      "\\(?: \\\\#\\([^\n]*\\)\\)?")
                              nil t)
      (let ((attrs (or (match-string 3)
                       "width=\"500\"")))
        (if (match-string 2) ; if there's a caption (link target)
            (replace-match (format "<a href=\"\\2\"><img src=\"\\1\" %s></a>"
                                   attrs))
          (replace-match (format "<a href=\"\\1\"><img src=\"\\1\" %s></a>"
                                 attrs)))))
    
    (let ((tab-path (or (find-gh-tab-path)
                        (progn
                          (new-tab "https://github.com/sponsors/osnr/dashboard/updates/new")
                          (sleep-for 0.2)
                          (find-gh-tab-path)))))
      ;; bring to front
      (activate-tab tab-path)
      (focus-window (concat (file-name-as-directory tab-path) "window"))

      ;; fill in title from title
      (write-region title nil (concat tab-path "/inputs/update_subject.txt"))
      ;; fill in body from body
      (write-region (point-min) (point-max) (concat tab-path "/inputs/update_body.txt")))))

(add-to-list 'image-type-file-name-regexps '("\\.pdf\\'" . imagemagick))
(add-to-list 'image-file-name-extensions "pdf")
(setq imagemagick-types-inhibit (remove 'PDF imagemagick-types-inhibit))
(setq org-image-actual-width 500)

(defadvice org-display-inline-images (around center-images activate)
  (let ((create-image-orig (symbol-function 'create-image)))
    (cl-letf (((symbol-function 'create-image)
               (lambda (file-or-data &optional type data-p &rest props)
                 (apply create-image-orig file-or-data type data-p
                        (plist-put props :background "white")))))
      ad-do-it)))


;; LaTeX
(require 'ob-latex)
(setq org-src-fontify-natively t)
(setq org-highlight-latex-and-related '(latex script entities))

(setq org-latex-pdf-process               ; for regular export
      '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
(setq-default TeX-engine 'xetex)

;; C
(defun maybe-qemu-style ()
  (when (and buffer-file-name
             (string-match "qemu" buffer-file-name))
    (c-set-style "linux")
    (setq c-basic-offset 4)))
(add-hook 'c-mode-hook 'maybe-qemu-style)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-eldoc)

(defun maybe-cs107e-style ()
  (when (and buffer-file-name
             (string-match "cs107e" buffer-file-name))
    (c-set-style "java")
    (flycheck-set-checker-executable 'c/c++-gcc "arm-none-eabi-gcc")
    (add-to-list (make-variable-buffer-local 'c-eldoc-cpp-command) "arm-none-eabi-gcc ")
    (add-to-list (make-variable-buffer-local 'c-eldoc-includes) "-I/Users/osnr/dev/cs107e/staff/libpi/include")
    (set (make-variable-buffer-local 'flycheck-gcc-args)
         '("-g" "-Wall" "-std=c99" "-ffreestanding" "-mapcs-frame" "-fno-omit-frame-pointer" "-mpoke-function-name"))
    (add-to-list (make-variable-buffer-local 'flycheck-gcc-include-path) "/Users/osnr/dev/cs107e/staff/libpi/include")
    (flycheck-select-checker 'c/c++-gcc)
    (setq c-basic-offset 4)))

(add-hook 'c-mode-hook 'flycheck-mode)
(add-hook 'c-mode-hook 'maybe-cs107e-style)

;; (add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)
;; (require 'stickyfunc-enhance)
(add-hook 'c-mode-hook 'hs-minor-mode)

(add-to-list 'safe-local-variable-values
             '(flycheck-checker . c/c++-gcc))
(add-to-list 'safe-local-variable-values
	     '(flycheck-c/c++-gcc-executable . "arm-none-eabi-gcc"))
;; (add-to-list 'safe-local-variable-values
;;              '(flycheck-gcc-args . ("-DUSERPROG" "-DVM")))
(add-to-list 'safe-local-variable-values
             '(flycheck-gcc-include-path . ("/Users/osnr/dev/cs107e/staff/libpi/include")))
(add-to-list 'safe-local-variable-values
             '(c-eldoc-cpp-command . "arm-none-eabi-gcc "))
(add-to-list 'safe-local-variable-values
             '(c-eldoc-includes . "-I/Users/osnr/dev/cs107e/staff/libpi/include"))

(require 'realgud-lldb)

;; Objective-C++
(add-to-list 'auto-mode-alist '("\\.mm\\'" . objc-mode))

;; C++
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c++-mode-hook 'flycheck-mode)
(add-hook 'c++-mode-hook 'company-mode)
(add-hook 'c++-mode-hook 'irony-eldoc)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

;; lols
(setq-default indent-tabs-mode nil)
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.saves"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups


;; web-mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-hook 'web-mode-hook
	  (lambda ()
            ;; (tern-mode t)
            (local-unset-key (kbd "C-c C-f"))))


;; Flycheck
(with-eval-after-load 'flycheck
  (add-hook 'flycheck-mode-hook #'flycheck-inline-mode))

;; TypeScript
(require 'prettier-js)
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-add-mode 'typescript-tide 'web-mode)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1)
  ;; (prettier-js-mode +1)
  )

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
;; (add-hook 'before-save-hook 'tide-format-before-save)
(global-set-key (kbd "M-'") #'company-complete)

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (or (string-equal "tsx" (file-name-extension buffer-file-name))
                      (string-equal "ts" (file-name-extension buffer-file-name)))
              (setup-tide-mode))))

(add-to-list 'auto-mode-alist '("\\.mjs\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))

;; (defun hot-reload-tabfs ()
;;   ;; copy on top of runtime background.js
;;   (copy-file "/Users/osnr/Code/tabfs/extension/background.js" "/Users/osnr/t/runtime/background.js" t)
;;   (message "hot reloaded"))

;; (add-hook 'js2-mode-hook
;;           (defun js2-setup+ ()
;;             (when (string-suffix-p "/tabfs/extension/background.js" buffer-file-name)
;;               (add-hook 'after-save-hook
;;                         #'hot-reload-tabfs
;;                         nil 'local))))

;; Clojure
(add-hook 'clojure-mode-hook
          (lambda ()
            (turn-on-smartparens-strict-mode)
            (sp-use-paredit-bindings)))


;; Emacs Lisp
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (turn-on-smartparens-strict-mode)
            (sp-use-paredit-bindings)))

;; Racket
(add-hook 'racket-mode-hook
          (lambda ()
            (turn-on-smartparens-strict-mode)
            (sp-use-paredit-bindings)))

;; Common Lisp / SBCL
;; (load (expand-file-name "~/.quicklisp/slime-helper.el"))
;; (setq inferior-lisp-program "sbcl")
;; (add-hook 'slime-repl-mode-hook
;;           (lambda ()
;;             (turn-on-smartparens-strict-mode)
;;             (sp-use-paredit-bindings)))
;; (sp-pair "'" nil :actions :rem)
;; (sp-pair "`" nil :actions :rem)

;; Markdown
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(defun website-insert-image (url &optional link-url)
  (let* ((static-folder (concat (replace-regexp-in-string "^/Users/osnr/Code/rsnous.com/content/"
                                                          "/Users/osnr/Code/rsnous.com/static/"
                                                          (file-name-sans-extension (buffer-file-name)))
                                "/"))
         (target-nondirectory-name (read-string (concat "Image name for "
                                                        (file-name-nondirectory url)
                                                        ": ")))
         (target-name (concat static-folder target-nondirectory-name)))
    ;; create static-folder for post if doesn't exist
    (unless (file-exists-p static-folder) (make-directory static-folder t))

    (url-copy-file url target-name 1) ; copy image to static-folder. will prompt if overwriting
    (magit-stage-file target-name)
    (insert (format "<div class=\"figure\">
  <img src=\"%s\">
</div>" target-nondirectory-name))))
(defun website-buffer-p ()
  (string-prefix-p "/Users/osnr/Code/rsnous.com" (expand-file-name (buffer-file-name))))

(defun doc-insert-image (url &optional link-url)
  (let* ((static-folder (concat (file-name-directory (buffer-file-name))
                                "doc/"))
         (target-nondirectory-name (read-string (concat "Image name for "
                                                        (file-name-nondirectory url)
                                                        ": ")))
         (target-name (concat static-folder target-nondirectory-name)))
    ;; create static-folder for post if doesn't exist
    (unless (file-exists-p static-folder) (make-directory static-folder t))

    (url-copy-file url target-name 1) ; copy image to static-folder. will prompt if overwriting
    (magit-stage-file target-name)
    (insert (format "<img src=\"doc/%s\" width=\"400\">" target-nondirectory-name))))

(defun markdown-image-dnd-protocol (url action)
  (when (and (derived-mode-p 'markdown-mode)
             (string-match "\\(png\\|jp[e]?g\\)\\>" url))
    (x-focus-frame nil) ; do i need this?
    
    (if (website-buffer-p)
        (progn 
          (when (string-prefix-p "file:///var/folders/" url)
            ;; it's from Screenotate; Esc out of Screenotate
            (shell-command "osascript -e 'tell application \"System Events\" to key code 53'"))
          (website-insert-image url))

      (doc-insert-image url))))

(add-hook 'markdown-mode-hook
          (lambda ()
            (turn-on-auto-fill)

            (make-local-variable 'dnd-protocol-alist)
            (add-to-list 'dnd-protocol-alist '("\\(png\\|jp[e]?g\\)\\>" . markdown-image-dnd-protocol))))

;; go
(add-hook 'go-mode-hook
          (lambda () (add-hook 'before-save-hook #'gofmt-before-save)))

;; fonts
(add-hook 'dired-mode-hook
          (lambda ()
            (setq buffer-face-mode-face '(:family "Monaco"))
            (buffer-face-mode)))

;; pdf
;; (load-file "~/.emacs.d/pdf-mode.el/pdf-mode.el")
;; (require 'pdf-mode)


;; random
(defun google ()
  "Google the selected region if any, display a query prompt otherwise."
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (url-hexify-string (if mark-active
         (buffer-substring (region-beginning) (region-end))
         (read-string "Google: "))))))
(global-set-key (kbd "C-c g") 'google)

(defun google-quoted ()
  "Google literally the selected region if any, display a query prompt otherwise."
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%22"
    (url-hexify-string (if mark-active
         (buffer-substring (region-beginning) (region-end))
         (read-string "Google quoted: ")))
    "%22")))
(global-set-key (kbd "C-c q") 'google-quoted)

(require 'string-inflection)
(global-set-key (kbd "C-c C-s") 'string-inflection-cycle)


;; python
(elpy-enable)
(defun my/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi))
(setenv "PYTHONPATH" (shell-command-to-string "$SHELL --login -c 'echo -n $PYTHONPATH'"))
(setenv "MANTLE_TARGET" "ice40")
(setq elpy-rpc-python-command "python3")


;; Verilog
(add-hook 'verilog-mode-hook
	  (lambda ()
            (local-unset-key (kbd "C-;"))))
(defun chip-view-json (arg)
  (interactive)
  ;; copy json file from chip (projectile-project-root)/arg
  (let* ((temp-json-path (make-temp-file "chip-view" nil ".json"))
         (default-directory (file-name-directory temp-json-path))
         (temp-svg-path (make-temp-file "chip-view" nil ".svg")))
    (shell-command (format "scp %s/%s %s"
                           (replace-regexp-in-string "^/ssh:" "" (projectile-project-root))
                           arg
                           temp-json-path))
    (shell-command (format "netlistsvg %s -o %s"
                           temp-json-path
                           temp-svg-path))
    (shell-command (format "open -R %s" temp-svg-path))))
(defun chip-prog (&optional arg)
  (interactive)
  (let* ((bin-filename (or arg "top_square.bin"))
         (bin-file (concat (projectile-project-root) bin-filename))
         (default-directory "/ssh:old-laptop:/Users/osnr/"))
    (eshell-command (format "cp %s %s" bin-file default-directory))
    (eshell-command (format "/ssh:old-laptop:/Library/Frameworks/Python.framework/Versions/Current/bin/tinyprog -p %s" bin-filename) t)))

;; TensorFlow/CUDA
(setenv "DYLD_LIBRARY_PATH" "/usr/local/cuda/lib")

;; PATH
(cond 
 ((or (eq window-system 'ns) (eq window-system 'mac)) ; macosx
  (progn
      (exec-path-from-shell-initialize)
      ;; Invoke login shells, so that .profile or .bash_profile is read
      (setq shell-command-switch "-lc"))))
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)

;; tramp
;; (defun tramp-aware-woman (man-page-path)
;;   (interactive)
;;   (let ((dir (eshell/pwd)))
;;     (woman-find-file
;;      (if (file-remote-p dir)
;;          (let ((vec (tramp-dissect-file-name dir)))
;;            (tramp-make-tramp-file-name
;;             (tramp-file-name-method vec)
;;             (tramp-file-name-user vec)
;;             (tramp-file-name-host vec)
;;             man-page-path))
;;        man-page-path))))


;; private
;; (load-file "~/.emacs.d/private.el")

;; (load-file "~/Code/dotfiles/realtalk-v2.el")
;; (setenv "LUA_PATH" (shell-command-to-string "$SHELL --login -c 'echo -n $LUA_PATH'"))
;; (setenv "LUA_CPATH" (shell-command-to-string "$SHELL --login -c 'echo -n $LUA_CPATH'"))

(setq lua-indent-nested-block-content-align nil)
(setq lua-indent-close-paren-align nil)

(defun lua-at-most-one-indent (old-function &rest arguments)
  (let ((old-res (apply old-function arguments)))
    (if (> old-res lua-indent-level) lua-indent-level old-res)))

(advice-add #'lua-calculate-indentation-block-modifier
            :around #'lua-at-most-one-indent)

;; elixir
(require 'eglot)
(add-to-list 'eglot-server-programs '(elixir-mode . ("/Users/osnr/aux/elixir-ls/release/language_server.sh")))

;; ocaml setup
(let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var" "share")))))
      (when (and opam-share (file-directory-p opam-share))
       ;; Register Merlin
       (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
       (autoload 'merlin-mode "merlin" nil t nil)
       ;; Automatically start it in OCaml buffers
       (add-hook 'tuareg-mode-hook 'merlin-mode t)
       (add-hook 'caml-mode-hook 'merlin-mode t)
       ;; Use opam switch to lookup ocamlmerlin binary
       (setq merlin-command 'opam)))

;; color theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/moe-theme.el/")
(add-to-list 'load-path "~/.emacs.d/moe-theme.el/")
(require 'moe-theme)

;; custom-set-variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#e090d7" "#8cc4ff" "#eeeeec"])
 '(asm-comment-char 47)
 '(auth-source-save-behavior nil)
 '(c-basic-offset 4)
 '(c-block-comment-prefix "  ")
 '(clojure-defun-indents '(add-watch render init-state render-state))
 '(compilation-always-kill t)
 '(compilation-ask-about-save nil)
 '(custom-enabled-themes '(moe-dark))
 '(custom-safe-themes
   '("27a1dd6378f3782a593cc83e108a35c2b93e5ecc3bd9057313e1d88462701fcd" default))
 '(diary-entry-marker 'font-lock-variable-name-face)
 '(dired-use-ls-dired nil)
 '(emms-mode-line-icon-image-cache
   '(image :type xpm :ascent center :data "/* XPM */
static char *note[] = {
/* width height num_colors chars_per_pixel */
\"    10   11        2            1\",
/* colors */
\". c #358d8d\",
\"# c None s None\",
/* pixels */
\"###...####\",
\"###.#...##\",
\"###.###...\",
\"###.#####.\",
\"###.#####.\",
\"#...#####.\",
\"....#####.\",
\"#..######.\",
\"#######...\",
\"######....\",
\"#######..#\" ;"))
 '(eshell-history-size 1024)
 '(fci-rule-color "#f6f0e1")
 '(flycheck-color-mode-line-face-to-color 'mode-line-buffer-id)
 '(flycheck-javascript-flow-args nil)
 '(gdb-many-windows nil)
 '(gnus-logo-colors '("#0d7b72" "#adadad") t)
 '(gnus-mode-line-image-cache
   '(image :type xpm :ascent center :data "/* XPM */
static char *gnus-pointer[] = {
/* width height num_colors chars_per_pixel */
\"    18    13        2            1\",
/* colors */
\". c #358d8d\",
\"# c None s None\",
/* pixels */
\"##################\",
\"######..##..######\",
\"#####........#####\",
\"#.##.##..##...####\",
\"#...####.###...##.\",
\"#..###.######.....\",
\"#####.########...#\",
\"###########.######\",
\"####.###.#..######\",
\"######..###.######\",
\"###....####.######\",
\"###..######.######\",
\"###########.######\" };") t)
 '(irony-supported-major-modes '(c++-mode c-mode objc-mode arduino-mode))
 '(js-curly-indent-offset 0)
 '(js-indent-level 2)
 '(large-file-warning-threshold 50000000)
 '(markdown-indent-on-enter nil)
 '(org-adapt-indentation nil)
 '(org-allow-promoting-top-level-subtree t)
 '(org-blank-before-new-entry '((heading) (plain-list-item)))
 '(org-export-backends '(ascii html latex md))
 '(org-latex-classes
   '(("beamer" "\\documentclass[presentation]{beamer}"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
     ("article" "\\documentclass[11pt]{article}"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
      ("\\paragraph{%s}" . "\\paragraph*{%s}")
      ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
     ("report" "\\documentclass[11pt]{report}"
      ("\\part{%s}" . "\\part*{%s}")
      ("\\chapter{%s}" . "\\chapter*{%s}")
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
     ("book" "\\documentclass[11pt]{book}"
      ("\\part{%s}" . "\\part*{%s}")
      ("\\chapter{%s}" . "\\chapter*{%s}")
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
     ("letter" "\\documentclass{letter}"
      ("" . ""))
     ("sigchi" "\\documentclass{sigchi}"
      ("" . ""))))
 '(org-latex-default-packages-alist
   '(("AUTO" "inputenc" t)
     ("T1" "fontenc" t)
     ("" "fixltx2e" nil)
     ("" "graphicx" t)
     ("" "grffile" t)
     ("" "longtable" nil)
     ("" "wrapfig" nil)
     ("" "rotating" nil)
     ("normalem" "ulem" t)
     ("" "amsmath" t)
     ("" "textcomp" t)
     ("" "amssymb" t)
     ("" "capt-of" nil)
     ("colorlinks=true" "hyperref" t)))
 '(org-latex-listings 'minted)
 '(org-latex-prefer-user-labels t)
 '(package-selected-packages
   '(elpy lua-mode arduino-mode unfill flycheck-irony flycheck-inline elixir-mode xterm-color web-mode tide string-inflection smex smartparens realgud-lldb projectile prettier-js moe-theme forge exec-path-from-shell eglot deft anzu ag))
 '(projectile-mode-line '(:eval (format " P[%s]" (projectile-project-name))))
 '(python-shell-interpreter "python3")
 '(safe-local-variable-values
   '((verilog-library-files "./cpu.v" "../../cartridge_interface.v" "../../NES_controller.v")
     (verilog-library-directories "." "../../fpgaboy_files/" "../..")
     (c-file-style . ruby)
     (c-eldoc-includes . "-I/Users/osnr/Dropbox/classes/cs140/pintos/src -I/Users/osnr/Dropbox/classes/cs140/pintos/src/lib -I/Users/osnr/Dropbox/classes/cs140/pintos/src/lib/kernel")
     (c-eldoc-cpp-command . "i386-elf-gcc ")
     (flycheck-gcc-include-path "/Users/osnr/Dropbox/classes/cs140/pintos/src" "/Users/osnr/Dropbox/classes/cs140/pintos/src/lib" "/Users/osnr/Dropbox/classes/cs140/pintos/src/lib/kernel")
     (flycheck-gcc-args "-DUSERPROG" "-DVM")
     (flycheck-c/c++-gcc-executable . "i386-elf-gcc")
     (flycheck-checker . c/c++-gcc)))
 '(tool-bar-mode nil)
 '(typescript-auto-indent-flag nil)
 '(typescript-indent-level 2)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   '((20 . "#c82829")
     (40 . "#f5871f")
     (60 . "#eab700")
     (80 . "#718c00")
     (100 . "#3e999f")
     (120 . "#4271ae")
     (140 . "#8959a8")
     (160 . "#c82829")
     (180 . "#f5871f")
     (200 . "#eab700")
     (220 . "#718c00")
     (240 . "#3e999f")
     (260 . "#4271ae")
     (280 . "#8959a8")
     (300 . "#c82829")
     (320 . "#f5871f")
     (340 . "#eab700")
     (360 . "#718c00")))
 '(vc-annotate-very-old-color nil)
 '(verilog-indent-level 4)
 '(verilog-indent-level-declaration 4)
 '(verilog-indent-level-module 4)
 '(verilog-indent-lists t)
 '(web-mode-attr-indent-offset 2)
 '(web-mode-code-indent-offset 2)
 '(web-mode-comment-formats '(("java" . "/*") ("php" . "/*") ("javascript" . "//")))
 '(web-mode-enable-auto-indentation nil)
 '(web-mode-enable-auto-quoting nil)
 '(web-mode-enable-comment-interpolation t)
 '(web-mode-markup-indent-offset 2))

(server-force-delete)
(server-start)

(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Operator Mono SSm" :foundry "nil" :slant normal :weight semi-light :height 131 :width normal))))
 '(font-lock-comment-delimiter-face ((t (:foreground "#a1db00" :slant italic))))
 '(font-lock-comment-face ((t (:foreground "#a1db00" :slant italic))))
 '(web-mode-comment-face ((t (:foreground "#a1db00" :slant italic)))))
(setq-default line-spacing 5)
