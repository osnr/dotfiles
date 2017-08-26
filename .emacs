;; Package management
(require 'package) ;; You might already have this line
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
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

(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'hyper)


;; General UI
(tool-bar-mode -1)

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

(global-set-key (kbd "C-c w") 'whitespace-mode)

(show-smartparens-global-mode +1)

(global-anzu-mode +1)


;; compile
(ignore-errors
  (require 'xterm-color)
  (defun my-colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (let* ((inhibit-read-only t)
             (compilation-filter-end (point))
             (new-string (delete-and-extract-region compilation-filter-start compilation-filter-end))
             (colorized-new-string (xterm-color-filter new-string)))
        (goto-char (point-max))
        (insert colorized-new-string))))
  (add-hook 'compilation-filter-hook 'my-colorize-compilation-buffer))
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
        (-if-let (win (get-buffer-window buffer))
            (select-window win)
          (progn
            (split-window-vertically)
            (other-window 1)
            (switch-to-buffer buffer-name)))
      (progn
        (split-window-vertically)
        (other-window 1)
        (eshell "new")
        (rename-buffer buffer-name)))))

(defun term-name ()
  "Makes up an appropriate name for an eshell."
  (let* ((parent
          (condition-case nil
              (projectile-project-root)
              (error
               (if (buffer-file-name)
                   (file-name-directory (buffer-file-name))
                 default-directory))))
         (name (car (last (split-string parent "/" t)))))
    (concat "*eshell: " name "*")))

(global-set-key (kbd "C-'") 'term-here)
(global-set-key (kbd "C-\"") 'eshell)

(add-hook 'eshell-mode-hook
          (lambda ()
            (setq xterm-color-preserve-properties t)
            (local-unset-key (kbd "C-c C-f"))))
(add-hook 'eshell-preoutput-filter-functions 'xterm-color-filter)

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


;; Git
(global-set-key (kbd "C-;") 'magit-status)
(setq vc-follow-symlinks t)


;; Projectile
(projectile-global-mode)
(global-set-key (kbd "C-c C-f") 'projectile-find-file)


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
(require 'acme-search)
(global-set-key [(hyper mouse-1)] 'acme-search-forward)


;; Notes
(require 'deft)
(setq deft-extensions '("org"))
(global-set-key (kbd "C-`") 'deft)
(setq deft-use-filename-as-title t)
(setq deft-use-filter-string-for-filename t)

(add-hook 'org-mode-hook
          (lambda ()
            (turn-on-org-cdlatex)
            (turn-on-auto-fill)
            (turn-on-visual-line-mode)
            (local-unset-key (kbd "C-'"))))

;; (require 'org-download)

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

;; (require 'srefactor)
;; (add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
;; (semantic-mode 1)
;; (define-key c-mode-map (kbd "M-RET") 'srefactor-refactor-at-point)
;; (define-key c++-mode-map (kbd "M-RET") 'srefactor-refactor-at-point)

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
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-hook 'web-mode-hook
	  (lambda ()
            ;; (tern-mode t)
            (local-unset-key (kbd "C-c C-f"))))


;; Flycheck / Flow
(add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-flycheck-flow")
(require 'flycheck-flow)
(add-hook 'js-mode-hook 'flycheck-mode)
(add-hook 'web-mode-hook 'flycheck-mode)
(flycheck-add-mode 'javascript-flow 'web-mode)


;; TypeScript
(require 'prettier-js)
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1)
  (prettier-js-mode))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
;; (add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)
(global-set-key (kbd "M-'") #'company-complete)

;; format options
(setq tide-format-options '(:placeOpenBraceOnNewLineForFunctions nil))

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))


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


;; Markdown
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-hook 'markdown-mode-hook
          (lambda ()
            (turn-on-auto-fill)))

;; go
(add-hook 'go-mode-hook
          (lambda () (add-hook 'before-save-hook #'gofmt-before-save)))

;; fonts
(add-hook 'dired-mode-hook
          (lambda ()
            (setq buffer-face-mode-face '(:family "Monaco"))
            (buffer-face-mode)))

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


;; PATH
(cond 
 ((eq window-system 'ns) ; macosx
  (progn
      (exec-path-from-shell-initialize)
      ;; Invoke login shells, so that .profile or .bash_profile is read
      (setq shell-command-switch "-lc"))))


;; private
(load-file "~/.emacs.d/private.el")

;; ocaml setup
(autoload 'merlin-mode "merlin" "Merlin mode" t)
(add-hook 'tuareg-mode-hook 'merlin-mode)
(add-hook 'caml-mode-hook 'merlin-mode)

;; custom-set-variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#e090d7" "#8cc4ff" "#eeeeec"])
 '(c-block-comment-prefix "  ")
 '(clojure-defun-indents (quote (add-watch render init-state render-state)))
 '(compilation-environment (quote ("TERM=xterm-256color")))
 '(compilation-error-regexp-alist (quote (python-tracebacks-and-caml)))
 '(custom-enabled-themes (quote (tango-dark)))
 '(eshell-history-size 1024)
 '(gdb-many-windows nil)
 '(js-curly-indent-offset 0)
 '(js-indent-level 2)
 '(large-file-warning-threshold 50000000)
 '(org-adapt-indentation nil)
 '(org-allow-promoting-top-level-subtree t)
 '(org-blank-before-new-entry (quote ((heading) (plain-list-item))))
 '(org-latex-classes
   (quote
    (("beamer" "\\documentclass[presentation]{beamer}"
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
      ("" . "")))))
 '(org-latex-default-packages-alist
   (quote
    (("AUTO" "inputenc" t)
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
     ("colorlinks=true" "hyperref" t))))
 '(org-latex-listings (quote minted))
 '(org-latex-prefer-user-labels t)
 '(package-selected-packages
   (quote
    (yapfify mocha recompile-on-save prettier-js typescript-mode company-jedi csv-mode web-mode-edit-element yaml-mode xterm-color wgrep-ag web-mode vagrant-tramp unfill undo-tree tuareg tern string-inflection ssh smex smartparens rich-minority rcirc-color racer projectile org-download org nodejs-repl neotree multi-term mmm-mode markdown-mode magit lua-mode image+ haskell-mode go-mode flycheck-rust exec-path-from-shell elpy elm-mode eimp deft company-racer coffee-mode clojure-mode cdlatex c-eldoc buttercup auctex anzu ag)))
 '(projectile-mode-line (quote (:eval (format " P[%s]" (projectile-project-name)))))
 '(safe-local-variable-values
   (quote
    ((verilog-library-files "./cpu.v" "../../cartridge_interface.v" "../../NES_controller.v")
     (verilog-library-directories "." "../../fpgaboy_files/" "../..")
     (c-file-style . ruby)
     (c-eldoc-includes . "-I/Users/osnr/Dropbox/classes/cs140/pintos/src -I/Users/osnr/Dropbox/classes/cs140/pintos/src/lib -I/Users/osnr/Dropbox/classes/cs140/pintos/src/lib/kernel")
     (c-eldoc-cpp-command . "i386-elf-gcc ")
     (flycheck-gcc-include-path "/Users/osnr/Dropbox/classes/cs140/pintos/src" "/Users/osnr/Dropbox/classes/cs140/pintos/src/lib" "/Users/osnr/Dropbox/classes/cs140/pintos/src/lib/kernel")
     (flycheck-gcc-args "-DUSERPROG" "-DVM")
     (flycheck-c/c++-gcc-executable . "i386-elf-gcc")
     (flycheck-checker . c/c++-gcc))))
 '(tool-bar-mode nil)
 '(typescript-auto-indent-flag nil)
 '(typescript-indent-level 2)
 '(web-mode-attr-indent-offset 2)
 '(web-mode-code-indent-offset 2)
 '(web-mode-enable-auto-indentation nil)
 '(web-mode-enable-auto-quoting nil)
 '(web-mode-markup-indent-offset 2))

(server-force-delete)
(server-start)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line
