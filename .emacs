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

(show-smartparens-global-mode +1)


;; Terminal
;; from https://www.reddit.com/r/emacs/comments/1zkj2d/advanced_usage_of_eshell/
(defun term-here ()
  "Opens up a new shell in the directory associated with the current buffer's file."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (name (car (last (split-string parent "/" t))))
         (buffer-name (concat "*eshell: " name "*")))
    (split-window-vertically)
    (other-window 1)

    (if (get-buffer buffer-name)
        (switch-to-buffer buffer-name)

        (progn
          (eshell "new")
          (rename-buffer buffer-name)))))

(global-set-key (kbd "C-'") 'term-here)


;; Git
(global-set-key (kbd "C-;") 'magit-status)


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


;; LaTeX
(require 'ob-latex)
(setq org-src-fontify-natively t)


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
(add-hook 'web-mode-hook
	  (lambda ()
	    (web-mode-set-content-type "jsx")))


;; Flycheck / Flow
(require 'flycheck-flow)
;; (add-hook 'web-mode-hook 'flycheck-mode)


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


;; fonts
(add-hook 'dired-mode-hook
          (lambda ()
            (setq buffer-face-mode-face '(:family "Monaco"))
            (buffer-face-mode)))


;; irc config
(require 'rcirc)

(require 'rcirc-color)
(rcirc-track-minor-mode 1)

(call-process "autoirc" nil 0 nil)


;; private
(load-file "~/.emacs.d/private.el")


;; custom-set-variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#e090d7" "#8cc4ff" "#eeeeec"])
 '(clojure-defun-indents (quote (add-watch render init-state render-state)))
 '(custom-enabled-themes (quote (deeper-blue)))
 '(js-curly-indent-offset 0)
 '(js-indent-level 2)
 '(web-mode-attr-indent-offset 2)
 '(web-mode-code-indent-offset 2)
 '(web-mode-enable-auto-indentation t)
 '(web-mode-enable-auto-quoting nil)
 '(web-mode-markup-indent-offset 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


(server-start)
