;;; post-init.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-

;; Use Command key as Meta
(setq mac-command-modifier 'meta)
;; Use Option key as Meta as well
(setq mac-option-modifier 'meta)

;; Fullscreen by default, as early as possible. This tiny window is not enough
(setq ns-use-native-fullscreen :true)
(add-to-list 'default-frame-alist '(fullscreen . fullscreen))

;; Highlight matching parens, with no delay
(setq show-paren-delay 0)

;; Display guide at 80 chars
(setq-default display-fill-column-indicator-column 80)
(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)
(add-hook 'text-mode-hook 'display-fill-column-indicator-mode)
(add-hook 'text-mode-hook 'display-fill-column-indicator-mode)
(set-face-attribute 'fill-column-indicator nil :foreground "grey20")

;; Enable auto-fill-mode
(add-hook 'prog-mode-hook 'auto-fill-mode)

;; Autosave every 5 characters
(setq auto-save-interval 5)

;; Set the mark rings capacity to 100
(setq global-mark-ring-max 100)
(setq mark-ring-max 100)

;; Use UTF-8
(setenv "LC_CTYPE" "en_US.UTF-8")

(pixel-scroll-precision-mode)

(setq display-time-format "%H:%M:%S %Z(%z)")
(setq display-time-interval 1)
(display-time-mode)

;; Change cursor color
(setq default-frame-alist `((cursor-color . "#FF2400")
                            ,@default-frame-alist)) 

;; Auto-revert in Emacs is a feature that automatically updates the
;; contents of a buffer to reflect changes made to the underlying file
;; on disk.
(add-hook 'after-init-hook #'global-auto-revert-mode)

;; recentf is an Emacs package that maintains a list of recently
;; accessed files, making it easier to reopen files you have worked on
;; recently.
(add-hook 'after-init-hook #'recentf-mode)
(setq recentf-auto-cleanup 'never)

;; Don't ask for confirmation before killing
(setq confirm-kill-emacs nil)

;; savehist is an Emacs feature that preserves the minibuffer history between
;; sessions. It saves the history of inputs in the minibuffer, such as commands,
;; search strings, and other prompts, to a file. This allows users to retain
;; their minibuffer history across Emacs restarts.
(add-hook 'after-init-hook #'savehist-mode)

;; save-place-mode enables Emacs to remember the last location within a file
;; upon reopening. This feature is particularly beneficial for resuming work at
;; the precise point where you previously left off.
(add-hook 'after-init-hook #'save-place-mode)

;; A few more useful configurations...
(use-package emacs
  :custom
  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; Hide commands in M-x which do not work in the current mode.  Vertico
  ;; commands are hidden in normal buffers. This setting is useful beyond
  ;; Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))

(use-package doom-themes
  :init
  (load-theme 'doom-moonlight))

(use-package vertico
  ;; It is recommended to enable the savehist package, because vertico sorts by
  ;; history position
  :ensure t
  :defer t
  :commands vertico-mode
  :hook (after-init . vertico-mode))

(use-package orderless
  ;; Vertico leverages Orderless' flexible matching capabilities, allowing users
  ;; to input multiple patterns separated by spaces, which Orderless then
  ;; matches in any order against the candidates.
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  ;; Marginalia allows Embark to offer you preconfigured actions in more
  ;; contexts. In addition to that, Marginalia also enhances Vertico by adding
  ;; rich annotations to the completion candidates displayed in Vertico's
  ;; interface.
  :ensure t
  :defer t
  :commands (marginalia-mode marginalia-cycle)
  :hook (after-init . marginalia-mode))

(use-package embark
  ;; Embark is an Emacs package that acts like a context menu, allowing
  ;; users to perform context-sensitive actions on selected items
  ;; directly from the completion interface.
  :ensure t
  :defer t
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init
  ;; Replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :after (embark consult)

  ;; Only necessary if you have the hook below
  :demand t
  
  ;; If you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package consult
  :ensure t
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x t b" . consult-buffer-other-tab)
         ("C-x r b" . consult-bookmark)
         ("C-x p b" . consult-project-buffer)
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)
         ("M-s e" . consult-isearch-history)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)
         ("M-r" . consult-history))

  ;; Enable automatic preview at point in the *Completions* buffer.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  :init
  ;; Optionally configure the register formatting. This improves the register
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))
  (setq consult-narrow-key "<"))

(use-package corfu
  :ensure t
  :defer t
  :commands (corfu-mode global-corfu-mode)

  :hook ((prog-mode . corfu-mode)
         (shell-mode . corfu-mode)
         (eshell-mode . corfu-mode))

  :custom
  ;; Hide commands in M-x which do not apply to the current mode.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Disable Ispell completion function. As an alternative try `cape-dict'.
  (text-mode-ispell-word-completion nil)
  (tab-always-indent 'complete)

  ;; Enable Corfu
  :config
  (global-corfu-mode))

(use-package cape
  :ensure t
  :defer t
  :commands (cape-dabbrev cape-file cape-elisp-block)
  :bind ("M-p" . cape-prefix-map)
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))

;; Displays the key bindings following currently entered incomplete command (a
;; prefix) in a popup
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package popper
  :ensure t                             ; or :straight t
  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode
          compilation-mode
          apropos-mode))
  (popper-mode +1)
  (popper-echo-mode +1))

;; Jump anywhere on scree using a char-based decision tree
(use-package avy
  :ensure t
  :bind ("C-'" . 'avy-goto-char-timer)
  :config
  (avy-setup-default)
  (set-face-attribute 'avy-lead-face nil
                      :foreground "black"
                      :background "#FFD700"))

(use-package ace-window
  :ensure t
  :bind ("M-n" . 'ace-window)
  :config
  (setq aw-dispatch-always t))

;; Dim windows that don't contain the point
(use-package dimmer
  :ensure t
  :config
  (dimmer-configure-which-key)
  (setq dimmer-fraction 0.4)
  (dimmer-mode t))

(use-package multiple-cursors
  :ensure t)

(use-package rainbow-delimiters
  :ensure t
  :config
  (dolist (hook '(emacs-lisp-mode-hook ;; When editing Emacs Lisp code
                  ;; While interactively evaluating Emacs Lisp expressions in
                  ;; inferior-emacs-lisp-mode (IELM)
                  ielm-mode-hook
                  ;; In Lisp interaction mode
                  lisp-interaction-mode-hook
                  ;; While editing Common Lisp code
                  lisp-mode-hook
                  ;; In SLIME REPL
                  slime-repl-mode-hook))
    (add-hook hook 'rainbow-delimiters-mode))
  (set-face-foreground 'rainbow-delimiters-depth-1-face "#c66") ;; red
  (set-face-foreground 'rainbow-delimiters-depth-2-face "#6c6") ;; green
  (set-face-foreground 'rainbow-delimiters-depth-3-face "#69f") ;; blue
  (set-face-foreground 'rainbow-delimiters-depth-4-face "#cc6") ;; yellow
  (set-face-foreground 'rainbow-delimiters-depth-5-face "#6cc") ;; cyan
  (set-face-foreground 'rainbow-delimiters-depth-6-face "#c6c") ;; magenta
  (set-face-foreground 'rainbow-delimiters-depth-7-face "#ccc") ;; light gray
  (set-face-foreground 'rainbow-delimiters-depth-8-face "#999") ;; medium gray
  (set-face-foreground 'rainbow-delimiters-depth-9-face "#666")) ;; dark gray

(use-package lispy
  :ensure t
  :init
  (dolist (hook '(emacs-lisp-mode-hook ;; When editing Emacs Lisp code
                ;; In eval-expression minibuffer
                 eval-expression-minibuffer-setup-hook
                ;; While interactively evaluating Emacs Lisp expressions in
                ;; inferior-emacs-lisp-mode (IELM)
                 ielm-mode-hook
                ;; In Lisp interaction mode
                lisp-interaction-mode-hook
                ;; While editing Common Lisp code
                lisp-mode-hook
                ;; In SLIME REPL
                slime-repl-mode-hook))
    (add-hook hook (lambda () (lispy-mode 1)))))

(use-package slime
  :ensure t
  :config
  (setq inferior-lisp-program "sbcl")
  (defun override-slime-del-key ()
    (define-key slime-repl-mode-map
                (read-kbd-macro "DEL") nil))
  (add-hook 'slime-repl-mode-hook 'override-slime-del-key)
  (require 'slime-autoloads)
  (slime-setup '(slime-repl
                 slime-tramp
                 slime-asdf)))

(use-package slime-docker
  :custom
  (slime-docker-image-name "sbcl-dev")
  (slime-docker-mounts `(((,(expand-file-name "~/projects/dendrit/") . "/root/"))))
  (slime-docker-uid 0)
  (slime-docker-gid 0)
  (slime-docker-ports `((:ip "127.0.0.1" :host-port 56366 :container-port 5000))))


(use-package treesit
  ;; emacs built-in
  :ensure nil
  :mode (("\\.js\\'"  . typescript-ts-mode)
         ("\\.json\\'" .  json-ts-mode)
         ("\\.Dockerfile\\'" . dockerfile-ts-mode))
  :preface
  (defun os/setup-install-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    (dolist (grammar
             '((css . ("https://github.com/tree-sitter/tree-sitter-css" "master"))
               (bash "https://github.com/tree-sitter/tree-sitter-bash" "master")
               (html . ("https://github.com/tree-sitter/tree-sitter-html" "master"))
               (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
               (json . ("https://github.com/tree-sitter/tree-sitter-json" "master"))
               (markdown . ("https://github.com/ikatyang/tree-sitter-markdown" "master"))
               (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
               (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
               (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "master"))))
      (add-to-list 'treesit-language-source-alist grammar)
      ;; Only install `grammar' if we don't already have it
      ;; installed. However, if you want to *update* a grammar then
      ;; this obviously prevents that from happening.
      (unless (treesit-language-available-p (car grammar))
        (treesit-install-language-grammar (car grammar)))))

  ;; Optional, but recommended. Tree-sitter enabled major modes are
  ;; distinct from their ordinary counterparts.
  ;;
  ;; You can remap major modes with `major-mode-remap-alist'. Note
  ;; that this does *not* extend to hooks! Make sure you migrate them
  ;; also
  (dolist (mapping
           '((css-mode . css-ts-mode)
             (js-mode . typescript-ts-mode)
             (js2-mode . typescript-ts-mode)
             (bash-mode . bash-ts-mode)
             (json-mode . json-ts-mode)
             (js-json-mode . json-ts-mode)
             (sh-mode . bash-ts-mode)
             (sh-base-mode . bash-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))
  :config
  (os/setup-install-grammars))


(use-package dockerfile-mode
  :ensure t
  :config
  (put 'dockerfile-image-name 'safe-local-variable #'stringp))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (shell . t)))

;; This is done so we can link by ID from other files. See
;; https://orgmode.org/org.html#FOOT27 and https://stackoverflow.com/a/64808821
;; for more info
(setq org-link-search-must-match-exact-headline nil)


