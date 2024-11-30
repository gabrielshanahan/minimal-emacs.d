;;; post-init.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;; Contains all customizations of Emacs

;;; Code:
;; Use Command key as Meta
(setq mac-command-modifier 'meta)
;; Use Option key as Meta as well
(setq mac-option-modifier 'meta)

;; Fullscreen by default, as early as possible. This tiny window is not enough
(setq ns-use-native-fullscreen :true)
(add-to-list 'default-frame-alist '(fullscreen . fullscreen))

;; Highlight matching parens, with no delay
(setq show-paren-delay 0)

;; Set fill column and display guide at 120 chars
(setq-default fill-column 120)
(setq-default display-fill-column-indicator-column 120)
(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)
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

;; Scroll by pixels, not by lines
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

;; Allow using :straight in `use-package'
(straight-use-package-mode 1)

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
  :commands vertico-mode
  :hook (after-init . vertico-mode))

(use-package orderless
  ;; Vertico leverages Orderless' flexible matching capabilities, allowing users
  ;; to input multiple patterns separated by spaces, which Orderless then
  ;; matches in any order against the candidates.
  :ensure t
  :custom
  (orderless-matching-styles '(orderless-literal orderless-regexp orderless-flex))
  (completion-styles '(orderless partial-completion basic))
  (completion-category-defaults nil)
  ;; The basic completion style needs to be tried first (not as a fallback) for
  ;; TRAMP hostname completion to work. The partial-completion style allows you
  ;; to use wildcards for file completion and partial paths, e.g., /u/s/l for
  ;; /usr/share/local
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  ;; Marginalia allows Embark to offer you preconfigured actions in more
  ;; contexts. In addition to that, Marginalia also enhances Vertico by adding
  ;; rich annotations to the completion candidates displayed in Vertico's
  ;; interface.
  :ensure t
  :commands (marginalia-mode marginalia-cycle)
  :hook (after-init . marginalia-mode))

(use-package embark
  ;; Embark is an Emacs package that acts like a context menu, allowing
  ;; users to perform context-sensitive actions on selected items
  ;; directly from the completion interface.
  :ensure t
  :defer t
  :bind
  (("C-c ." . embark-act)       ;; Basically "right-click" a thing at point
   ("C-c /" . embark-dwim)      ;; Basically "left-click" a thing at point
   ("C-h B" . embark-bindings)) ;; Alternative for `describe-bindings'
  
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
  :defer t
  :after (embark consult)

  ;; Only necessary if you have the hook below
  :demand t
  
  ;; If you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package consult-dir
  :ensure t
  :defer t
  :after (consult vertico)
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

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
  ;; Configure the register formatting. This improves the register
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Tweak the register preview window.
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

;; In-buffer completion - basically Vertico for buffers
(use-package corfu
  :ensure t
  :defer t
  :after orderless
  :commands (corfu-mode global-corfu-mode)

  :hook ((prog-mode . corfu-mode)
         (shell-mode . corfu-mode)
         (eshell-mode . corfu-mode))

  :custom
  ;; Hide commands in M-x which do not apply to the current mode.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Disable Ispell completion function. As an alternative try `cape-dict'.
  (text-mode-ispell-word-completion nil)
  ;; When TAB is pressed, first try indendint line. If it's already indented,
  ;; then complete at point
  (tab-always-indent 'complete)
  ;; Allows cycling through candidates
  (corfu-cycle t)
  ;; Enable auto completion
  (corfu-auto t)
  ;; Minimum length of prefix for completion
  (corfu-auto-prefix 2)
  ;; No delay for completion
  (corfu-auto-delay 0)
  ;; Automatically update info popup after that number of seconds
  (corfu-popupinfo-delay '(0.5 . 0.2))
  ;; Insert previewed candidate
  (corfu-preview-current nil)
  (corfu-preselect 'prompt)
  ;; Don't auto expand tempel snippets
  (corfu-on-exact-match nil)

  :bind (:map corfu-map
              ("M-SPC"      . corfu-insert-separator)
              ("TAB"        . corfu-next)
              ([tab]        . corfu-next)
              ("S-TAB"      . corfu-previous)
              ([backtab]    . corfu-previous)
              ("S-<return>" . corfu-insert)
              ("RET"        . corfu-insert))

  ;; Enable Corfu
  :init
  (global-corfu-mode)
  ;; Update Corfu history and sort completions by history.
  (corfu-history-mode)
  ;; Display an information popup for completion candidate when using Corfu. The
  ;; popup displays either the candidate documentation or the candidate
  ;; location.
  (corfu-popupinfo-mode)
  :config
  (add-hook 'eshell-mode-hook
            (lambda ()
              (setq-local corfu-quit-at-boundary t
                          corfu-quit-no-match t
                          corfu-auto nil)
              (corfu-mode))
            nil
            t))

;; Completion backends for Corfu
(use-package cape
  :ensure t
  :commands (cape-dabbrev cape-file cape-elisp-block)
  :bind ("C-c p" . cape-prefix-map)
  :init
  ;; Word from current buffers
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  ;; Eshell, Comint or minibuffer history
  (add-hook 'completion-at-point-functions #'cape-history)
  ;; Elisp symbol
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  ;; Elisp in MD/ORG code blocks
  (add-hook 'completion-at-point-functions #'cape-elisp-symbol)
  ;; Programming keyword
  (add-hook 'completion-at-point-functions #'cape-keyword))

;; Displays the key bindings following currently entered incomplete command (a
;; prefix) in a popup
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package popper
  :ensure t
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
  :defer t
  :bind ("C-c g" . 'avy-goto-char-timer)
  :config
  (avy-setup-default)
  (set-face-attribute 'avy-lead-face nil
                      :foreground "black"
                      :background "#FFD700"))

(use-package ace-window
  :ensure t
  :defer t
  :bind ("C-c w" . 'ace-window)
  :config
  ;; Don't automatically switch to other window when there are only two.
  ;; This is because we want to be able to run other commands that "switch to"
  (setq aw-dispatch-always t))

;; Dim windows that don't contain the point
(use-package dimmer
  :ensure t
  :config
  (dimmer-configure-which-key)
  (setq dimmer-fraction 0.4)
  (dimmer-mode t))

;; Highlight TODO's etc
(use-package hl-todo
  :hook (prism-mode . hl-todo-mode)
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        `(("TODO"       warning bold)
          ("FIXME"      error bold)
          ("HACK"       font-lock-constant-face bold)
          ("REVIEW"     font-lock-keyword-face bold)
          ("NOTE"       success bold)
          ("DEPRECATED" font-lock-doc-face bold))))

(use-package multiple-cursors
  :ensure t
  :defer t)

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
  (set-face-foreground 'rainbow-delimiters-depth-1-face "#c66")  ;; red
  (set-face-foreground 'rainbow-delimiters-depth-2-face "#6c6")  ;; green
  (set-face-foreground 'rainbow-delimiters-depth-3-face "#69f")  ;; blue
  (set-face-foreground 'rainbow-delimiters-depth-4-face "#cc6")  ;; yellow
  (set-face-foreground 'rainbow-delimiters-depth-5-face "#6cc")  ;; cyan
  (set-face-foreground 'rainbow-delimiters-depth-6-face "#c6c")  ;; magenta
  (set-face-foreground 'rainbow-delimiters-depth-7-face "#ccc")  ;; light gray
  (set-face-foreground 'rainbow-delimiters-depth-8-face "#999")  ;; medium gray
  (set-face-foreground 'rainbow-delimiters-depth-9-face "#666")) ;; dark gray

(use-package lispy
  :ensure t
  :defer t
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
  :defer t
  :config
  (setq inferior-lisp-program "sbcl")
  (defun override-slime-del-key ()
    (define-key slime-repl-mode-map
                (read-kbd-macro "DEL") nil))
  (add-hook 'slime-repl-mode-hook 'override-slime-del-key)
  (require 'slime-autoloads)
  (slime-setup '(slime-fancy
                 slime-tramp
                 slime-asdf)))

(use-package slime-docker
  :ensure t
  :defer t
  :custom
  (slime-docker-image-name "sbcl-dev")
  (slime-docker-mounts `(((,(expand-file-name "~/projects/dendrit/") . "/root/"))))
  (slime-docker-uid 0)
  (slime-docker-gid 0)
  (slime-docker-ports `((:ip "127.0.0.1" :host-port 56366 :container-port 5000))))

(use-package treesit
  ;; emacs built-in
  :ensure nil
  :defer t
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

;; Linter/syntax checker
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :bind (:map flycheck-mode-map
              ("M-n" . flycheck-next-error)
              ("M-p" . flycheck-previous-error)))

(use-package dockerfile-mode
  :ensure t
  :defer t
  :config
  (put 'dockerfile-image-name 'safe-local-variable #'stringp))

;;; ORG mode
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (shell . t)))

;; This is done so we can link by ID from other files. See
;; https://orgmode.org/org.html#FOOT27 and https://stackoverflow.com/a/64808821
;; for more info
(setq org-link-search-must-match-exact-headline nil)

(use-package gumshoe
  :init
  ;; Enabing global-gumshoe-mode will initiate tracking
  (global-gumshoe-mode +1)
  :config
  :bind (("M-[" . gumshoe-win-backtrack)
         :map global-gumshoe-backtracking-mode-map
         ("M-[" . global-gumshoe-backtracking-mode-back)
         ("M-]" . global-gumshoe-backtracking-mode-forward)))

;; Colorises s-exp's based on their depth.
;; This mode needs to be activated manually the first time, since the hook is
;; only added in :config. This is because if it's added in :init, the colors get
;; completely screwed up for reasons I couldn't figure out (likely some other
;; mode hook runs afterwards, and changes something). It's not perfect, but in
;; practice it works fine
(use-package prism
  :ensure t
  :defer t
  :config
  (dolist (hook '(emacs-lisp-mode-hook lisp-mode-hook))
    (add-hook hook 'prism-mode))
  (prism-set-colors :num 16
    :desaturations (cl-loop for i from 0 below 16
                            collect (* i 2.5))
    :lightens (cl-loop for i from 0 below 16
                       collect (* i 2.5))
    :colors (list "sandy brown" "DarkOrchid1" "medium sea green" "dodgerblue")
  
    :comments-fn
    (lambda (color)
      (prism-blend color
                   (face-attribute 'font-lock-comment-face :foreground) 0.25))
  
    :strings-fn
    (lambda (color)
      (prism-blend color "white" 0.5))))

;; Edit in grep buffers
(use-package wgrep
  :ensure t
  :defer t
  :config
  (setq wgrep-auto-save-buffer t))

;; Needed for `lsp-enable-snippet'
(use-package yasnippet
  :ensure t
  :defer t
  :after lsp-mode)

;; https://emacs-lsp.github.io/lsp-mode/page/performance/#use-plists-for-deserialization
;; https://github.com/emacs-lsp/lsp-mode/issues/4059
;; Also necessary to delete all *.elc files (in my case in .emacs.d/var/straight
;; https://github.com/emacs-lsp/lsp-mode/issues/3602
(setq lsp-use-plists t)
(use-package lsp-mode
  :diminish "LSP"
  :ensure t
  :hook ((lsp-mode . lsp-diagnostics-mode)
         (lsp-mode . lsp-enable-which-key-integration)
         ((tsx-ts-mode
           typescript-ts-mode
           js-ts-mode) . lsp-deferred))
  :custom
  (lsp-keymap-prefix "C-c l")           ; Prefix for LSP actions
  (lsp-completion-provider :none)       ; Using Corfu as the provider
  (lsp-diagnostics-provider :flycheck)
  (lsp-session-file (locate-user-emacs-file ".lsp-session"))
  (lsp-log-io nil) ; IMPORTANT! Toggle only for debugging! Drastically affects performance
  (lsp-keep-workspace-alive nil) ; Close LSP server if all project buffers are closed
  (lsp-idle-delay 0.5)           ; Debounce timer for `after-change-function'
  ;; core
  (lsp-enable-xref t)           ; Use xref to find references
  (lsp-auto-configure t)        ; Used to decide between current active servers
  (lsp-eldoc-enable-hover t)    ; Display signature information in the echo area
  (lsp-enable-dap-auto-configure t)     ; Debug support
  (lsp-enable-file-watchers nil)
  (lsp-enable-imenu t)
  (lsp-enable-links nil)                 ; No need since we have `browse-url'
  (lsp-enable-suggest-server-download t) ; Useful prompt to download LSP providers
  (lsp-enable-symbol-highlighting t) ; Shows usages of symbol at point in the current buffer
  (lsp-enable-text-document-color nil)  ; This is Treesitter's job

  (lsp-ui-sideline-show-hover nil)      ; Sideline used only for diagnostics
  (lsp-ui-sideline-diagnostic-max-lines 20) ; 20 lines since typescript errors can be quite big
  ;; completion
  (lsp-completion-enable t)
  (lsp-completion-enable-additional-text-edit t) ; Ex: auto-insert an import for a completion candidate
  (lsp-enable-snippet t)              ; Important to provide full JSX completion
  (lsp-completion-show-kind t)        ; Optional
  ;; headerline
  (lsp-headerline-breadcrumb-enable t)  ; Optional, I like the breadcrumbs
  (lsp-headerline-breadcrumb-enable-diagnostics nil) ; Don't make them red, too noisy
  (lsp-headerline-breadcrumb-enable-symbol-numbers nil)
  (lsp-headerline-breadcrumb-icons-enable nil)
  ;; modeline
  (lsp-modeline-code-actions-enable nil) ; Modeline should be relatively clean
  (lsp-modeline-diagnostics-enable nil)  ; Already supported through `flycheck'
  (lsp-modeline-workspace-status-enable nil) ; Modeline displays "LSP" when lsp-mode is enabled
  (lsp-signature-doc-lines 1)      ; Don't raise the echo area. It's distracting
  (lsp-ui-doc-use-childframe t)    ; Show docs for symbol at point
  (lsp-eldoc-render-all t) ; This would be very useful if it would respect `lsp-signature-doc-lines', currently it's distracting
  ;; lens
  (lsp-lens-enable t)              ; Optional, I don't need it
  ;; semantic
  (lsp-semantic-tokens-enable nil) ; Related to highlighting, and we defer to treesitter
  :preface
  (defun lsp-booster--advice-json-parse (old-fn &rest args)
    "Try to parse bytecode instead of json."
    (or
     (when (equal (following-char) ?#)

       (let ((bytecode (read (current-buffer))))
         (when (byte-code-function-p bytecode)
           (funcall bytecode))))
     (apply old-fn args)))
  (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
    "Prepend emacs-lsp-booster command to lsp CMD."
    (let ((orig-result (funcall old-fn cmd test?)))
      (if (and (not test?) ;; for check lsp-server-present?
               (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
               lsp-use-plists
               (not (functionp 'json-rpc-connection)) ;; native json-rpc
               (executable-find "emacs-lsp-booster"))
          (progn
            (message "Using emacs-lsp-booster for %s!" orig-result)
            (cons "emacs-lsp-booster" orig-result))
        orig-result)))
  :init
  ;; Initiate https://github.com/blahgeek/emacs-lsp-booster for performance
  (advice-add (if (progn (require 'json)
                         (fboundp 'json-parse-buffer))
                  'json-parse-buffer
                'json-read)
              :around
              #'lsp-booster--advice-json-parse)
  (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command))

(use-package lsp-ui
  :ensure t
  :commands
  (lsp-ui-doc-show
   lsp-ui-doc-glance)
  :bind (:map lsp-mode-map
              ("C-c C-d" . 'lsp-ui-doc-glance))
  :after lsp-mode
  :config (setq lsp-ui-doc-enable t
                lsp-ui-doc-show-with-cursor nil ; Don't show doc when cursor is over symbol - too distracting
                lsp-ui-doc-include-signature t  ; Show signature
                lsp-ui-doc-position 'at-point))

(use-package lsp-tailwindcss
  :straight '(lsp-tailwindcss :type git :host github :repo "merrickluo/lsp-tailwindcss")
  :init (setq lsp-tailwindcss-add-on-mode t)
  :config
  (dolist (tw-major-mode
           '(css-mode
             css-ts-mode
             typescript-mode
             typescript-ts-mode
             tsx-ts-mode
             js2-mode
             js-ts-mode
             lisp-mode))
    (add-to-list 'lsp-tailwindcss-major-modes tw-major-mode)))

;; End
