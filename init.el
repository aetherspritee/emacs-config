;; The default is 800 kilobytes. Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

(defun start/org-babel-tangle-config ()
  "Automatically tangle our Emacs.org config file when we save it. Credit to Emacs From Scratch for this one!"
  (when (string-equal (file-name-directory (buffer-file-name))
                      (expand-file-name user-emacs-directory))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'start/org-babel-tangle-config)))

(require 'use-package-ensure) ;; Load use-package-always-ensure
(setq use-package-always-ensure t) ;; Always ensures that a package is installed
(setq package-archives '(("melpa" . "https://melpa.org/packages/") ;; Sets default package repositories
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/"))) ;; For Eat Terminal

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(defun +org/dwim-at-point (&optional arg)
  "Do-what-I-mean at point.

If on a:
- checkbox list item or todo heading: toggle it.
- citation: follow it
- headline: cycle ARCHIVE subtrees, toggle latex fragments and inline images in
  subtree; update statistics cookies/checkboxes and ToCs.
- clock: update its time.
- footnote reference: jump to the footnote's definition
- footnote definition: jump to the first reference of this footnote
- timestamp: open an agenda view for the time-stamp date/range at point.
- table-row or a TBLFM: recalculate the table's formulas
- table-cell: clear it and go into insert mode. If this is a formula cell,
  recaluclate it instead.
- babel-call: execute the source block
- statistics-cookie: update it.
- src block: execute it
- latex fragment: toggle it.
- link: follow it
- otherwise, refresh all inline images in current tree."
  (interactive "P")
  (if (button-at (point))
      (call-interactively #'push-button)
    (let* ((context (org-element-context))
           (type (org-element-type context)))
      ;; skip over unimportant contexts
      (while (and context (memq type '(verbatim code bold italic underline strike-through subscript superscript)))
        (setq context (org-element-property :parent context)
              type (org-element-type context)))
      (pcase type
        ((or `citation `citation-reference)
         (org-cite-follow context arg))

        (`headline
         (cond ((memq (bound-and-true-p org-goto-map)
                      (current-active-maps))
                (org-goto-ret))
               ((and (fboundp 'toc-org-insert-toc)
                     (member "TOC" (org-get-tags)))
                (toc-org-insert-toc)
                (message "Updating table of contents"))
               ((string= "ARCHIVE" (car-safe (org-get-tags)))
                (org-force-cycle-archived))
               ((or (org-element-property :todo-type context)
                    (org-element-property :scheduled context))
                (org-todo
                 (if (eq (org-element-property :todo-type context) 'done)
                     (or (car (+org-get-todo-keywords-for (org-element-property :todo-keyword context)))
                         'todo)
                   'done))))
         ;; Update any metadata or inline previews in this subtree
         (org-update-checkbox-count)
         (org-update-parent-todo-statistics)
         (when (and (fboundp 'toc-org-insert-toc)
                    (member "TOC" (org-get-tags)))
           (toc-org-insert-toc)
           (message "Updating table of contents"))
         (let* ((beg (if (org-before-first-heading-p)
                         (line-beginning-position)
                       (save-excursion (org-back-to-heading) (point))))
                (end (if (org-before-first-heading-p)
                         (line-end-position)
                       (save-excursion (org-end-of-subtree) (point))))
                (overlays (ignore-errors (overlays-in beg end)))
                (latex-overlays
                 (cl-find-if (lambda (o) (eq (overlay-get o 'org-overlay-type) 'org-latex-overlay))
                             overlays))
                (image-overlays
                 (cl-find-if (lambda (o) (overlay-get o 'org-image-overlay))
                             overlays)))
           (+org--toggle-inline-images-in-subtree beg end)
           (if (or image-overlays latex-overlays)
               (org-clear-latex-preview beg end)
             (org--latex-preview-region beg end))))

        (`clock (org-clock-update-time-maybe))

        (`footnote-reference
         (org-footnote-goto-definition (org-element-property :label context)))

        (`footnote-definition
         (org-footnote-goto-previous-reference (org-element-property :label context)))

        ((or `planning `timestamp)
         (org-follow-timestamp-link))

        ((or `table `table-row)
         (if (org-at-TBLFM-p)
             (org-table-calc-current-TBLFM)
           (ignore-errors
             (save-excursion
               (goto-char (org-element-property :contents-begin context))
               (org-call-with-arg 'org-table-recalculate (or arg t))))))

        (`table-cell
         (org-table-blank-field)
         (org-table-recalculate arg)
         (when (and (string-empty-p (string-trim (org-table-get-field)))
                    (bound-and-true-p evil-local-mode))
           (evil-change-state 'insert)))

        (`babel-call
         (org-babel-lob-execute-maybe))

        (`statistics-cookie
         (save-excursion (org-update-statistics-cookies arg)))

        ((or `src-block `inline-src-block)
         (org-babel-execute-src-block arg))

        ((or `latex-fragment `latex-environment)
         (org-latex-preview arg))

        (`link
         (let* ((lineage (org-element-lineage context '(link) t))
                (path (org-element-property :path lineage)))
           (if (or (equal (org-element-property :type lineage) "img")
                   (and path (image-type-from-file-name path)))
               (+org--toggle-inline-images-in-subtree
                (org-element-property :begin lineage)
                (org-element-property :end lineage))
             (org-open-at-point arg))))

        ((guard (org-element-property :checkbox (org-element-lineage context '(item) t)))
         (org-toggle-checkbox))

        (`paragraph
         (+org--toggle-inline-images-in-subtree))

        (_
         (if (or (org-in-regexp org-ts-regexp-both nil t)
                 (org-in-regexp org-tsr-regexp-both nil  t)
                 (org-in-regexp org-link-any-re nil t))
             (call-interactively #'org-open-at-point)
           (+org--toggle-inline-images-in-subtree
            (org-element-property :begin context)
            (org-element-property :end context))))))))

    ;; (evil-define-key 'normal 'evil-org-mode "RET" #'+org/dwim-at-point)
    ;; (define-key org-mode-map (kbd "RET") #'+org/dwim-at-point)

(use-package evil
      :init ;; Execute code Before a package is loaded
      (evil-mode)
      :config ;; Execute code After a package is loaded
      (evil-set-initial-state 'eat-mode 'insert) ;; Set initial state in eat terminal to insert mode
      :custom ;; Customization of package custom variables
      (evil-want-keybinding nil)    ;; Disable evil bindings in other modes (It's not consistent and not good)
      (evil-want-C-u-scroll t)      ;; Set C-u to scroll up
      (evil-want-C-i-jump nil)      ;; Disables C-i jump
      (evil-undo-system 'undo-redo) ;; C-r to redo
      (org-return-follows-link t)   ;; Sets RETURN key in org-mode to follow links
      ;; Unmap keys in 'evil-maps. If not done, org-return-follows-link will not work
      :bind (:map evil-motion-state-map
                  ("SPC" . nil)
                  ("RET" . nil)
                  ;; ("RET" . org-todo)
                  ("TAB" . nil)))
    (use-package evil-collection
      :after evil
      :config
      ;; Setting where to use evil-collection
      ;; (setq evil-collection-mode-list '(dired ibuffer magit corfu vertico consult lsp-ui-imenu))
      (evil-set-initial-state 'package-menu-mode 'motion)
      (evil-collection-init))
    (use-package evil-commentary
      :after evil
      :config
      (evil-commentary-mode)
      )
(with-eval-after-load 'evil-maps
  (define-key evil-motion-state-map (kbd "SPC") '+org/dwim-at-point)
  (define-key evil-motion-state-map (kbd "RET") nil)
  (define-key evil-motion-state-map (kbd "TAB") nil))
;; Setting RETURN key in org-mode to follow links
  (setq org-return-follows-link  t)

(use-package general
      :config
      (general-evil-setup)
      ;; Set up 'SPC' as the leader key
      (general-create-definer start/leader-keys
        :states '(normal insert visual motion emacs)
        :keymaps 'override
        :prefix "SPC"           ;; Set leader key
        :global-prefix "C-SPC") ;; Set global leader key

      (start/leader-keys
        "." '(find-file :wk "Find file")
        ;; "TAB" '(comment-line :wk "Comment lines")
        "RET" '(consult-bookmark :wk "Bookmarks!")
        "p" '(projectile-command-map :wk "Projectile command map")
        "," '(consult-buffer :wk "Switch buffers")
        "h" '(evil-window-left :wk "Switch to left window")
        "j" '(evil-window-down :wk "Switch to lower window")
        "k" '(evil-window-up :wk "Switch to uppper window")
        "l" '(evil-window-right :wk "Switch to right window")
       )
      (start/leader-keys
        "d" '(lsp-ui-doc-show :wk "show doc"))

      (start/leader-keys
        "o a" '(org-agenda :wk "Open agenda"))
      
      (start/leader-keys
        "TAB n" '(persp-next :wk "Next workspace")
        "TAB p" '(persp-prev :wk "Previous workspace")
        "TAB d" '(persp-remove-by-name :wk "Delete workspace")
        "TAB TAB" '(persp-switch :wk "New persp"))

      (start/leader-keys
        "s b" '(consult-line :wk "Search buffer")
        "s p" '(consult-ripgrep :wk "Search project")
        )

      (start/leader-keys
        "w d" '(delete-window :wk "Close window")
        "w n" '(split-window-vertically :wk "Split window vertically")
        "w v" '(split-window-horizontally :wk "Split window horizontally")
       )
      (start/leader-keys
        "f" '(:ignore t :wk "Find")
        "f c" '((lambda () (interactive) (find-file "~/.config/emacs/config.org")) :wk "Edit emacs config")
        "f r" '(consult-recent-file :wk "Recent files")
        "f i" '(consult-imenu :wk "Imenu buffer locations"))

      (start/leader-keys
        "b" '(:ignore t :wk "Buffer Bookmarks")
        "b b" '(consult-buffer :wk "Switch buffer")
        "b d" '(kill-this-buffer :wk "Kill this buffer")
        "b i" '(ibuffer :wk "Ibuffer")
        "b n" '(next-buffer :wk "Next buffer")
        "b p" '(previous-buffer :wk "Previous buffer")
        "b r" '(revert-buffer :wk "Reload buffer")
        "b j" '(consult-bookmark :wk "Bookmark jump"))

      (start/leader-keys
        "n r f" '(org-roam-node-find :wk "Find roam nodes")
        "n r i" '(org-roam-node-insert :wk "Insert node")
        "n r D" '(org-roam-dailies-capture-today :wk "Insert node"))
	  
      (start/leader-keys
		"m d" '(org-deadline :wk "Deadline")
		"m s" '(org-schedule :wk "Schedule")
		"m t" '(org-timestamp :wk "Timestamp"))
	  
      (start/leader-keys
        "e" '(:ignore t :wk "Eglot Evaluate")
        "e e" '(eglot-reconnect :wk "Eglot Reconnect")
        "e f" '(eglot-format :wk "Eglot Format")
        "e l" '(consult-flymake :wk "Consult Flymake")
        "e b" '(eval-buffer :wk "Evaluate elisp in buffer")
        "e r" '(eval-region :wk "Evaluate elisp in region"))

      (start/leader-keys
        "g" '(:ignore t :wk "Git")
        "g g" '(magit-status :wk "Magit status"))

      (start/leader-keys
        "N i" '(citar-open :wk "Open citar")
        "N r" '(doi-insert-bibtex :wk "insert bibtex entry from DOI")
        "N c" '(orb-insert-link :wk "insert orb link"
))
      ;; (start/leader-keys
      ;;   "h" '(:ignore t :wk "Help") ;; To get more help use C-h commands (describe variable, function, etc.)
      ;;   "h q" '(save-buffers-kill-emacs :wk "Quit Emacs and Daemon"))

      (start/leader-keys
        "r r" '((lambda () (interactive)
                  (load-file "~/.config/emacs/init.el"))
                :wk "Reload Emacs config"))

      ;; (start/leader-keys
      ;;   "s" '(:ignore t :wk "Show")
      ;;   "s e" '(eat :wk "Eat terminal"))

      (start/leader-keys
        "t" '(:ignore t :wk "Toggle")
        "t t" '(visual-line-mode :wk "Toggle truncated lines (wrap)")
        "t l" '(display-line-numbers-mode :wk "Toggle line numbers")))
(add-hook 'org-mode-hook (lambda ()
       (setq-local electric-pair-inhibit-predicate
               `(lambda (c)
              (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))

(use-package emacs
  :custom
  (menu-bar-mode nil)         ;; Disable the menu bar
  (scroll-bar-mode nil)       ;; Disable the scroll bar
  (tool-bar-mode nil)         ;; Disable the tool bar
  ;;(inhibit-startup-screen t)  ;; Disable welcome screen

  (delete-selection-mode t)   ;; Select text and delete it by typing.
  (electric-indent-mode nil)  ;; Turn off the weird indenting that Emacs does by default.
  (electric-pair-mode t)      ;; Turns on automatic parens pairing

  (blink-cursor-mode nil)     ;; Don't blink cursor
  (global-auto-revert-mode t) ;; Automatically reload file and show changes if the file has changed

  ;;(dired-kill-when-opening-new-dired-buffer t) ;; Dired don't create new buffer
  (recentf-mode t) ;; Enable recent file mode

  ;;(global-visual-line-mode t)           ;; Enable truncated lines
  ;;(display-line-numbers-type 'relative) ;; Relative line numbers
  (global-display-line-numbers-mode t)  ;; Display line numbers

  (mouse-wheel-progressive-speed nil) ;; Disable progressive speed when scrolling
  (scroll-conservatively 10) ;; Smooth scrolling
  ;;(scroll-margin 8)

  (tab-width 4)

  (make-backup-files nil) ;; Stop creating ~ backup files
  (auto-save-default nil) ;; Stop creating # auto save files
  :hook
  (prog-mode . (lambda () (hs-minor-mode t))) ;; Enable folding hide/show globally
  :config
  ;; Move customization variables to a separate file and load it, avoid filling up init.el with unnecessary variables
  (setq custom-file (locate-user-emacs-file "custom-vars.el"))
  (load custom-file 'noerror 'nomessage)
  :bind (
         ([escape] . keyboard-escape-quit) ;; Makes Escape quit prompts (Minibuffer Escape)
         )
  ;; Fix general.el leader key not working instantly in messages buffer with evil mode
  :ghook ('after-init-hook
          (lambda (&rest _)
            (when-let ((messages-buffer (get-buffer "*Messages*")))
              (with-current-buffer messages-buffer
                (evil-normalize-keymaps))))
          nil nil t)
  )

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-tomorrow-night t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; treemacs theme
  ;; (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  ;; (doom-themes-treemacs-config)
  (doom-themes-org-config))

(add-to-list 'default-frame-alist '(alpha-background . 90)) ;; For all new frames henceforth

(set-face-attribute 'default nil
                    :font "CaskaydiaCove Nerd Font" ;; Set your favorite type of font or download JetBrains Mono
                    :height 120
                    :weight 'medium)
;; This sets the default font on all graphical frames created after restarting Emacs.
;; Does the same thing as 'set-face-attribute default' above, but emacsclient fonts
;; are not right unless I also add this method of setting the default font.

;;(add-to-list 'default-frame-alist '(font . "JetBrains Mono")) ;; Set your favorite font
(setq-default line-spacing 0.12)

(use-package emacs
  :bind
  ("C-+" . text-scale-increase)
  ("C--" . text-scale-decrease)
  ("<C-wheel-up>" . text-scale-increase)
  ("<C-wheel-down>" . text-scale-decrease))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 25)     ;; Sets modeline height
  (doom-modeline-bar-width 5)   ;; Sets right bar width
  (doom-modeline-persp-name t)  ;; Adds perspective name to modeline
  (lsp-modeline-diagnostics-enable nil)
  (doom-modeline-persp-icon t)) ;; Adds folder icon next to persp name

;; (use-package dashboard
;;   :ensure t
;;     :custom
;;     (dashboard-center-content t)
;;     (dashboard-icon-type 'nerd-icons)
;;     (dashboard-vertically-center-content t)
;;   :config
;;   (dashboard-setup-startup-hook))

;;     (use-package doom-dashboard
;;         ;; For Straight Users
;;         :straight (doom-dashboard :host github
;;                                     :repo "aetherspritee/doom-dashboard")
;;         ;; Or for built-in package-vc
;;         ;; :vc (:url "https://github.com/emacs-dashboard/doom-dashboard.git" :rev :newest)
;;         :after dashboard
;;         :demand t
;;         ;; Movement keys like doom.
;;         :bind
;;         (:map dashboard-mode-map
;;             ("<remap> <dashboard-previous-line>" . widget-backward)
;;             ("<remap> <dashboard-next-line>" . widget-forward)
;;             ("<remap> <previous-line>" . widget-backward)
;;             ("<remap> <next-line>"  . widget-forward)
;;             ("<remap> <right-char>" . widget-forward)
;;             ("<remap> <left-char>"  . widget-backward))
;;         :custom
;;         (dashboard-banner-logo-title "another day another try . . .")

;;         (dashboard-startup-banner "~/Stuff/nasa.svg") ; Use banner you want
;;         (dashboard-footer-icon 
;;         (nerd-icons-faicon "nf-fa-github_alt" :face 'success :height 1.5))
;;         (dashboard-page-separator "\n")
;;         (dashboard-startupify-list `(dashboard-insert-banner
;;                                     dashboard-insert-banner-title
;;                                     dashboard-insert-newline
;;                                     dashboard-insert-items
;;                                     ,(dashboard-insert-newline 2)
;;                                     dashboard-insert-init-info
;;                                     ,(dashboard-insert-newline 2)
;;                                     doom-dashboard-insert-homepage-footer))
;;         (dashboard-item-generators
;;         '((recents   . doom-dashboard-insert-recents-shortmenu)
;;             (bookmarks . doom-dashboard-insert-bookmark-shortmenu)
;;             (projects  . doom-dashboard-insert-project-shortmenu)
;;             (agenda    . doom-dashboard-insert-org-agenda-shortmenu)))
;;         (dashboard-items '(projects agenda bookmarks recents)))

(use-package projectile
  :init
  (projectile-mode)
  :custom
  (projectile-run-use-comint-mode t) ;; Interactive run dialog when running projects inside emacs (like giving input)
  (projectile-switch-project-action #'projectile-dired) ;; Open dired when switching to a project
  (projectile-project-search-path '("~/projects/" "~/work/" ("~/github" . 1)))) ;; . 1 means only search the first subdirectory level for projects
;; Use Bookmarks for smaller, not standard projects

;; (use-package eglot
;;  :ensure nil ;; Don't install eglot because it's now built-in
;;  :hook ((c-mode c++-mode ;; Autostart lsp servers for a given mode
;;                 lua-mode
;;                 python-mode
;;                 ) ;; Lua-mode needs to be installed
;;         . eglot-ensure)
;;  :custom
;;  ;; Good default
;;  (eglot-events-buffer-size 0) ;; No event buffers (Lsp server logs)
;;  (eglot-autoshutdown t);; Shutdown unused servers.
;;  (eglot-report-progress nil) ;; Disable lsp server logs (Don't show lsp messages at the bottom, java)
;;  ;; Manual lsp servers
;;  :config
;;  (add-to-list 'eglot-server-programs
;;               ;; `(lua-mode . ("PATH_TO_THE_LSP_FOLDER/bin/lua-language-server" "-lsp"))) ;; Adds our lua lsp server to eglot's server list
;;               `(python-mode . ("usr/bin/pyright" "-lsp"))) ;; Adds our lua lsp server to eglot's server list
;;  )

(use-package lsp-ui :commands lsp-ui-mode)
    (use-package lsp-mode
      :custom
        (lsp-completion-provider :none) ;; we use Corfu!
      :init
      ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
      (setq lsp-keymap-prefix "C-c l")
      (defun my/orderless-dispatch-flex-first (_pattern index _total) (and (eq index 0) 'orderless-flex))
      (defun my/lsp-mode-setup-completion ()
        (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults)) '(orderless))
        (add-hook 'orderless-style-dispatchers #'my/orderless-dispatch-flex-first nil 'local)
        (setq-local completion-at-point-functions (list (cape-capf-buster #'lsp-completion-at-point))))

      :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
             (python-mode . lsp)
             ;; if you want which-key integration
             (lsp-mode . lsp-enable-which-key-integration)
             (lsp-mode . lsp-ui-mode)
             (lsp-completion-mode . my/lsp-mode-setup-completion)
            )
      :commands lsp)

    ;; optionally
    ;; if you are helm user
    ;; (use-package helm-lsp :commands helm-lsp-workspace-symbol)
    ;; if you are ivy user
    ;; (use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
    ;; (use-package lsp-treemacs :commands lsp-treemacs-errors-list)

    ;; optionally if you want to use debugger
    (use-package dap-mode)
    ;; (use-package dap-LANGUAGE) to load the dap adapter for your language
(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))  ; or lsp-deferred
(setq lsp-ui-doc-position 'at-point)

(use-package yasnippet-snippets
  :hook (prog-mode . yas-minor-mode))

(use-package pyvenv)

(use-package org
  :ensure nil
  :custom
  (org-edit-src-content-indentation 4) ;; Set src block automatic indent to 4 instead of 2.

  :hook
  (org-mode . org-indent-mode) ;; Indent text

  ;; The following prevents <> from auto-pairing when electric-pair-mode is on.
  ;; Otherwise, org-tempo is broken when you try to <s TAB...
  ;;(org-mode . (lambda ()
  ;;              (setq-local electric-pair-inhibit-predicate
  ;;                          `(lambda (c)
  ;;                             (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))
  )
  (setq org-hide-emphasis-markers t)
(with-no-warnings
(custom-declare-face '+org-todo-active  '((t (:inherit (bold font-lock-constant-face org-todo)))) "")
(custom-declare-face '+org-todo-project '((t (:inherit (bold font-lock-doc-face org-todo)))) "")
(custom-declare-face '+org-todo-onhold  '((t (:inherit (bold warning org-todo)))) "")
(custom-declare-face '+org-todo-cancel  '((t (:inherit (bold error org-todo)))) ""))
    (setq org-todo-keywords
            '((sequence
            "TODO(t)"
            "CURR(c)"                             ; A task that needs doing & is ready to do
            "PROJ(p)"  ; A project, which usually contains other tasks
            "WORK(u)"
            "PRCS(v)"                             ; A recurring task
            "STRT(s)"
            "THNK(n)"                             ; A task that is in progress
            "WAIT(w)"  ; Something external is holding up this task
            "HOLD(h)"  ; This task is paused/on hold because of me
            "IDEA(i)"  ; An unconfirmed and unapproved task or notion
            "|"
            "DONE(d)"  ; Task successfully completed
            "KILL(k)") ; Task was cancelled, aborted or is no longer applicable
            (sequence
            "[ ](T)"   ; A task that needs doing
            "[-](S)"   ; Task is in progress
            "[?](W)"   ; Task is being held up or paused
            "|"
            "[X](D)")  ; Task was completed
            (sequence
            "|"
            "OKAY(o)"
            "YES(y)"
            "NO(n)"))
            org-todo-keyword-faces
            '(("[-]"  . +org-todo-active)
            ("STRT" . +org-todo-active)
            ("[?]"  . +org-todo-onhold)
            ("WAIT" . +org-todo-onhold)
            ("HOLD" . +org-todo-onhold)
            ("PROJ" . +org-todo-project)
            ("NO"   . +org-todo-cancel)
            ("KILL" . +org-todo-cancel)))

(setq org-agenda-files '("~/Dropbox/Orga/"))
(setq org-agenda-custom-commands
    '(
        ("D" "Meine Agenda"
        ((todo "THNK|HOLD"
                (
                (org-agenda-overriding-header " REMINDER\n")
                ))
        (agenda " "
                (
                (org-agenda-overriding-header " SOOOON\n")
                (org-agenda-span 30)
                (org-agenda-start-day "+0d")
                (org-agenda-show-all-dates nil)
                (org-agenda-entry-types '(:deadline))
                (org-deadline-warning-days 0)
                ))
        (agenda " "
                (
                (org-agenda-overriding-header " Day\n")
                (org-agenda-span 1)
                (org-agenda-start-day "+0d")
                (org-deadline-warning-days 0)
                (org-agenda-day-face-function (lambda (date) 'org-agenda-date))
                ))
        (agenda " "
                ((org-agenda-overriding-header "󰎕 Tomorrow\n")
                (org-agenda-start-day "+1d")
                (org-agenda-span 1)
                (org-agenda-show-all-dates nil)
                ))
        (todo "PROJ"
                ((org-agenda-overriding-header "󰀸 Projects i want to do :]\n")))
        (agenda " "
                ((org-agenda-overriding-header " ÜBERMORGEN\n")
                (org-agenda-start-day "+2d")
                (org-agenda-span 1)
                (org-agenda-show-all-dates nil)
                ))
        (todo "CURR"
                ((org-agenda-overriding-header " Current projects\n")))
        ))
        ))

(use-package toc-org
  :commands toc-org-enable
  :hook (org-mode . toc-org-mode))

(use-package org-modern
    :after org
    :ensure t
    :custom
    (org-modern-hide-stars nil)		; adds extra indentation
    (org-modern-table nil)
    (org-modern-list 
    '(;; (?- . "-")
        (?* . "•")
        (?+ . "‣")))
    (org-modern-block-name '("" . "")) ; or other chars; so top bracket is drawn promptly
    :hook
    (org-mode . org-modern-mode)
    (org-agenda-finalize . org-modern-agenda))
    (use-package org-modern-indent
	  :after org
      :straight (org-modern-indent :type git :host github :repo "jdtsmith/org-modern-indent")
      :config ; add late to hook
      (add-hook 'org-mode-hook #'org-modern-indent-mode 90))

(use-package org-bullets-mode
  :ensure org-bullets
  :config
  :hook org-mode)

(use-package org-tempo
  :ensure nil
  :after org)

(use-package org-autolist
  :after org
  :hook (org-mode . org-autolist-mode))

(use-package evil-org
   :hook (org-mode . evil-org-mode)
   :hook (org-capture-mode . evil-insert-state)
   :hook (doom-docs-org-mode . evil-org-mode)
   :after org
   :init
   (defvar evil-org-retain-visual-state-on-shift t)
   (defvar evil-org-special-o/O '(org-todo))
   (defvar evil-org-use-additional-insert t)
   :config
   (add-hook 'evil-org-mode-hook #'evil-normalize-keymaps)
   ;; (evil-org-set-key-theme)
   (setq evil-org-key-theme '(textobjects return navigation additional insert todo))
(require 'evil-org-agenda)
(evil-org-agenda-set-keys)
    (evil-define-key 'normal evil-org-mode-map
      (kbd "-") 'org-ctrl-c-minus
      (kbd "|") 'org-table-goto-column
      (kbd "M-o") (evil-org-define-eol-command org-insert-heading)
      (kbd "M-t") (evil-org-define-eol-command org-insert-todo)
      (kbd "C-RETURN")   '+org/insert-item-below
      (kbd "C-S-RETURN") '+org/insert-item-above
      (kbd "RETURN") '+org/dwim-at-point
      (kbd "RET") '+org/dwim-at-point
      ))


 ;; (use-package evil-org-agenda
 ;;   :hook (org-agenda-mode . evil-org-agenda-mode)
 ;;   :config
 ;;   (evil-org-agenda-set-keys))

(use-package org-roam
		  :straight (org-roam :type git :host github :repo "org-roam/org-roam" :commit "ca873f7")
          :ensure t
          :custom
          (org-roam-directory (file-truename "~/Roam/"))
          :config
          (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
          (org-roam-db-autosync-mode))

    (use-package org-ref)
    (use-package citar
      :custom
      (citar-bibliography '("~/Roam/papers/lib.bib"))
      (citar-library-paths '("~/Roam/papers/"))
      (citar-notes-paths '("~/Roam/master/" "~/Roam/uni/" "~/Roam/notes/"))
      :hook
        (LaTeX-mode . citar-capf-setup)
        (org-mode . citar-capf-setup)
    )
    (setq citar-file-open-functions '(("pdf" . citar-file-open-external)))

    (use-package bibtex-completion)
    (setq bibtex-completion-library-path '("~/Roam/master/"))
    (setq bibtex-completion-bibliography "~/Roam/papers/lib.bib")
    (setq bibtex-completion-notes-path "~/Roam/master/")
    (use-package citar-org-roam
    :after (citar org-roam)
    :config (citar-org-roam-mode)
    (setq citar-org-roam-capture-template-key "n")
)
    (use-package org-roam-bibtex
    :after (org-roam citar)
    :hook (org-roam-mode . org-roam-bibtex-mode)
    :config
    (require 'org-ref)
    (require 'citar-org-roam)
    (citar-register-notes-source
    'orb-citar-source (list :name "Org-Roam Notes"
            :category 'org-roam-node
            :items #'citar-org-roam--get-candidates
            :hasitems #'citar-org-roam-has-notes
            :open #'citar-org-roam-open-note
            :create #'orb-citar-edit-note
            :annotate #'citar-org-roam--annotate))

    (setq citar-notes-source 'orb-citar-source)
    (setq orb-roam-ref-format 'org-cite)
    (setq org-roam-bibtex-mode t)
    ) 
(setq org-roam-capture-templates
  '(("d" "default" plain
     "%?"
     :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
     :unnarrowed t)
  ("m" "master" plain (file "~/Roam/test/templates/stuff.org")
     :target (file+head "master/%<%Y%m%d%H%M%S>-${citekey}.org" "#+title: ${citekey}\n#+description: ${title}\n#+filetags: :uni:MA:\n\n* Summary\n\n* Further Reading")
     :unnarrowed t)
  ("u" "uni" plain
   "#+STARTUP: latexpreview\n %?"
   :target (file+head "uni/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
   :unnarrowed t)
  ("n" "ref + noter" plain
   (file "~/Roam/test/templates/noternotes.org")
   :target (file+head "~/Roam/notes/${citekey}.org" "#+title: ${citekey}\n* ${title}\n")
   :unnarrowed t
   )
  ("l" "lotr" plain
   "\n\n* Summary\n%?"
   :if-new (file+head "~/Roam/Lord of the Rings/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
   :unnarrowed t)
  ))
(setq org-roam-dailies-capture-templates
    '(("w" "Weekly" entry "* Thought of the week\n %?\n* Review \n\n* What to keep up\n\n* What to improve\n\n* Vibes/Mood/Interest"
        :if-new (file+head "weekly/weekly-%<%d-%m-%Y>.org" "#+title: Weekly: %<%d-%m-%Y>\n"))
        ("d" "Daily" entry "* %?" :if-new (file+head  "daily-%<%d-%m-%Y>.org" "#+title: %<%d-%m-%Y>\n"))
    )
)
(setq org-link-frame-setup (quote
                           ((vm . vm-visit-folder)
                            (vm-imap . vm-visit-imap-folder)
                            (gnus . gnus)
                            (file . find-file)
                            (wl . wl)))
                          )

(use-package eat
  :hook ('eshell-load-hook #'eat-eshell-mode))

;; (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; (require 'start-multiFileExample)

;; (start/hello)

(use-package nerd-icons
  :if (display-graphic-p))

(use-package nerd-icons-dired
  :hook (dired-mode . (lambda () (nerd-icons-dired-mode t))))

(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package magit
  :commands magit-status)

(use-package diff-hl
  :hook ((dired-mode         . diff-hl-dired-mode-unless-remote)
         (magit-pre-refresh  . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :init (global-diff-hl-mode))

(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-auto-prefix 2)          ;; Minimum length of prefix for auto completion.
  (corfu-popupinfo-mode t)       ;; Enable popup information
  (corfu-popupinfo-delay 0.5)    ;; Lower popupinfo delay to 0.5 seconds from 2 seconds
  (corfu-separator ?\s)          ;; Orderless field separator, Use M-SPC to enter separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin
  (completion-ignore-case t)
  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)
  (corfu-preview-current nil) ;; Don't insert completion without confirmation
  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :init
  (global-corfu-mode))

(use-package nerd-icons-corfu
  :after corfu
  :init (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package cape
     :after corfu
     :init
     ;; Add to the global default value of `completion-at-point-functions' which is
     ;; used by `completion-at-point'.  The order of the functions matters, the
     ;; first function returning a result wins.  Note that the list of buffer-local
     ;; completion functions takes precedence over the global list.
     ;; The functions that are added later will be the first in the list

     (add-to-list 'completion-at-point-functions #'cape-dabbrev) ;; Complete word from current buffers
     (add-to-list 'completion-at-point-functions #'cape-dict) ;; Dictionary completion
     (add-to-list 'completion-at-point-functions #'cape-file) ;; Path completion
     (add-to-list 'completion-at-point-functions #'cape-elisp-block) ;; Complete elisp in Org or Markdown mode
     (add-to-list 'completion-at-point-functions #'cape-keyword) ;; Keyword/Snipet completion

     ;;(add-to-list 'completion-at-point-functions #'cape-abbrev) ;; Complete abbreviation
     ;;(add-to-list 'completion-at-point-functions #'cape-history) ;; Complete from Eshell, Comint or minibuffer history
     ;;(add-to-list 'completion-at-point-functions #'cape-line) ;; Complete entire line from current buffer
     ;;(add-to-list 'completion-at-point-functions #'cape-elisp-symbol) ;; Complete Elisp symbol
     ;;(add-to-list 'completion-at-point-functions #'cape-tex) ;; Complete Unicode char from TeX command, e.g. \hbar
     ;;(add-to-list 'completion-at-point-functions #'cape-sgml) ;; Complete Unicode char from SGML entity, e.g., &alpha
     ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345) ;; Complete Unicode char using RFC 1345 mnemonics
     )

;;   (setq-local lsp-mode completion-at-point-functions (list (cape-capf-buster #'cape:lsp-cape) #'cape-file))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package vertico
      :init
      (vertico-mode)
      :bind (
       :map vertico-map
        ("C-j" . vertico-next)
        ("C-k" . vertico-previous)))

;; Configure directory extension.
(use-package vertico-directory
  :after vertico
  :ensure nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-char))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

    (savehist-mode) ;; Enables save history mode

    (use-package marginalia
      :after vertico
      :init
      (marginalia-mode))

    (use-package nerd-icons-completion
      :after marginalia
      :config
      (nerd-icons-completion-mode)
      :hook
      ('marginalia-mode-hook . 'nerd-icons-completion-marginalia-setup))

(use-package embark
  :ensure t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command))

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package consult
  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))

  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  ;; (consult-customize
  ;; consult-theme :preview-key '(:debounce 0.2 any)
  ;; consult-ripgrep consult-git-grep consult-grep
  ;; consult-bookmark consult-recent-file consult-xref
  ;; consult--source-bookmark consult--source-file-register
  ;; consult--source-recent-file consult--source-project-recent-file
  ;; :preview-key "M-."
  ;; :preview-key '(:debounce 0.4 any))

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
   ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
   ;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
   ;;;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
   ;;;; 4. projectile.el (projectile-project-root)
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-function (lambda (_) (projectile-project-root)))
   ;;;; 5. No project support
  ;; (setq consult-project-function nil)
  )

;;     (use-package company)
;; (add-hook 'after-init-hook 'global-company-mode)

(use-package diminish)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init
  (which-key-mode 1)
  :diminish
  :custom
  (which-key-side-window-location 'bottom)
  (which-key-sort-order #'which-key-key-order-alpha) ;; Same as default, except single characters are sorted alphabetically
  (which-key-sort-uppercase-first nil)
  (which-key-add-column-padding 1) ;; Number of spaces to add to the left of each column
  (which-key-min-display-lines 6)  ;; Increase the minimum lines to display, because the default is only 1
  (which-key-idle-delay 0.8)       ;; Set the time delay (in seconds) for the which-key popup to appear
  (which-key-max-description-length 25)
  (which-key-allow-imprecise-window-fit nil)) ;; Fixes which-key window slipping out in Emacs Daemon

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))
;; Increase the amount of data which Emacs reads from the process
(setq read-process-output-max (* 1024 1024)) ;; 1mb

(with-eval-after-load 'evil-maps
  (define-key evil-motion-state-map (kbd "RET") #'+org/dwim-at-point)
  )

(use-package latex
  :ensure auctex
  :hook ...)
(setq org-highlight-latex-and-related '(latex script entities))
(plist-put org-format-latex-options :scale 1.5)

(use-package enlight
     :init
     (unless (package-installed-p 'enlight)
       (package-vc-install
        '(enlight
          :vc-backend Git
          :url "https://github.com/ichernyshovvv/enlight"
          :branch "master"))))   

       (use-package grid
         :init
         (unless (package-installed-p 'grid)
           (package-vc-install
            '(grid
              :vc-backend Git
              :url "https://github.com/ichernyshovvv/grid.el"
              :branch "master"))))

       (defvar enlight-lipsum "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.

       Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.")

       (defface enlight-yellow-bold
         '((t (:foreground "#cabf00" :bold t)))
         "Yellow bold face")

       (defvar enlight-guix
         (propertize

        ;;   " ..                             `.
        ;; `--..```..`           `..```..--`   
        ;;   .-:///-:::.       `-:::///:-.     
        ;;      ````.:::`     `:::.````        
        ;;           -//:`    -::-             
        ;;            ://:   -::-              
        ;;            `///- .:::`              
        ;;             -+++-:::.               
        ;;              :+/:::-                
        ;;              `-....`                "

            "
8b,dPPYba,  ,adPPYYba, ,adPPYba, ,adPPYYba,  
88P'   `\8a ''     `Y8 I8[    '' ''     `Y8  
88       88 ,adPPPPP88  `*Y8ba,  ,adPPPPP88  
88       88 88,    ,88 aa    ]8I 88,    ,88  
88       88 `*8bbdP'Y8 `*YbbdP*' `*8bbdP'Y8  

            "
          'face 'enlight-yellow-bold))

       (defvar enlight-guix-widget
         `( :content ,(concat "\n" (propertize "Block 1" 'face 'enlight-yellow-bold)
                      "\nGUIX MANAGEMENT WIDGET\n\n")
            :width 22 :border t :align center :padding 2))

       (defvar enlight-email-width
         `( :content
            ,(concat "\n" (propertize (format "%s" (nerd-icons-octicon "nf-oct-clock")) 'face 'enlight-yellow-bold)
                 "\n"(current-time-string)"\n\n")
            :padding 2 :width 22 :align center :border t))

       (defvar enlight-weather-width
         `( :content
            ,(concat "\n" (propertize "Block 3" 'face 'enlight-yellow-bold)
                 "\nWEATHER WIDGET\n\n")
            :padding 2 :width 22 :border t :align center))

       (defvar enlight-calendar
         (progn
           (calendar)
           (diary-mark-entries)
           (prog1 (with-current-buffer (buffer-name (current-buffer))
                (buffer-string))
             (calendar-exit))))

       (use-package enlight
         :custom
         (enlight-content
          (concat
           (grid-get-box `( :align center :content ,enlight-guix :width 80))
           (grid-get-row
            (list
             (grid-get-box
              (concat
           (grid-get-box
            `( :content
               ,(concat
                 (grid-get-box `( :content ,(propertize "another day another try . . ." 'face 'enlight-yellow-bold)
                          :width 80 :align center))
                 (grid-get-row
                  `(,enlight-guix-widget
                "     "
                ,enlight-email-width
                "     "
                ,enlight-weather-width)))
               :width 80))
           enlight-calendar "\n"
           (grid-get-row
            `(,(concat
                (propertize "MENU" 'face 'highlight)
                "\n"
                (enlight-menu
                 '(("Org Mode"
                ("Org-Agenda " (org-agenda nil "D") "D"))
               ("Downloads"
                ;; ("Transmission" transmission "t")
                ("Downloads folder" (dired "~/Downloads") "a"))
               ("Other"
                ("Bookmarks" consult-bookmark "b")))))
              ,(grid-get-column
            `(,(concat
                (propertize "Files" 'face 'highlight)
                "\n"
                (enlight-menu
                 '(("Roam"
                ("MA Hub" (find-file "~/Roam/master/20231129173749-ma_hub.org") "M")
                ("Virga Yasf" (find-file "~/Roam/uni/20240311092511-integrate_yasf_into_virga.org") "Y"))
               ("Code"
                ("master" (dired "~/Code/master/") "M"))
               ("Other"
                ("Bookmarks" consult-bookmark "b")))))
                )))))))))))


(setopt initial-buffer-choice #'enlight)

(use-package persp-mode
  :demand t
  :config
  (setq persp-auto-resume-time -1 ;; No autoload buffers
        ;; persp-set-last-persp-for-new-frames nil
        ;; persp-reset-windows-on-nil-window-conf t
        persp-autokill-buffer-on-remove t
        ;; persp-add-buffer-on-after-change-major-mode t
        persp-kill-foreign-buffer-behaviour 'kill)
  (persp-mode 1))

;; (setq persp-hook-up-emacs-buffer-completion t)

(use-package hl-todo
  :hook ((org-mode . hl-todo-mode)
         (prog-mode . hl-todo-mode))
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        `(("TODO"       warning bold)
          ("FIXME"      error bold)
          ("HACK"       font-lock-constant-face bold)
          ("REVIEW"     font-lock-keyword-face bold)
          ("NOTE"       success bold)
          ("DEPRECATED" font-lock-doc-face bold))))
