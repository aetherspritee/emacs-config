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
          (evil-collection-init)
        (setq evil-collection-want-find-usages-bindings t)
)
        (use-package evil-commentary
          :after evil
          :config
          (evil-commentary-mode)
          )
    (with-eval-after-load 'evil-maps
      (define-key evil-motion-state-map (kbd "SPC") '+org/dwim-at-point)
      (define-key evil-motion-state-map (kbd "RET") nil)
      (define-key evil-motion-state-map (kbd "TAB") nil)
      (define-key evil-motion-state-map (kbd "g r") 'lsp-find-references))
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
            "," '(persp-switch-to-buffer :wk "Switch buffers")
            "h" '(evil-window-left :wk "Switch to left window")
            "j" '(evil-window-down :wk "Switch to lower window")
            "k" '(evil-window-up :wk "Switch to uppper window")
            "l" '(evil-window-right :wk "Switch to right window")
           )
          (start/leader-keys
            "d" '(lsp-ui-doc-show :wk "show doc"))

          (start/leader-keys
            ;; "H" '(enlight-open :wk "show dashboard"))
            "H" '(dashboard-open :wk "show dashboard"))

          (start/leader-keys
            "o a" '(org-agenda :wk "Open agenda")
            "o n" '(treemacs :wk "Treemacs")
            "o b" '(org-timeblock :wk "Org timeblock")
            )

        (start/leader-keys
            "c c" '(compile :wk "compile")
            "c k" '(kill-compilation :wk "kill compilation")
            "c C" '(recompile :wk "kill compilation")
            "c s" '(lsp-treemacs-symbols :wk "treemacs symbols"))

          (start/leader-keys
            "TAB n" '(+workspace/swap-right :wk "Next workspace")
            "TAB p" '(+workspace/swap-left :wk "Previous workspace")
            "TAB d" '(+workspace/delete :wk "Delete workspace")
            "1" '((lambda () (interactive) (+workspace/switch-to 0)) :wk "Switch to workspace 0")
            "2" '((lambda () (interactive) (+workspace/switch-to 1)) :wk "Switch to workspace 1")
            "3" '((lambda () (interactive) (+workspace/switch-to 2)) :wk "Switch to workspace 2")
            "4" '((lambda () (interactive) (+workspace/switch-to 3)) :wk "Switch to workspace 3")
            "5" '((lambda () (interactive) (+workspace/switch-to 4)) :wk "Switch to workspace 4")
            "6" '((lambda () (interactive) (+workspace/switch-to 5)) :wk "Switch to workspace 5")
            "TAB TAB" '(+workspace/new :wk "New persp"))

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
            "b r" '(my/raise-popup :wk "Raise popup buffer")
            "b s" '(bookmark-set :wk "Set bookmark")
            "b j" '(consult-bookmark :wk "Bookmark jump"))

          (start/leader-keys
            "n r f" '(org-roam-node-find :wk "Find roam nodes")
            "n r i" '(org-roam-node-insert :wk "Insert node")
            "n r D" '(org-roam-dailies-capture-today :wk "Insert node"))

          (start/leader-keys
            "m d" '(org-deadline :wk "Deadline")
            "m s" '(org-schedule :wk "Schedule")
            "m t" '(org-time-stamp :wk "Timestamp"))

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
            "N c" '(org-cite-insert :wk "insert orb link"
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

    ;; (add-hook prog-mode-hook
    ;; (lambda ()
    ;; (local-set-key "g r" 'lsp-find-references)))

    (add-hook 'org-mode-hook (lambda ()
           (setq-local electric-pair-inhibit-predicate
                   `(lambda (c)
                  (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))

(setq initial-frame-alist (append initial-frame-alist '((left . 75) (top . 75) (width . 240) (height . 73 ))))

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
  (display-line-numbers-type 'relative) ;; Relative line numbers
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
  ;; (load-theme 'doom-tomorrow-night t)
  (load-theme 'doom-solarized-light t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; treemacs theme
  ;; (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  ;; (doom-themes-treemacs-config)
  (doom-themes-org-config))

(add-to-list 'default-frame-alist '(alpha-background . 100)) ;; For all new frames henceforth

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

(defun in-git-p ()
              (not (string-match "^fatal" (shell-command-to-string "git rev-parse --git-dir"))))
            (defun git-parse-status ()
              (interactive)
              (concat 
            " ["
            (let ((plus-minus (vc-git--run-command-string
                       buffer-file-name "diff" "--numstat" "--")))
              (if (and plus-minus
                   (string-match "^\\([0-9]+\\)\t\\([0-9]+\\)\t" plus-minus))
                   (concat
                (propertize (format "+%s " (match-string 1 plus-minus)) 'face 'nerd-icons-green)
                (propertize (format "-%s" (match-string 2 plus-minus)) 'face 'error))
                (propertize "✔" 'face '(:foreground "green3" :weight bold))))
            "]"))

         (defun git-remote-status ()
          (interactive)
          (let* (;; get the branch we are on.
                 (branch (s-trim
                          (shell-command-to-string
                           "git rev-parse --abbrev-ref HEAD")))
                 ;; get the remote the branch points to.
                 (remote (s-trim
                          (shell-command-to-string
                           (format "git config branch.%s.remote" branch))))
                 (remote-branch (s-trim
                                 (shell-command-to-string
                                  "git for-each-ref --format='%(upstream:short)' $(git symbolic-ref -q HEAD)")))
                 (commits (split-string
                           (s-trim
                            (shell-command-to-string
                             (format
                              "git rev-list --count --left-right HEAD...%s"
                              remote-branch)))))
                 (local (nth 0 commits))
                 (remotes (nth 1 commits)))
            (concat
             (propertize (format "%s " (nerd-icons-octicon "nf-oct-git_branch")) 'face 'doom-modeline-project-dir)
             (propertize (format "%s " branch) 'face 'doom-modeline-project-dir)
             (propertize "[" 'face 'default)
             (propertize (format "↑%s" local) 'face 'warning)
             (propertize "|" 'face 'default)
             (propertize (format "↓%s" remotes) 'face 'warning)
             ;; (format "↑%s|↓%s" local remotes)
             (propertize "]" 'face 'default)
             )))
                (use-package doom-modeline
                  :custom
                  (doom-modeline-height 25)     ;; Sets modeline height
                  (doom-modeline-bar-width 5)   ;; Sets right bar width
                  (doom-modeline-persp-name t)  ;; Adds perspective name to modeline
                  (lsp-modeline-diagnostics-enable nil)
                  (doom-modeline-persp-icon t) ;; Adds folder icon next to persp name
                  (doom-modeline-env-enable-python t)
                  (doom-modeline-modal-icon nil)
                  ;; (doom-modeline-vcs-max-length 0)
                  :config
                  (doom-modeline-def-segment my-vcs
                    (concat
                    (git-remote-status)
                    (git-parse-status)
                    ))

            (add-hook 'doom-modeline-mode-hook (lambda () (doom-modeline-set-modeline 'my-simple-line 'default)))
            ;; (add-hook 'doom-modeline-mode-hook (lambda () (doom-modeline-set-modeline 'my-simple-line 'default)))
            (add-hook 'doom-modeline-mode-hook 'setup-doom-modeline-evil-states)
            (doom-modeline-def-modeline 'my-simple-line
              '(bar matches modals buffer-info remote-host buffer-position parrot selection-info)
              '(my-vcs misc-info minor-modes major-mode process check))
            (add-hook 'doom-modeline-mode-hook (lambda () (doom-modeline-set-modeline 'my-simple-line 'default)))
            ;; Set default mode-line

    (setq doom-modeline-modal-icon nil
          evil-normal-state-tag   (propertize " Normal ")
          evil-emacs-state-tag    (propertize " Emacs " )
          evil-insert-state-tag   (propertize " Insert ")
          evil-motion-state-tag   (propertize " Motion ")
          evil-visual-state-tag   (propertize " Visual ")
          evil-operator-state-tag (propertize " Operator "))

    (defun setup-doom-modeline-evil-states () ;; setting up colors
      (set-face-attribute 'doom-modeline-evil-normal-state nil   :background "green3"  :foreground "black")
      (set-face-attribute 'doom-modeline-evil-emacs-state nil    :background "orange3" :foreground "black")
      (set-face-attribute 'doom-modeline-evil-insert-state nil   :background "red3"    :foreground "white")
      (set-face-attribute 'doom-modeline-evil-motion-state nil   :background "blue3"   :foreground "white")
      (set-face-attribute 'doom-modeline-evil-visual-state nil   :background "pink3" :foreground "black")
      (set-face-attribute 'doom-modeline-evil-operator-state nil :background "purple3"))

    :hook
    (after-init . doom-modeline-mode)
)

(defun my/doom-dashboard-insert-recents-shortmenu (&rest _)
      "Insert recent files short menu widget."
      (let* ((fn (alist-get 'recents doom-dashboard-shortmenu-functions))
             (fn-keymap (format "\\[%s]" fn))
             (icon-name (alist-get 'recents dashboard-heading-icons))
             (icon (nerd-icons-octicon icon-name :face 'dashboard-heading)))
        (if dashboard-display-icons-p
            (insert (format "%-1s   " icon)))
        (widget-create 'item
                       :tag (format "%-30s" "Recently opened files")
                       :action (lambda (&rest _)
                                 (call-interactively 
                                  (alist-get 'recents doom-dashboard-shortmenu-functions)))
                       :mouse-face 'highlight
                       :button-face 'dashboard-heading
                       :button-prefix ""
                       :button-suffix ""
                       :format "%[%t%]")
        (if doom-dashboard-set-widget-binding
            (insert (propertize "SPC f r"
                                'face
                                'doom-dashboard-bindings-face)))))

(defun my/doom-dashboard-insert-org-agenda-shortmenu (&rest _)
  "Insert `org-agenda' shortmenu widget."
  (let* ((fn (alist-get 'agenda doom-dashboard-shortmenu-functions))
         (fn-keymap (format "\\[%s]" fn))
         (icon-name (alist-get 'agenda dashboard-heading-icons))
         (icon (nerd-icons-octicon icon-name :face 'dashboard-heading)))
    (if dashboard-display-icons-p
        (insert (format "%-1s   " icon)))
    (widget-create 'item
                   :tag (format "%-30s" "Open org-agenda")
                   :action (lambda (&rest _)
                             (call-interactively 
                              (alist-get 'agenda doom-dashboard-shortmenu-functions)))
                   :mouse-face 'highlight
                   :button-face 'dashboard-heading
                   :button-prefix ""
                   :button-suffix ""
                   :format "%[%t%]")
    (if doom-dashboard-set-widget-binding
        (insert (propertize "SPC o a"
                            'face
                            'doom-dashboard-bindings-face)))))


(defun my/doom-dashboard-insert-bookmark-shortmenu (&rest _)
  "Insert bookmark shortmenu widget."
  (let* ((fn (alist-get 'bookmarks doom-dashboard-shortmenu-functions))
         (fn-keymap (format "\\[%s]" fn))
         (icon-name (alist-get 'bookmarks dashboard-heading-icons))
         (icon (nerd-icons-octicon icon-name :face 'dashboard-heading)))
    (if dashboard-display-icons-p
        (insert (format "%-1s   " icon)))
    (widget-create 'item
                   :tag (format "%-30s" "Jump to bookmark")
                   :action (lambda (&rest _)
                             (call-interactively 
                              (alist-get 'bookmarks doom-dashboard-shortmenu-functions)))
                   :mouse-face 'highlight
                   :button-face 'dashboard-heading
                   :button-prefix ""
                   :button-suffix ""
                   :format "%[%t%]")
    (if doom-dashboard-set-widget-binding
        (insert (propertize "SPC RET"
                            'face
                            'doom-dashboard-bindings-face)))))   

    (use-package dashboard
      :ensure t
        :custom
        (dashboard-center-content t)
        (dashboard-icon-type 'nerd-icons)
        (dashboard-vertically-center-content t)
      :config
      (dashboard-setup-startup-hook))

        (use-package doom-dashboard
            ;; For Straight Users
            :straight (doom-dashboard :host github
                                        :repo "aetherspritee/doom-dashboard")
            ;; Or for built-in package-vc
            ;; :vc (:url "https://github.com/emacs-dashboard/doom-dashboard.git" :rev :newest)
            :after dashboard
            :demand t
            ;; Movement keys like doom.
            :bind
            (:map dashboard-mode-map
                ("<remap> <dashboard-previous-line>" . widget-backward)
                ("<remap> <dashboard-next-line>" . widget-forward)
                ("<remap> <previous-line>" . widget-backward)
                ("<remap> <next-line>"  . widget-forward)
                ("<remap> <right-char>" . widget-forward)
                ("<remap> <left-char>"  . widget-backward))
            :custom
            (dashboard-banner-logo-title "another day another try . . .")

            (dashboard-startup-banner "~/Stuff/nasa.svg") ; Use banner you want
            (dashboard-footer-icon 
            (nerd-icons-faicon "nf-fae-planet" :face 'success :height 1.5))
            (dashboard-page-separator "\n")
            (dashboard-startupify-list `(dashboard-insert-banner
                                        dashboard-insert-newline
                                        dashboard-insert-banner-title
                                        doom-dashboard-insert-homepage-footer
                                        dashboard-insert-newline
                                        dashboard-insert-items
                                        ,(dashboard-insert-newline 2)
                                        dashboard-insert-init-info
                                        ,(dashboard-insert-newline 2)
                                        ))
            (dashboard-item-generators
            '((recents   . my/doom-dashboard-insert-recents-shortmenu)
                (bookmarks . my/doom-dashboard-insert-bookmark-shortmenu)
                (projects  . doom-dashboard-insert-project-shortmenu)
                (agenda    . my/doom-dashboard-insert-org-agenda-shortmenu)))
            (dashboard-items '(agenda bookmarks recents)))

(use-package projectile
  :init
  (projectile-mode)
  :custom
  (projectile-run-use-comint-mode t) ;; Interactive run dialog when running projects inside emacs (like giving input)
  (projectile-switch-project-action #'projectile-dired) ;; Open dired when switching to a project
  (projectile-project-search-path '("~/projects/" "~/work/" ("~/github" . 1)))) ;; . 1 means only search the first subdirectory level for projects
;; Use Bookmarks for smaller, not standard projects

(use-package lsp-ui :commands lsp-ui-mode)
    (use-package lsp-mode
      :commands (lsp lsp-deferred)
    :init
    (defun my/update-completions-list ()
        (progn
            (fset 'non-greedy-lsp (cape-capf-properties #'lsp-completion-at-point :exclusive 'no))
            (setq completion-at-point-functions
                '(non-greedy-lsp cape-file cape-dabbrev))))

      (setq lsp-keymap-prefix "C-c l")

      :hook (
             (python-mode . lsp)
             (c-mode . lsp)
             (f90-mode . lsp)
             (julia-mode . lsp)
             (go-mode . lsp)
             (lsp-mode . lsp-enable-which-key-integration)
             (lsp-mode . lsp-ui-mode)
             (lsp-completion-mode . my/update-completions-list)
            )
      :custom
        (lsp-completion-provider :none)) ;; we use Corfu!

    (use-package dap-mode)
    ;; (use-package dap-LANGUAGE) to load the dap adapter for your language
(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))  ; or lsp-deferred
(setq lsp-ui-doc-position 'at-point)
(setq lsp-ui-sideline-show-hover nil)
(setq lsp-ui-sideline-enable t)
(setq lsp-ui-sideline-show-diagnostics t)

(use-package yasnippet-snippets
  :hook (prog-mode . yas-minor-mode))

(defun +modeline-update-env-in-all-windows-h (&rest _)
  "Update version strings in all buffers."
  (dolist (window (window-list))
    (with-selected-window window
      (when (fboundp 'doom-modeline-update-env)
        (doom-modeline-update-env))
      (force-mode-line-update))))

    (use-package pyvenv
  :after python
  :init
 (add-hook 'pyvenv-post-activate-hooks #'+modeline-update-env-in-all-windows-h)
  :config
  (add-hook 'python-mode-local-vars-hook #'pyvenv-track-virtualenv)
  (add-to-list 'global-mode-string
              '(pyvenv-virtual-env-name (" venv:" pyvenv-virtual-env-name " "))
              'append))
(setq lsp-pyright-venv-path "/home/yulivee/venv")

(setq-default python-indent-offset 4)
    (add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
        (add-hook 'python-mode-hook
              (lambda ()
                ;; (setq indent-tabs-mode t)
                (setq tab-width 1)
                (setq python-indent-offset 4)
            (setq evil-shift-width 4)
))

(use-package go-mode)

(use-package julia-mode)

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
(setq org-agenda-window-setup 'only-window)
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
;; -        (use-package org-modern-indent
;; -    	  :after org
;; -          :straight (org-modern-indent :type git :host github :repo "jdtsmith/org-modern-indent")
;; -          :config ; add late to hook
;; -          (add-hook 'org-mode-hook #'org-modern-indent-mode 90))

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
          ;;:straight (org-roam :type git :host github :repo "org-roam/org-roam" :commit "ca873f7")
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
       (org-cite-global-bibliography '("~/Roam/papers/lib.bib"))
       (org-cite-insert-processor 'citar)
        (org-cite-follow-processor 'citar)
        (org-cite-activate-processor 'citar)
      :hook
        (LaTeX-mode . citar-capf-setup)
        (org-mode . citar-capf-setup)
    )
    (setq citar-file-open-functions '(("pdf" . citar-file-open-external)))
    (defvar citar-indicator-files-icons
        (citar-indicator-create
        :symbol (nerd-icons-faicon
                    "nf-fa-file_o"
                    :face 'nerd-icons-green
                    :v-adjust -0.1)
        :function #'citar-has-files
        :padding "  " ; need this because the default padding is too low for these icons
        :tag "has:files"))
        (defvar citar-indicator-links-icons
        (citar-indicator-create
        :symbol (nerd-icons-faicon
                    "nf-fa-link"
                    :face 'nerd-icons-orange
                    :v-adjust 0.01)
        :function #'citar-has-links
        :padding "  "
        :tag "has:links"))
        (defvar citar-indicator-notes-icons
        (citar-indicator-create
        :symbol (nerd-icons-codicon
                    "nf-cod-note"
                    :face 'nerd-icons-blue
                    :v-adjust -0.3)
        :function #'citar-has-notes
        :padding "    "
        :tag "has:notes"))
(setq citar-indicators
(list citar-indicator-files-icons
        citar-indicator-links-icons
        citar-indicator-notes-icons
        )) 


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
    :after (org-roam citar bibtex-completion org-ref citar-org-roam)
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

(use-package magit-todos
:after magit
:config (magit-todos-mode 1))

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

;;     (use-package enlight
;;      :init
;;      (unless (package-installed-p 'enlight)
;;        (package-vc-install
;;         '(enlight
;;           :vc-backend Git
;;           :url "https://github.com/ichernyshovvv/enlight"
;;           :branch "master"))))   

;;        (use-package grid
;;          :init
;;          (unless (package-installed-p 'grid)
;;            (package-vc-install
;;             '(grid
;;               :vc-backend Git
;;               :url "https://github.com/ichernyshovvv/grid.el"
;;               :branch "master"))))

;;        (defvar enlight-lipsum "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.

;;        Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.")

;;        (defface enlight-yellow-bold
;;          '((t (:foreground "#cabf00" :bold t)))
;;          "Yellow bold face")

;;        (defvar enlight-guix
;;          (propertize
;;             "
;; 8b,dPPYba,  ,adPPYYba, ,adPPYba, ,adPPYYba,  
;; 88P'   `\8a ''     `Y8 I8[    '' ''     `Y8  
;; 88       88 ,adPPPPP88  `*Y8ba,  ,adPPPPP88  
;; 88       88 88,    ,88 aa    ]8I 88,    ,88  
;; 88       88 `*8bbdP'Y8 `*YbbdP*' `*8bbdP'Y8  
;;             "
;;           'face 'enlight-yellow-bold))

;;        (defvar enlight-guix-widget
;;          `( :content ,(concat "\n" (propertize "Block 1" 'face 'enlight-yellow-bold)
;;                       "\nGUIX MANAGEMENT WIDGET\n\n")
;;             :width 22 :border t :align center :padding 2))

;;        (defvar enlight-email-width
;;          `( :content
;;             ,(concat "\n" (propertize (format "%s" (nerd-icons-octicon "nf-oct-clock")) 'face 'enlight-yellow-bold)
;;                  "\n"(current-time-string)"\n\n")
;;             :padding 2 :width 22 :align center :border t))

;;        (defvar enlight-weather-width
;;          `( :content
;;             ,(concat "\n" (propertize "Block 3" 'face 'enlight-yellow-bold)
;;                  "\nWEATHER WIDGET\n\n")
;;             :padding 2 :width 22 :border t :align center))

;;        (defvar enlight-calendar
;;          (progn
;;            (calendar)
;;            (diary-mark-entries)
;;            (prog1 (with-current-buffer (buffer-name (current-buffer))
;;                 (buffer-string))
;;              (calendar-exit))))

;;        (use-package enlight
;;          :custom
;;          (enlight-content
;;           (concat
;;            (grid-get-box `( :align center :content ,enlight-guix :width 80))
;;            (grid-get-row
;;             (list
;;              (grid-get-box
;;               (concat
;;            (grid-get-box
;;             `( :content
;;                ,(concat
;;                  (grid-get-box `( :content ,(propertize "another day another try . . ." 'face 'enlight-yellow-bold)
;;                           :width 80 :align center))
;;                  (grid-get-row
;;                   `(,enlight-guix-widget
;;                 "     "
;;                 ,enlight-email-width
;;                 "     "
;;                 ,enlight-weather-width)))
;;                :width 80))
;;            enlight-calendar "\n"
;;            (grid-get-row
;;             `(,(concat
;;                 (propertize "MENU" 'face 'highlight)
;;                 "\n"
;;                 (enlight-menu
;;                  '(("Org Mode"
;;                 ("Org-Agenda " (org-agenda nil "D") "D"))
;;                ("Downloads"
;;                 ;; ("Transmission" transmission "t")
;;                 ("Downloads folder" (dired "~/Downloads") "a"))
;;                ("Other"
;;                 ("Bookmarks" consult-bookmark "b")))))
;;               ,(grid-get-column
;;             `(,(concat
;;     			"         "
;;                 (propertize "Files" 'face 'highlight)
;;                 "\n"
;;     			"         "
;;                 (enlight-menu
;;                  '(("Roam"
;;                 ("          MA Hub" (find-file "~/Roam/master/20231129173749-ma_hub.org") "M")
;;                 ("          Virga Yasf" (find-file "~/Roam/uni/20240311092511-integrate_yasf_into_virga.org") "Y"))
;;                ("          Code"
;;                 ("          master" (dired "~/Code/master/") "M"))
;;                 )))
;;                 )))))))))))


;; (setopt initial-buffer-choice #'enlight)

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

(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'none)

;;; ui/workspaces/autoload/workspaces.el -*- lexical-binding: t; -*-

(defvar +workspace--last nil)
(defvar +workspace--index 0)

;;;###autoload
(defface +workspace-tab-selected-face '((t (:inherit highlight)))
  "The face for selected tabs displayed by `+workspace/display'"
  :group 'persp-mode)

;;;###autoload
(defface +workspace-tab-face '((t (:inherit default)))
  "The face for selected tabs displayed by `+workspace/display'"
  :group 'persp-mode)


;;
;;; Library

(defun +workspace--protected-p (name)
  (equal name persp-nil-name))

(defun +workspace--generate-id ()
  (or (cl-loop for name in (+workspace-list-names)
               when (string-match-p "^#[0-9]+$" name)
               maximize (string-to-number (substring name 1)) into max
               finally return (if max (1+ max)))
      1))


;;; Predicates
;;;###autoload
(defalias #'+workspace-p #'perspective-p
  "Return t if OBJ is a perspective hash table.")

;;;###autoload
(defun +workspace-exists-p (name)
  "Returns t if NAME is the name of an existing workspace."
  (member name (+workspace-list-names)))

;;;###autoload
(defalias #'+workspace-contains-buffer-p #'persp-contain-buffer-p
  "Return non-nil if BUFFER is in WORKSPACE (defaults to current workspace).")


;;; Getters
;;;###autoload
(defalias #'+workspace-current #'get-current-persp
  "Return the currently active workspace.")

;;;###autoload
(defun +workspace-get (name &optional noerror)
  "Return a workspace named NAME. Unless NOERROR is non-nil, this throws an
error if NAME doesn't exist."
  (cl-check-type name string)
  (when-let (persp (persp-get-by-name name))
    (cond ((+workspace-p persp) persp)
          ((not noerror)
           (error "No workspace called '%s' was found" name)))))

;;;###autoload
(defun +workspace-current-name ()
  "Get the name of the current workspace."
  (safe-persp-name (+workspace-current)))

;;;###autoload
(defun +workspace-list ()
  "Return a list of workspace structs (satisifes `+workspace-p')."
  ;; We don't use `hash-table-values' because it doesn't ensure order in older
  ;; versions of Emacs
  (cl-loop for name in persp-names-cache
           if (gethash name *persp-hash*)
           collect it))

;;;###autoload
(defun +workspace-list-names ()
  "Return the list of names of open workspaces."
  persp-names-cache)

;;;###autoload
(defun +workspace-buffer-list (&optional persp)
  "Return a list of buffers in PERSP.

PERSP can be a string (name of a workspace) or a workspace (satisfies
`+workspace-p'). If nil or omitted, it defaults to the current workspace."
  (let ((persp (or persp (+workspace-current))))
    (unless (+workspace-p persp)
      (user-error "Not in a valid workspace (%s)" persp))
    (persp-buffers persp)))

;;;###autoload
(defun +workspace-orphaned-buffer-list ()
  "Return a list of buffers that aren't associated with any perspective."
  (cl-remove-if #'persp--buffer-in-persps (buffer-list)))


;;; Actions
;;;###autoload
(defun +workspace-load (name)
  "Loads a single workspace (named NAME) into the current session. Can only
retrieve perspectives that were explicitly saved with `+workspace-save'.

Returns t if successful, nil otherwise."
  (when (+workspace-exists-p name)
    (user-error "A workspace named '%s' already exists." name))
  (persp-load-from-file-by-names
   (expand-file-name +workspaces-data-file persp-save-dir)
   *persp-hash* (list name))
  (+workspace-exists-p name))

;;;###autoload
(defun +workspace-save (name)
  "Saves a single workspace (NAME) from the current session. Can be loaded again
with `+workspace-load'. NAME can be the string name of a workspace or its
perspective hash table.

Returns t on success, nil otherwise."
  (unless (+workspace-exists-p name)
    (error "'%s' is an invalid workspace" name))
  (let ((fname (expand-file-name +workspaces-data-file persp-save-dir)))
    (persp-save-to-file-by-names fname *persp-hash* (list name))
    (and (member name (persp-list-persp-names-in-file fname))
         t)))

;;;###autoload
(defun +workspace-new (name)
  "Create a new workspace named NAME. If one already exists, return nil.
Otherwise return t on success, nil otherwise."
  (when (+workspace--protected-p name)
    (error "Can't create a new '%s' workspace" name))
  (when (+workspace-exists-p name)
    (error "A workspace named '%s' already exists" name))
  (let ((persp (persp-add-new name))
        (+popup--inhibit-transient t))
    (save-window-excursion
      (let ((ignore-window-parameters t)
            (+popup--inhibit-transient t))
        (persp-delete-other-windows))
      (switch-to-buffer (doom-fallback-buffer))
      (setf (persp-window-conf persp)
            (funcall persp-window-state-get-function (selected-frame))))
    persp))

;;;###autoload
(defun +workspace-rename (name new-name)
  "Rename the current workspace named NAME to NEW-NAME. Returns old name on
success, nil otherwise."
  (when (+workspace--protected-p name)
    (error "Can't rename '%s' workspace" name))
  (persp-rename new-name (+workspace-get name)))

;;;###autoload
(defun +workspace-delete (workspace &optional inhibit-kill-p)
  "Delete the workspace denoted by WORKSPACE, which can be the name of a perspective
or its hash table. If INHIBIT-KILL-P is non-nil, don't kill this workspace's
buffers."
  (unless (stringp workspace)
    (setq workspace (persp-name workspace)))
  (when (+workspace--protected-p workspace)
    (error "Can't delete '%s' workspace" workspace))
  (+workspace-get workspace) ; error checking
  (persp-kill workspace inhibit-kill-p)
  (not (+workspace-exists-p workspace)))

;;;###autoload
(defun +workspace-switch (name &optional auto-create-p)
  "Switch to another workspace named NAME (a string).

If AUTO-CREATE-P is non-nil, create the workspace if it doesn't exist, otherwise
throws an error."
  (unless (+workspace-exists-p name)
    (if auto-create-p
        (+workspace-new name)
      (error "%s is not an available workspace" name)))
  (let ((old-name (+workspace-current-name)))
    (unless (equal old-name name)
      (setq +workspace--last
            (or (and (not (string= old-name persp-nil-name))
                     old-name)
                +workspaces-main))
      (persp-frame-switch name))
    (equal (+workspace-current-name) name)))


;;
;;; Commands

;;;###autoload
(defalias '+workspace/restore-last-session #'doom/quickload-session)

;;;###autoload
(defun +workspace/load (name)
  "Load a workspace and switch to it. If called with C-u, try to reload the
current workspace (by name) from session files."
  (interactive
   (list
    (if current-prefix-arg
        (+workspace-current-name)
      (completing-read
       "Workspace to load: "
       (persp-list-persp-names-in-file
        (expand-file-name +workspaces-data-file persp-save-dir))))))
  (if (not (+workspace-load name))
      (+workspace-error (format "Couldn't load workspace %s" name))
    (+workspace/switch-to name)
    (+workspace/display)))

;;;###autoload
(defun +workspace/save (name)
  "Save the current workspace. If called with C-u, autosave the current
workspace."
  (interactive
   (list
    (if current-prefix-arg
        (+workspace-current-name)
      (completing-read "Workspace to save: " (+workspace-list-names)))))
  (if (+workspace-save name)
      (+workspace-message (format "'%s' workspace saved" name) 'success)
    (+workspace-error (format "Couldn't save workspace %s" name))))

;;;###autoload
(defun +workspace/rename (new-name)
  "Rename the current workspace."
  (interactive (list (completing-read "New workspace name: " (list (+workspace-current-name)))))
  (condition-case-unless-debug ex
      (let* ((current-name (+workspace-current-name))
             (old-name (+workspace-rename current-name new-name)))
        (unless old-name
          (error "Failed to rename %s" current-name))
        (+workspace-message (format "Renamed '%s'->'%s'" old-name new-name) 'success))
    ('error (+workspace-error ex t))))

;;;###autoload
(defun +workspace/delete (name)
  "Delete this workspace. If called with C-u, prompts you for the name of the
workspace to delete."
  (interactive
   (let ((current-name (+workspace-current-name)))
     (list
      (if current-prefix-arg
          (completing-read (format "Delete workspace (default: %s): " current-name)
                           (+workspace-list-names)
                           nil nil nil nil current-name)
        current-name))))
  (condition-case-unless-debug ex
      ;; REVIEW refactor me
      (let ((workspaces (+workspace-list-names)))
        (if (not (member name workspaces))
            (+workspace-message (format "'%s' workspace doesn't exist" name) 'warn)
          (cond ((delq (selected-frame) (persp-frames-with-persp (get-frame-persp)))
                 (user-error "Can't close workspace, it's visible in another frame"))
                ((not (equal (+workspace-current-name) name))
                 (+workspace-delete name))
                ((cdr workspaces)
                 (+workspace-delete name)
                 (+workspace-switch
                  (if (+workspace-exists-p +workspace--last)
                      +workspace--last
                    (car (+workspace-list-names))))
                 (unless (doom-buffer-frame-predicate (window-buffer))
                   (switch-to-buffer (doom-fallback-buffer))))
                (t
                 (+workspace-switch +workspaces-main t)
                 (unless (string= (car workspaces) +workspaces-main)
                   (+workspace-delete name))
                 (doom/kill-all-buffers (doom-buffer-list))))
          (+workspace-message (format "Deleted '%s' workspace" name) 'success)))
    ('error (+workspace-error ex t))))

;;;###autoload
(defun +workspace/kill-session (&optional interactive)
  "Delete the current session, all workspaces, windows and their buffers."
  (interactive (list t))
  (let ((windows (length (window-list)))
        (persps (length (+workspace-list-names)))
        (buffers 0))
    (let ((persp-autokill-buffer-on-remove t))
      (unless (cl-every #'+workspace-delete (+workspace-list-names))
        (+workspace-error "Could not clear session")))
    (+workspace-switch +workspaces-main t)
    (setq buffers (doom/kill-all-buffers (buffer-list)))
    (when interactive
      (message "Killed %d workspace(s), %d window(s) & %d buffer(s)"
               persps windows buffers))))

;;;###autoload
(defun +workspace/kill-session-and-quit ()
  "Kill emacs without saving anything."
  (interactive)
  (let ((persp-auto-save-opt 0))
    (kill-emacs)))

;;;###autoload
(defun +workspace/new (&optional name clone-p)
  "Create a new workspace named NAME. If CLONE-P is non-nil, clone the current
workspace, otherwise the new workspace is blank."
  (interactive (list nil current-prefix-arg))
  (unless name
    (setq name (format "#%s" (+workspace--generate-id))))
  (condition-case e
      (cond ((+workspace-exists-p name)
             (error "%s already exists" name))
            (clone-p (persp-copy name t))
            (t
             (+workspace-switch name t)
             (+workspace/display)))
    ((debug error) (+workspace-error (cadr e) t))))

;;;###autoload
(defun +workspace/new-named (name)
  "Create a new workspace with a given NAME."
  (interactive "sWorkspace Name: ")
  (+workspace/new name))

;;;###autoload
(defun +workspace/switch-to (index)
  "Switch to a workspace at a given INDEX. A negative number will start from the
end of the workspace list."
  (interactive
   (list (or current-prefix-arg
             (completing-read "Switch to workspace: " (+workspace-list-names)))))
  (when (and (stringp index)
             (string-match-p "^[0-9]+$" index))
    (setq index (string-to-number index)))
  (condition-case-unless-debug ex
      (let ((names (+workspace-list-names))
            (old-name (+workspace-current-name)))
        (cond ((numberp index)
               (let ((dest (nth index names)))
                 (unless dest
                   (error "No workspace at #%s" (1+ index)))
                 (+workspace-switch dest)))
              ((stringp index)
               (+workspace-switch index t))
              (t
               (error "Not a valid index: %s" index)))
        (unless (called-interactively-p 'interactive)
          (if (equal (+workspace-current-name) old-name)
              (+workspace-message (format "Already in %s" old-name) 'warn)
            (+workspace/display))))
    ('error (+workspace-error (cadr ex) t))))

;;;###autoload
(dotimes (i 9)
  (defalias (intern (format "+workspace/switch-to-%d" i))
    (lambda () (interactive) (+workspace/switch-to i))
    (format "Switch to workspace #%d" (1+ i))))

;;;###autoload
(defun +workspace/switch-to-final ()
  "Switch to the final workspace in open workspaces."
  (interactive)
  (+workspace/switch-to (car (last (+workspace-list-names)))))

;;;###autoload
(defun +workspace/other ()
  "Switch to the last activated workspace."
  (interactive)
  (+workspace/switch-to +workspace--last))

;;;###autoload
(defun +workspace/cycle (n)
  "Cycle n workspaces to the right (default) or left."
  (interactive (list 1))
  (let ((current-name (+workspace-current-name)))
    (if (equal current-name persp-nil-name)
        (+workspace-switch +workspaces-main t)
      (condition-case-unless-debug ex
          (let* ((persps (+workspace-list-names))
                 (perspc (length persps))
                 (index (cl-position current-name persps)))
            (when (= perspc 1)
              (user-error "No other workspaces"))
            (+workspace/switch-to (% (+ index n perspc) perspc))
            (unless (called-interactively-p 'interactive)
              (+workspace/display)))
        ('user-error (+workspace-error (cadr ex) t))
        ('error (+workspace-error ex t))))))

;;;###autoload
(defun +workspace/switch-left ()  (interactive) (+workspace/cycle -1))

;;;###autoload
(defun +workspace/switch-right () (interactive) (+workspace/cycle +1))

;;;###autoload
(defun +workspace/close-window-or-workspace ()
  "Close the selected window. If it's the last window in the workspace, either
close the workspace (as well as its associated frame, if one exists) and move to
the next."
  (interactive)
  (let ((delete-window-fn (if (featurep 'evil) #'evil-window-delete #'delete-window)))
    (if (window-dedicated-p)
        (funcall delete-window-fn)
      (let ((current-persp-name (+workspace-current-name)))
        (cond ((or (+workspace--protected-p current-persp-name)
                   (cdr (doom-visible-windows)))
               (funcall delete-window-fn))

              ((cdr (+workspace-list-names))
               (let ((frame-persp (frame-parameter nil 'workspace)))
                 (if (string= frame-persp (+workspace-current-name))
                     (delete-frame)
                   (+workspace/delete current-persp-name))))

              ((+workspace-error "Can't delete last workspace" t)))))))

;;;###autoload
(defun +workspace/swap-left (&optional count)
  "Swap the current workspace with the COUNTth workspace on its left."
  (interactive "p")
  (let* ((current-name (+workspace-current-name))
         (count (or count 1))
         (index (- (cl-position current-name persp-names-cache :test #'equal)
                   count))
         (names (remove current-name persp-names-cache)))
    (unless names
      (user-error "Only one workspace"))
    (let ((index (min (max 0 index) (length names))))
      (setq persp-names-cache
            (append (cl-subseq names 0 index)
                    (list current-name)
                    (cl-subseq names index))))
    (when (called-interactively-p 'any)
      (+workspace/display))))

;;;###autoload
(defun +workspace/swap-right (&optional count)
  "Swap the current workspace with the COUNTth workspace on its right."
  (interactive "p")
  (funcall-interactively #'+workspace/swap-left (- count)))


;;
;;; Tabs display in minibuffer

(defun +workspace--tabline (&optional names)
  (let ((names (or names (+workspace-list-names)))
        (current-name (+workspace-current-name)))
    (mapconcat
     #'identity
     (cl-loop for name in names
              for i to (length names)
              collect
              (propertize (format " [%d] %s " (1+ i) name)
                          'face (if (equal current-name name)
                                    '+workspace-tab-selected-face
                                  '+workspace-tab-face)))
     " ")))

(defun +workspace--message-body (message &optional type)
  (concat (+workspace--tabline)
          (propertize " | " 'face 'font-lock-comment-face)
          (propertize (format "%s" message)
                      'face (pcase type
                              ('error 'error)
                              ('warn 'warning)
                              ('success 'success)
                              ('info 'font-lock-comment-face)))))

;;;###autoload
(defun +workspace-message (message &optional type)
  "Show an 'elegant' message in the echo area next to a listing of workspaces."
  (message "%s" (+workspace--message-body message type)))

;;;###autoload
(defun +workspace-error (message &optional noerror)
  "Show an 'elegant' error in the echo area next to a listing of workspaces."
  (funcall (if noerror #'message #'error)
           "%s" (+workspace--message-body message 'error)))

;;;###autoload
(defun +workspace/display ()
  "Display a list of workspaces (like tabs) in the echo area."
  (interactive)
  (let (message-log-max)
    (message "%s" (+workspace--tabline))))


;;
;;; Hooks

;;;###autoload
(defun +workspaces-delete-associated-workspace-h (&optional frame)
  "Delete workspace associated with current frame.
A workspace gets associated with a frame when a new frame is interactively
created."
  (when (and persp-mode (not (bound-and-true-p with-editor-mode)))
    (unless frame
      (setq frame (selected-frame)))
    (let ((frame-persp (frame-parameter frame 'workspace)))
      (when (string= frame-persp (+workspace-current-name))
        (+workspace/delete frame-persp)))))

;;;###autoload
(defun +workspaces-associate-frame-fn (frame &optional _new-frame-p)
  "Create a blank, new perspective and associate it with FRAME."
  (when persp-mode
    (if (not (persp-frame-list-without-daemon))
        (+workspace-switch +workspaces-main t)
      (with-selected-frame frame
        (+workspace-switch (format "#%s" (+workspace--generate-id)) t)
        (unless (doom-real-buffer-p (current-buffer))
          (switch-to-buffer (doom-fallback-buffer)))
        (set-frame-parameter frame 'workspace (+workspace-current-name))
        ;; ensure every buffer has a buffer-predicate
        (persp-set-frame-buffer-predicate frame))
      (run-at-time 0.1 nil #'+workspace/display))))

(defvar +workspaces--project-dir nil)
;;;###autoload
(defun +workspaces-set-project-action-fn ()
  "A `projectile-switch-project-action' that sets the project directory for
`+workspaces-switch-to-project-h'."
  (setq +workspaces--project-dir default-directory))

;;;###autoload
(defun +workspaces-switch-to-project-h (&optional dir)
  "Creates a workspace dedicated to a new project. If one already exists, switch
to it. If in the main workspace and it's empty, recycle that workspace, without
renaming it.

Afterwords, runs `+workspaces-switch-project-function'. By default, this prompts
the user to open a file in the new project.

This be hooked to `projectile-after-switch-project-hook'."
  (when dir
    (setq +workspaces--project-dir dir))
  ;; HACK Clear projectile-project-root, otherwise cached roots may interfere
  ;;      with project switch (see #3166)
  (let (projectile-project-root)
    (when (and persp-mode +workspaces--project-dir)
      (when projectile-before-switch-project-hook
        (with-temp-buffer
          ;; Load the project dir-local variables into the switch buffer, so the
          ;; action can make use of them
          (setq default-directory +workspaces--project-dir)
          (hack-dir-local-variables-non-file-buffer)
          (run-hooks 'projectile-before-switch-project-hook)))
      (unwind-protect
          (if (and (not (null +workspaces-on-switch-project-behavior))
                   (or (eq +workspaces-on-switch-project-behavior t)
                       (equal (safe-persp-name (get-current-persp)) persp-nil-name)
                       (+workspace-buffer-list)))
              (let* ((persp
                      (let ((project-name (doom-project-name +workspaces--project-dir)))
                        (or (+workspace-get project-name t)
                            (+workspace-new project-name))))
                     (new-name (persp-name persp)))
                (+workspace-switch new-name)
                (with-current-buffer (doom-fallback-buffer)
                  (setq default-directory +workspaces--project-dir)
                  (hack-dir-local-variables-non-file-buffer))
                (unless current-prefix-arg
                  (funcall +workspaces-switch-project-function +workspaces--project-dir))
                (+workspace-message
                 (format "Switched to '%s' in new workspace" new-name)
                 'success))
            (with-current-buffer (doom-fallback-buffer)
              (setq default-directory +workspaces--project-dir)
              (hack-dir-local-variables-non-file-buffer)
              (message "Switched to '%s'" (doom-project-name +workspaces--project-dir)))
            (with-demoted-errors "Workspace error: %s"
              (+workspace-rename (+workspace-current-name) (doom-project-name +workspaces--project-dir)))
            (unless current-prefix-arg
              (funcall +workspaces-switch-project-function +workspaces--project-dir)))
        (run-hooks 'projectile-after-switch-project-hook)
        (setq +workspaces--project-dir nil)))))

;;;###autoload
(defun +workspaces-save-tab-bar-data-h (_)
  "Save the current workspace's tab bar data."
  (when (get-current-persp)
    (set-persp-parameter
     'tab-bar-tabs (tab-bar-tabs))
    (set-persp-parameter 'tab-bar-closed-tabs tab-bar-closed-tabs)))

;;;###autoload
(defun +workspaces-save-tab-bar-data-to-file-h (&rest _)
  "Save the current workspace's tab bar data to file."
  (when (get-current-persp)
    ;; HACK: Remove fields (for window-configuration) that cannot be serialized.
    (set-persp-parameter 'tab-bar-tabs
                         (frameset-filter-tabs (tab-bar-tabs) nil nil t))))

;;;###autoload
(defun +workspaces-load-tab-bar-data-h (_)
  "Restores the tab bar data of the workspace we have just switched to."
  (tab-bar-tabs-set (persp-parameter 'tab-bar-tabs))
  (setq tab-bar-closed-tabs (persp-parameter 'tab-bar-closed-tabs))
  (tab-bar--update-tab-bar-lines t))

;;;###autoload
(defun +workspaces-load-tab-bar-data-from-file-h (&rest _)
  "Restores the tab bar data from file."
  (when-let ((persp-tab-data (persp-parameter 'tab-bar-tabs)))
    (tab-bar-tabs-set persp-tab-data)
    (tab-bar--update-tab-bar-lines t)))
(setq persp-suppress-no-prefix-key-warning t)
;;
;;; Advice

;;;###autoload
;; (defun +workspaces-autosave-real-buffers-a (fn &rest args)
;;   "Don't autosave if no real buffers are open."
;;   (when (doom-real-buffer-list)
;;     (apply fn args))
;;   t)

;;; ui/workspaces/config.el -*- lexical-binding: t; -*-

;; `persp-mode' gives me workspaces, a workspace-restricted `buffer-list', and
;; file-based session persistence. I used workgroups2 before this, but abandoned
;; it because it was unstable and slow; `persp-mode' is neither (and still
;; maintained).
;;
;; NOTE persp-mode requires `workgroups' for file persistence in Emacs 24.4.

(defvar +workspaces-main "main"
  "The name of the primary and initial workspace, which cannot be deleted.")

;; (defvar +workspaces-switch-project-function #'doom-project-find-file
;;   "The function to run after `projectile-switch-project' or
;; `counsel-projectile-switch-project'. This function must take one argument: the
;; new project directory.")

(defvar +workspaces-on-switch-project-behavior 'non-empty
  "Controls the behavior of workspaces when switching to a new project.

Can be one of the following:

t           Always create a new workspace for the project
'non-empty  Only create a new workspace if the current one already has buffers
            associated with it.
nil         Never create a new workspace on project switch.")

;; FIXME actually use this for wconf bookmark system
(defvar +workspaces-data-file "_workspaces"
  "The basename of the file to store single workspace perspectives. Will be
stored in `persp-save-dir'.")

(defvar +workspace--old-uniquify-style nil)


;;
;; Packages

(use-package persp-mode
  :unless noninteractive
  :commands persp-switch-to-buffer
  ;; :hook (doom-init-ui . persp-mode)
  ;; :init
  ;;   (persp-mode)
  :config
  (setq persp-autokill-buffer-on-remove 'kill-weak
        persp-reset-windows-on-nil-window-conf nil
        persp-nil-hidden t
        persp-auto-save-fname "autosave"
        persp-save-dir (concat "workspaces/")
        persp-set-last-persp-for-new-frames t
        persp-switch-to-added-buffer nil
        persp-kill-foreign-buffer-behaviour 'kill
        persp-remove-buffers-from-nil-persp-behaviour nil
        persp-auto-resume-time -1 ; Don't auto-load on startup
        persp-auto-save-opt (if noninteractive 0 1)) ; auto-save on kill


  ;;;; Create main workspace
  ;; The default perspective persp-mode creates is special and doesn't represent
  ;; a real persp object, so buffers can't really be assigned to it, among other
  ;; quirks, so I replace it with a "main" perspective.
  ;; (add-hook '(persp-mode-hook persp-after-load-state-functions)
  ;;   (defun +workspaces-ensure-no-nil-workspaces-h (&rest _)
  ;;     (when persp-mode
  ;;       (dolist (frame (frame-list))
  ;;         (when (string= (safe-persp-name (get-current-persp frame)) persp-nil-name)
  ;;           ;; Take extra steps to ensure no frame ends up in the nil perspective
  ;;           (persp-frame-switch (or (cadr (hash-table-keys *persp-hash*))
  ;;                                   +workspaces-main)
  ;;                               frame))))))

  (add-hook 'persp-mode-hook
    (defun +workspaces-init-first-workspace-h (&rest _)
      "Ensure a main workspace exists."
      (when persp-mode
        (let (persp-before-switch-functions)
          ;; Try our best to hide the nil perspective.
          (when (equal (car persp-names-cache) persp-nil-name)
            (pop persp-names-cache))
          ;; ...and create a *real* main workspace to fill this role.
          (unless (or (persp-get-by-name +workspaces-main)
                      ;; Start from 2 b/c persp-mode counts the nil workspace
                      (> (hash-table-count *persp-hash*) 2))
            (persp-add-new +workspaces-main))
          ;; HACK Fix #319: the warnings buffer gets swallowed when creating
          ;;      `+workspaces-main', so display it ourselves, if it exists.
          (when-let (warnings (get-buffer "*Warnings*"))
            (unless (get-buffer-window warnings)
              (save-excursion
                (display-buffer-in-side-window
                 warnings '((window-height . shrink-window-if-larger-than-buffer)))))))))
    (defun +workspaces-init-persp-mode-h ()
      (cond (persp-mode
             ;; `uniquify' breaks persp-mode. It renames old buffers, which causes
             ;; errors when switching between perspective (their buffers are
             ;; serialized by name and persp-mode expects them to have the same
             ;; name when restored).
             (when uniquify-buffer-name-style
               (setq +workspace--old-uniquify-style uniquify-buffer-name-style))
             (setq uniquify-buffer-name-style nil)
             ;; Ensure `persp-kill-buffer-query-function' is last
             (remove-hook 'kill-buffer-query-functions #'persp-kill-buffer-query-function)
             (add-hook 'kill-buffer-query-functions #'persp-kill-buffer-query-function t)
             ;; Restrict buffer list to workspace
             ;; (advice-add #'doom-buffer-list :override #'+workspace-buffer-list)
             )
            (t
             (when +workspace--old-uniquify-style
               (setq uniquify-buffer-name-style +workspace--old-uniquify-style))
             ))))

  ;; Per-workspace `winner-mode' history
  (add-to-list 'window-persistent-parameters '(winner-ring . t))

  (add-hook 'persp-before-deactivate-functions
    (defun +workspaces-save-winner-data-h (_)
      (when (and (bound-and-true-p winner-mode)
                 (get-current-persp))
        (set-persp-parameter
         'winner-ring (list winner-currents
                            winner-ring-alist
                            winner-pending-undo-ring)))))

  (add-hook 'persp-activated-functions
    (defun +workspaces-load-winner-data-h (_)
      (when (bound-and-true-p winner-mode)
        (cl-destructuring-bind
            (currents alist pending-undo-ring)
            (or (persp-parameter 'winner-ring) (list nil nil nil))
          (setq winner-undo-frame nil
                winner-currents currents
                winner-ring-alist alist
                winner-pending-undo-ring pending-undo-ring)))))


  ;; (add-hook 'persp-add-buffer-on-after-change-major-mode-filter-functions
  ;;           #'doom-unreal-buffer-p)

  ;; (defadvice! +workspaces--evil-alternate-buffer-a (&optional window)
  ;;   "Make `evil-alternate-buffer' ignore buffers outside the current workspace."
  ;;   :override #'evil-alternate-buffer
  ;;   (let* ((prev-buffers
  ;;           (if persp-mode
  ;;               (cl-remove-if-not #'persp-contain-buffer-p (window-prev-buffers)
  ;;                                 :key #'car)
  ;;             (window-prev-buffers)))
  ;;          (head (car prev-buffers)))
  ;;     (if (eq (car head) (window-buffer window))
  ;;         (cadr prev-buffers)
  ;;       head)))

  ;; HACK Fixes #4196, #1525: selecting deleted buffer error when quitting Emacs
  ;;      or on some buffer listing ops.
  ;; (defadvice! +workspaces-remove-dead-buffers-a (persp)
  ;;   :before #'persp-buffers-to-savelist
  ;;   (when (perspective-p persp)
  ;;     ;; HACK Can't use `persp-buffers' because of a race condition with its gv
  ;;     ;;      getter/setter not being defined in time.
  ;;     (setf (aref persp 2)
  ;;           (cl-delete-if-not #'persp-get-buffer-or-null (persp-buffers persp)))))

  ;; Delete the current workspace if closing the last open window
  ;; (define-key! persp-mode-map
  ;;   [remap delete-window] #'+workspace/close-window-or-workspace
  ;;   [remap evil-window-delete] #'+workspace/close-window-or-workspace)

  ;; per-frame workspaces
  (setq persp-init-frame-behaviour t
        persp-init-new-frame-behaviour-override nil
        persp-interactive-init-frame-behaviour-override #'+workspaces-associate-frame-fn
        persp-emacsclient-init-frame-behaviour-override #'+workspaces-associate-frame-fn)
  (add-hook 'delete-frame-functions #'+workspaces-delete-associated-workspace-h)
  (add-hook 'server-done-hook #'+workspaces-delete-associated-workspace-h)


  ;; Don't bother auto-saving the session if no real buffers are open.
  ;; (advice-add #'persp-asave-on-exit :around #'+workspaces-autosave-real-buffers-a)

  ;; Fix #1973: visual selection surviving workspace changes
  (add-hook 'persp-before-deactivate-functions #'deactivate-mark)

  ;; Fix #1017: stop session persistence from restoring a broken posframe

  ;; Don't try to persist dead/remote buffers. They cause errors.
  (add-hook 'persp-filter-save-buffers-functions
    (defun +workspaces-dead-buffer-p (buf)
      ;; Fix #1525: Ignore dead buffers in PERSP's buffer list
      (not (buffer-live-p buf)))
    (defun +workspaces-remote-buffer-p (buf)
      ;; And don't save TRAMP buffers; they're super slow to restore
      (let ((dir (buffer-local-value 'default-directory buf)))
        (ignore-errors (file-remote-p dir)))))

  ;; Otherwise, buffers opened via bookmarks aren't treated as "real" and are
  ;; excluded from the buffer list.
  (add-hook 'bookmark-after-jump-hook #'+workspaces-add-current-buffer-h)

  ;;; eshell
  (persp-def-buffer-save/load
   :mode 'eshell-mode :tag-symbol 'def-eshell-buffer
   :save-vars '(major-mode default-directory))
  ;; compile
  (persp-def-buffer-save/load
   :mode 'compilation-mode :tag-symbol 'def-compilation-buffer
   :save-vars '(major-mode default-directory compilation-directory
                compilation-environment compilation-arguments))
  ;; magit
  (persp-def-buffer-save/load
   :mode 'magit-status-mode :tag-symbol 'def-magit-status-buffer
   :save-vars '(default-directory)
   :load-function (lambda (savelist &rest _)
                    (cl-destructuring-bind (buffer-name vars &rest _rest) (cdr savelist)
                      (magit-status (alist-get 'default-directory vars)))))
  ;; Restore indirect buffers
  (defvar +workspaces--indirect-buffers-to-restore nil)
  (persp-def-buffer-save/load
   :tag-symbol 'def-indirect-buffer
   :predicate #'buffer-base-buffer
   :save-function (lambda (buf tag vars)
                    (list tag (buffer-name buf) vars
                          (buffer-name (buffer-base-buffer buf))))
   :load-function (lambda (savelist &rest _rest)
                    (cl-destructuring-bind (buf-name _vars base-buf-name &rest _)
                        (cdr savelist)
                      (push (cons buf-name base-buf-name)
                            +workspaces--indirect-buffers-to-restore)
                      nil)))
  (add-hook 'persp-after-load-state-functions
    (defun +workspaces-reload-indirect-buffers-h (&rest _)
      (dolist (ibc +workspaces--indirect-buffers-to-restore)
        (cl-destructuring-bind (buffer-name . base-buffer-name) ibc
          (let ((base-buffer (get-buffer base-buffer-name)))
            (when (buffer-live-p base-buffer)
              (when (get-buffer buffer-name)
                (setq buffer-name (generate-new-buffer-name buffer-name)))
              (make-indirect-buffer base-buffer buffer-name t)))))
      (setq +workspaces--indirect-buffers-to-restore nil)))

;;; tab-bar
  (add-hook 'tab-bar-mode-hook
    (defun +workspaces-set-up-tab-bar-integration-h ()
      (add-hook 'persp-before-deactivate-functions #'+workspaces-save-tab-bar-data-h)
      (add-hook 'persp-activated-functions #'+workspaces-load-tab-bar-data-h)
      ;; Load and save configurations for tab-bar.
      (add-hook 'persp-before-save-state-to-file-functions #'+workspaces-save-tab-bar-data-to-file-h)
      (+workspaces-load-tab-bar-data-from-file-h))))
(add-hook 'after-init-hook (lambda () (persp-mode)))

(use-package treemacs
      :ensure t
      :defer t
      :init
      (with-eval-after-load 'winum
        (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
      :config
      (progn
        (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
              treemacs-deferred-git-apply-delay        0.5
              treemacs-directory-name-transformer      #'identity
              treemacs-display-in-side-window          t
              treemacs-eldoc-display                   'simple
              treemacs-file-event-delay                2000
              treemacs-file-extension-regex            treemacs-last-period-regex-value
              treemacs-file-follow-delay               0.2
              treemacs-file-name-transformer           #'identity
              treemacs-follow-after-init               t
              treemacs-expand-after-init               t
              treemacs-find-workspace-method           'find-for-file-or-pick-first
              treemacs-git-command-pipe                ""
              treemacs-goto-tag-strategy               'refetch-index
              treemacs-header-scroll-indicators        '(nil . "^^^^^^")
              treemacs-hide-dot-git-directory          t
              treemacs-indentation                     2
              treemacs-indentation-string              " "
              treemacs-is-never-other-window           nil
              treemacs-max-git-entries                 5000
              treemacs-missing-project-action          'ask
              treemacs-move-files-by-mouse-dragging    t
              treemacs-move-forward-on-expand          nil
              treemacs-no-png-images                   nil
              treemacs-no-delete-other-windows         t
              treemacs-project-follow-cleanup          nil
              treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
              treemacs-position                        'left
              treemacs-read-string-input               'from-child-frame
              treemacs-recenter-distance               0.1
              treemacs-recenter-after-file-follow      nil
              treemacs-recenter-after-tag-follow       nil
              treemacs-recenter-after-project-jump     'always
              treemacs-recenter-after-project-expand   'on-distance
              treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
              treemacs-project-follow-into-home        nil
              treemacs-show-cursor                     nil
              treemacs-show-hidden-files               t
              treemacs-silent-filewatch                nil
              treemacs-silent-refresh                  nil
              treemacs-sorting                         'alphabetic-asc
              treemacs-select-when-already-in-treemacs 'move-back
              treemacs-space-between-root-nodes        t
              treemacs-tag-follow-cleanup              t
              treemacs-tag-follow-delay                1.5
              treemacs-text-scale                      nil
              treemacs-user-mode-line-format           nil
              treemacs-user-header-line-format         nil
              treemacs-wide-toggle-width               70
              treemacs-width                           35
              treemacs-width-increment                 1
              treemacs-width-is-initially-locked       t
              treemacs-workspace-switch-cleanup        nil)

        ;; The default width and height of the icons is 22 pixels. If you are
        ;; using a Hi-DPI display, uncomment this to double the icon size.
        ;;(treemacs-resize-icons 44)

        (treemacs-follow-mode t)
        (treemacs-filewatch-mode t)
        (treemacs-fringe-indicator-mode 'always)
        (when treemacs-python-executable
          (treemacs-git-commit-diff-mode t))

        (pcase (cons (not (null (executable-find "git")))
                     (not (null treemacs-python-executable)))
          (`(t . t)
           (treemacs-git-mode 'deferred))
          (`(t . _)
           (treemacs-git-mode 'simple)))

        (treemacs-hide-gitignored-files-mode nil))
      :bind
      (:map global-map
            ("M-0"       . treemacs-select-window)
            ("C-x t 1"   . treemacs-delete-other-windows)
            ("C-x t t"   . treemacs)
            ("C-x t d"   . treemacs-select-directory)
            ("C-x t B"   . treemacs-bookmark)
            ("C-x t C-t" . treemacs-find-file)
            ("C-x t M-t" . treemacs-find-tag)))

    (use-package treemacs-evil
      :after (treemacs evil)
      :ensure t)

    ;; (use-package treemacs-projectile
    ;;   :after (treemacs projectile)
    ;;   :ensure t)

    ;; (use-package treemacs-icons-dired
    ;;   :hook (dired-mode . treemacs-icons-dired-enable-once)
    ;;   :ensure t)

    (use-package treemacs-magit
      :after (treemacs magit)
      :ensure t)

    ;; (use-package treemacs-persp 
    ;;   :after (treemacs persp-mode) 
    ;;   :ensure t
    ;;   :config (treemacs-set-scope-type 'Perspectives))

    ;; (use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
    ;;   :after (treemacs)
    ;;   :ensure t
    ;;   :config (treemacs-set-scope-type 'Tabs))

(lsp-treemacs-sync-mode 1)

(use-package visual-regexp-steroids)

(use-package tree-sitter)
    (use-package tree-sitter-langs)
    (add-hook 'prog-mode-hook 'tree-sitter-hl-mode)
    (use-package evil-textobj-tree-sitter)

 ;; Goto start of next function
(define-key evil-normal-state-map (kbd "tf") (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "function.outer")))
(define-key evil-visual-state-map (kbd "tf") (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "function.outer")))
;; Goto start of previous function
(define-key evil-normal-state-map (kbd "tF") (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "function.outer" t)))
(define-key evil-visual-state-map (kbd "tF") (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "function.outer" t)))
;; Goto end of next function
(define-key evil-normal-state-map (kbd "Tf") (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "function.outer" nil t)))
(define-key evil-visual-state-map (kbd "Tf") (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "function.outer" nil t)))
;; Goto end of previous function
(define-key evil-normal-state-map (kbd "TF") (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "function.outer" t t)))   
(define-key evil-visual-state-map (kbd "TF") (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "function.outer" t t)))   
;; next class
(define-key evil-normal-state-map (kbd "tc") (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "class.outer")))
(define-key evil-visual-state-map (kbd "tc") (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "class.outer")))
;; prev class
(define-key evil-normal-state-map (kbd "tC") (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "class.outer" t)))
(define-key evil-visual-state-map (kbd "tC") (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "class.outer" t)))
;; next param
(define-key evil-normal-state-map (kbd "ta") (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "parameter.outer")))
(define-key evil-visual-state-map (kbd "ta") (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "parameter.outer")))
;; prev param
(define-key evil-normal-state-map (kbd "tA") (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "parameter.outer" t)))
(define-key evil-visual-state-map (kbd "tA") (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "parameter.outer" t)))

(use-package direnv
 :config
 (direnv-mode))

(use-package flycheck
:ensure t
:config
(add-hook 'after-init-hook #'global-flycheck-mode))

(defun my/raise-popup ()
  (interactive)
  (let ((buffer (current-buffer)))
    (unless (one-window-p)
      (delete-window))
    (switch-to-buffer buffer )))

(setq load-prefer-newer t)

(use-package org-roam-ui)

(use-package org-timeblock)
(setq org-timeblock-span 1)

(defun my/compile-python ()
      "bruh"
    (setq-local compile-command
     (concat "python " (when buffer-file-name (shell-quote-argument buffer-file-name)))))

(add-hook 'python-mode-hook 'my/compile-python)
