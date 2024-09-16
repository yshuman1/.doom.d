;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")
(setq org-roam-directory "~/roam/")  ;; Org-roam directory set to ~/roam
(setq org-agenda-files
      (append (directory-files-recursively "~/roam" "\\.org$")
              (directory-files-recursively "~/org" "\\.org$")))


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
(defun my/org-save-and-commit ()
  "Commit and push changes to the Git repository with a timestamp when saving or exiting an Org file.
Provides feedback if the commit and push were successful."
  (when (and (buffer-file-name)
             (string-equal (file-name-extension (buffer-file-name)) "org"))
    (let* ((current-time (format-time-string "%Y-%m-%d %H:%M:%S"))
           (commit-command (format "git commit -m 'Auto-commit: %s'" current-time))
           (add-result (shell-command (format "git add %s" (shell-quote-argument (buffer-file-name)))))
           (commit-result (shell-command commit-command))
           (push-result (shell-command "git push")))
      (save-buffer)
      (if (and (= add-result 0) (= commit-result 0) (= push-result 0))
          (message "Commit and push successful: %s" current-time)
        (message "Error during commit or push")))))

(add-hook 'org-mode-hook
          (lambda ()
            (add-hook 'after-save-hook #'my/org-save-and-commit nil t)
            (add-hook 'kill-buffer-hook #'my/org-save-and-commit nil t)))

(defun save-and-commit-config ()
  "Automatically commit and push changes to Doom Emacs configuration files with a timestamp."
  (when (and (buffer-file-name)
             (string-match-p (expand-file-name "~/.doom.d/") (buffer-file-name))
             (or (string-equal (file-name-nondirectory (buffer-file-name)) "config.el")
                 (string-equal (file-name-nondirectory (buffer-file-name)) "init.el")
                 (string-equal (file-name-nondirectory (buffer-file-name)) "packages.el")))
    (let ((timestamp (format-time-string "%Y-%m-%d %H:%M:%S")))
      (shell-command "git add ~/.doom.d/config.el ~/.doom.d/init.el ~/.doom.d/packages.el")
      (shell-command (format "git commit -m 'Auto-commit: %s'" timestamp))
      (shell-command "git push origin main"))))  ;; Adjust 'main' if your branch name is different

(add-hook 'after-save-hook 'save-and-commit-config)

;; Add basic Org-roam configuration without custom node setup
(use-package! org-roam
  :after org
  :custom
  (org-roam-directory "~/roam/")  ;; Set Org-roam directory to ~/roam
  :config
  (org-roam-db-autosync-mode))
(setq org-roam-file-extensions '("org"))

;; Org-roam UI setup
(use-package! org-roam-ui
  :after org-roam
  :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

;; sets up shell so that it starts in a window in the bottom 20% of the screen
;; Function to open shell in a bottom 20% window
(defun my/open-shell-in-bottom-20 ()
  "Open shell in a window that takes up 20% of the screen."
  (interactive)
  (let ((shell-buffer (get-buffer "*shell*")))
    (if shell-buffer
        ;; If shell buffer exists, just switch to it
        (pop-to-buffer shell-buffer)
      ;; Otherwise, split window and open shell
      (progn
        (select-window (split-window-below)) ;; Split window below
        (shrink-window (- (window-height) (floor (* 0.2 (frame-height))))) ;; Resize to 20%
        (shell))))) ;; Open shell

;; Bind this function to Option + s
(global-set-key (kbd "M-s") 'my/open-shell-in-bottom-20)

;; Remove Org-roam node timestamp functions and node metadata functions
;; Remove any function or hook that was modifying node timestamps or titles

(setq org-agenda-custom-commands
      '(("c" "All TODOs"
         ((todo "")))))

;; setup for inline images
;; Automatically display images when opening org files
(setq org-startup-with-inline-images t)

;; Refresh images after executing code blocks
(add-hook 'org-babel-after-execute-hook #'org-redisplay-inline-images)

;; Adjust how images are displayed
(setq org-image-actual-width nil)  ; Use actual image width

;;allow images in dired
(use-package! image-dired
  :config
  (setq image-dired-thumb-size 150))

;;allow images in emacs web
(setq shr-inhibit-images nil)  ; Enable images in EWW

(setq org-preview-latex-default-process 'dvisvgm)

(use-package! ob-dot
  :after org)

(setq org-plantuml-jar-path (expand-file-name "/opt/homebrew/opt/plantuml/libexec/plantuml.jar"))

(defun my/org-paste-image ()
  "Paste an image from clipboard into the current Org buffer."
  (interactive)
  (let ((filename (concat (make-temp-name "screenshot_") ".png")))
    (shell-command (concat "pngpaste " filename))
    (insert (concat "[[file:" filename "]]"))
    (org-display-inline-images)))

(global-set-key (kbd "C-c C-x C-i") 'my/org-paste-image)

(after! org-roam
  (setq org-roam-capture-templates
        '(("m" "meeting" plain
           "%?"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: %^{Title}\n#+filetags: %^{filetags}\n#+attendees: %^{attendees}\n#+meeting-date: %^{Meeting Date}\n\n")
           :unnarrowed t))))


;; Function to move cursor to end of buffer after capture
(defun my-org-roam-move-to-end-of-buffer ()
  "Move cursor to the end of the buffer after creating an Org-roam note."
  (goto-char (point-max)))

;; Hook to trigger the function after org-roam capture
(add-hook 'org-capture-after-finalize-hook #'my-org-roam-move-to-end-of-buffer)
(setq org-roam-completion t)

(defun my-org-roam-git-pull ()
  "Perform a git pull in the org-roam directory at startup."
  (when (file-directory-p "~/roam/")
    (let ((default-directory "~/roam/"))
      (shell-command "git pull"))))

(add-hook 'emacs-startup-hook #'my-org-roam-git-pull)

;; launch open kitty terminal - external
(defun open-kitty-terminal ()
  (interactive)
  (shell-command "kitty &"))

(map! :leader
      :desc "Open Kitty Terminal" "o t" #'open-kitty-terminal)
