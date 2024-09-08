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

;; set a org-roam directory
(setq org-roam-directory "~/roam")

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

;; configuring mu4e for setting up gmail
(after! mu4e
  (setq mu4e-maildir (expand-file-name "~/.mail/gmail")
        mu4e-get-mail-command "mbsync -a"
        mu4e-update-interval 300
        mu4e-view-show-images t
        mu4e-view-show-addresses t
        mu4e-compose-signature-auto-include nil
        mu4e-change-filenames-when-moving t
        mu4e-attachment-dir "~/Downloads"

        ;; Contexts
        mu4e-contexts
        `(,(make-mu4e-context
            :name "Gmail"
            :match-func (lambda (msg)
                          (when msg
                            (mu4e-message-contact-field-matches
                             msg '(:from :to :cc :bcc) "yasinuveritech@gmail.com")))
            :vars '((user-mail-address      . "yasinuveritech@gmail.com")
                    (user-full-name         . "Yasin Shuman")
                    (mu4e-drafts-folder     . "/gmail/[Gmail]/Drafts")
                    (mu4e-sent-folder       . "/gmail/[Gmail]/Sent Mail")
                    (mu4e-trash-folder      . "/gmail/[Gmail]/Trash")
                    (mu4e-refile-folder     . "/gmail/[Gmail]/All Mail")
                    (mu4e-sent-messages-behavior . delete))))))

;; sets up org-roam-ui
(use-package! org-roam-ui
  :after org-roam
  ;; Defer org-roam-ui-mode loading until Org-roam is loaded
  :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

;; sets up shell so that it starts in a window in the bottom 20% of the screen
;; Function to open shell in bottom 20% window
(defun my/open-shell-in-bottom-20 ()
  "Open shell in a window that takes up 20% of the screen, if not already running."
  (interactive)
  (let ((shell-buffer (get-buffer "*shell*")))
    (if shell-buffer
        (pop-to-buffer shell-buffer)
      (let ((new-window (split-window-vertically (floor (* 0.2 (window-height))))))
        (select-window new-window)
        (shell)))))


;; Bind this function to Option + s (or any key combination you prefer)
(global-set-key (kbd "M-s") 'my/open-shell-in-bottom-20)
