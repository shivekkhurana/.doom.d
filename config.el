(setq user-full-name "Shivek Khurana"
      user-mail-address "khuranashivek@gmail.com")

(setq delete-by-moving-to-trash t)

(setq undo-limit 80000000)

(setq evil-want-fine-undo t)

(setq auto-save-default t)

(global-subword-mode 1)

(setq doom-font (font-spec :family "JetBrains Mono" :size 14 :weight 'normal)
      doom-variable-pitch-font (font-spec :family "JetBrains Mono" :size 14)
      doom-unicode-font (font-spec :family "Apple Color Emoji" :size 14))

(setq doom-theme 'doom-dracula)

(setq display-line-numbers-type 'relative)

;; Get all files in splash folder as alternatives
(let ((alternatives (directory-files (expand-file-name "~/.doom.d/splash/") nil "^\\([^.]\\|\\.[^.]\\|\\.\\..\\)")))
  (setq fancy-splash-image
        (concat doom-user-dir "splash/"
                (nth (random (length alternatives)) alternatives))))

(setq org-directory "~/Wip/time-machine/org")

(setq org-log-done 'time ; set a time when a task was complete
      org-log-into-drawer "LOGBOOK" ; save state changes in ~:LOGBOOK:~ drawer
      org-pretty-entities t)

(setq org-agenda-start-with-log-mode t
      org-agenda-log-mode-items '(closed clock state))

(setq org-complete-tags-always-offer-all-agenda-tags t)

(setq treemacs-is-never-other-window nil)

(setq treemacs-show-hidden-files t)

;; (after! PACKAGE
;;  (setq x y))
