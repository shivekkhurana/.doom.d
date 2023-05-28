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

(setq +org-capture-todo-file "queue.org")

(setq daily-tasks-list '("workout" "morning meditation" "night meditation" "sleep on time" "cut off screen at 7pm" "dinner before 6pm"))

(defun generate-checkbox-list (list)
  (mapconcat (lambda (item) (format "- [ ] %s" item)) list "\n"))

(setq org-capture-templates
      '(("t" "Enter TODO item in queue 🗳️" entry
         (file+headline +org-capture-todo-file "Inbox")
         "* TODO %U %?\n%i\n%a")
        ("s" "Start my day ☀️" entry
         (file+datetree +org-capture-journal-file)
         "* %U ☀️ [/]\n%(generate-checkbox-list daily-tasks-list)"
         :immediate-finish t)
        ("j" "Journal ☁️" entry
         (file+olp+datetree +org-capture-journal-file)
         "* %U %?\n%i\n%a" :prepend t)))

(setq treemacs-is-never-other-window nil)

(setq treemacs-show-hidden-files t)

;; (after! PACKAGE
;;  (setq x y))
