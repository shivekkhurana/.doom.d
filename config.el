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

(setq daily-tasks-list '("workout"
                         "meditation on pride"
                         "meditation on daily events"
                         "sleep on time"
                         "dinner before 6pm"
                         "practice portuguese"
                         ))

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

(setq web-mode-enable-front-matter-block t)

(define-derived-mode astro-mode web-mode "astro")
(setq auto-mode-alist
      (append '((".*\\.astro\\'" . astro-mode))
              auto-mode-alist))

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration
               '(astro-mode . "astro"))

  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("astro-ls" "--stdio"))
                    :activation-fn (lsp-activate-on "astro")
                    :server-id 'astro-ls)))

(setq treemacs-is-never-other-window nil)

(setq treemacs-show-hidden-files t)

(setq lsp-use-plists t)

(defun lsp-booster--advice-json-parse (old-fn &rest args)
  "Try to parse bytecode instead of json."
  (or
   (when (equal (following-char) ?#)
     (let ((bytecode (read (current-buffer))))
       (when (byte-code-function-p bytecode)
         (funcall bytecode))))
   (apply old-fn args)))
(advice-add (if (progn (require 'json)
                       (fboundp 'json-parse-buffer))
                'json-parse-buffer
              'json-read)
            :around
            #'lsp-booster--advice-json-parse)

(defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  "Prepend emacs-lsp-booster command to lsp CMD."
  (let ((orig-result (funcall old-fn cmd test?)))
    (if (and (not test?)                             ;; for check lsp-server-present?
             (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
             lsp-use-plists
             (not (functionp 'json-rpc-connection))  ;; native json-rpc
             (executable-find "emacs-lsp-booster"))
        (progn
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))
(advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)

;; (after! PACKAGE
;;  (setq x y))
