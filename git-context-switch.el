#! /usr/bin/env emacs --script

(require 'cl)

(defconst DEBUG nil)

(cd (or (locate-dominating-file "." ".git") (error "Not under a git repo.")))

(defvar active-context
  (if (file-exists-p "./.git/context")
      (with-temp-buffer
        (insert-file-contents "./.git/context")
        (buffer-string))
    "default"))

(defvar inactive-contexts
  (when (file-exists-p "./.git/refs/contexts")
    (remove-if
     (lambda (x) (string-match "^\\.*$" x))
     (directory-files "./.git/refs/contexts"))))

(defvar all-contexts (cons active-context inactive-contexts))

;; ---- utilities

(defun context-prefix (name)
  (unless (string= name (convert-standard-filename name))
    (error "Invalid context name %s." name))
  (concat "refs/contexts/" name "/"))

(defun context-exists-p (name)
  (member name all-contexts))

(defun context-active-p (name)
  (string= active-context name))

(defun maybe-make-directory! (dir)
  (unless (file-exists-p dir)
    (when DEBUG (message "DEBUG: Creating directory %s." dir))
    (make-directory dir t)))

(defun trash-file-or-directory! (file)
  (cond ((file-directory-p file)
         (when DEBUG (message "DEBUG: Deleting directory %s." file))
         (delete-directory file t t))
        (t
         (when DEBUG (message "DEBUG: Deleting file %s." file))
         (delete-file file t))))

(defun write-file-silently! (file)
  (write-region (point-min) (point-max) file nil (unless DEBUG 'nomsg)))

(defun rename-file! (from to)
  (when DEBUG (message "DEBUG: Renaming file %s to %s." from to))
  (rename-file from to))

(defun copy-file-or-directory! (from to)
  (cond ((file-directory-p from)
         (when DEBUG (message "DEBUG: Copying directory %s to %s." from to))
         (copy-directory from to))
        (t
         (when DEBUG (message "DEBUG: Copying file %s to %s." from to))
         (copy-file from to))))

(defun rename-ref! (from &optional to copy)
  (let ((case-fold-search nil)
        (from-ref (concat "./.git/" from))
        (from-log (concat "./.git/logs/" from))
        (to-ref (and to (concat "./.git/" to)))
        (to-log (and to (concat "./.git/logs/" to)))
        (found nil))
    (when to
      (maybe-make-directory! (file-name-directory to-ref))
      (maybe-make-directory! (file-name-directory to-log)))
    (when (file-exists-p from-ref)
      (setq found t)
      (cond ((null to)
             (trash-file-or-directory! from-ref))
            ((not copy)
             (rename-file! from-ref to-ref))
            (t
             (copy-file-or-directory! from-ref to-ref))))
    (when (file-exists-p from-log)
      (cond ((null to)
             (trash-file-or-directory! from-log))
            ((not copy)
             (rename-file! from-log to-log))
            (t
             (copy-file-or-directory! from-log to-log))))
    (when (file-exists-p "./.git/packed-refs")
      (with-temp-buffer
        (insert-file-contents "./.git/packed-refs")
        (set-buffer-file-coding-system 'utf-8-unix)
        (let ((regex (concat "^\\([0-9a-f]* \\)" (regexp-quote from) "\\(.*\\)\n")))
          (while (search-forward-regexp regex nil t)
            (let ((oldstr (match-string 0))
                  (newstr (and to (concat (match-string 1) to (match-string 2) "\n"))))
              (cond ((null to)
                     (replace-match ""))
                    ((not copy)
                     (when DEBUG (message "DEBUG: Replacing packed-ref %s with %s." oldstr newstr))
                     (replace-match newstr t t))
                    (t
                     (when DEBUG (message "DEBUG: Copying packed-ref %s to %s." oldstr newstr))
                     (insert newstr)))
              (setq found t))))
        (write-file-silently! "./.git/packed-refs")))
    (unless found
      (error "Internal error: ref %s not found." from))))

(defun parse-symbolic-ref (ref)
  "Returns (REF . DETACHED_P) of symbolic ref REF."
  (with-temp-buffer
    (insert-file-contents (concat "./.git/" ref))
    (if (search-forward-regexp "^ref: refs/\\(.*\\)$" nil t)
        (cons (match-string 1) nil)
      (cons (buffer-substring (point-at-bol) (point-at-eol)) t))))

(defun update-symbolic-ref-context! (ref &optional to)
  "Update symbolic ref REF to refer context TO. When TO is
omitted or nil, REF will refer the active context."
  (when DEBUG (message "Updating symbolic-ref %s." ref))
  (with-temp-buffer
    (let ((file (concat "./.git/" ref)))
      (insert-file-contents file)
      (when (search-forward-regexp "^ref: \\(refs/\\(?:contexts/[^/]+/\\)?\\)" nil t)
        (replace-match (if to (context-prefix to) "refs/") t t nil 1))
      (write-file-silently! file))))

(defun shell-command-to-string-noerror (command)
  "Execute command COMMAND and returns the output as a string, if
the command exits with 0. Otherwise dump the output and raises an
error."
  (when DEBUG (message "Executing `%s`." command))
  (with-temp-buffer
    (unless (zerop (call-process-shell-command command nil t))
      (message (buffer-string))
      (error "Error executing `%s`." command))
    (when DEBUG (message (buffer-string)))
    (buffer-string)))

(defun update-active-context! (new-active-context)
  (setq active-context new-active-context)
  (with-temp-buffer
    (insert new-active-context)
    (write-file-silently! "./.git/context")))

;; ---- command implementations

(defun command/list ()
  (message "* %s"
           (mapconcat 'identity
                      (cons active-context
                            (mapcar (lambda (s) (concat "  " s)) inactive-contexts))
                      "\n")))

(defun command/create (context-name)
  (when (context-exists-p context-name)
    (error "Context %s already exists." context-name))
  (let* ((head-ref (parse-symbolic-ref "HEAD"))
         (detatched-p (cdr head-ref))
         (head-ref (car head-ref))
         (context-prefix (context-prefix context-name))
         (newhead (concat context-prefix "HEAD")))
    (rename-ref! "HEAD" newhead t)
    (update-symbolic-ref-context! newhead context-name)
    (unless detatched-p
      (rename-ref! (concat "refs/" head-ref) (concat context-prefix head-ref) t)))
  (message "Context %s created." context-name))

(defun command/delete (context-name)
  (when (context-active-p context-name)
    (error "Cannot delete active context %s." context-name))
  (unless (context-exists-p context-name)
    (error "Context %s does not exist." context-name))
  (trash-file-or-directory! (concat "./.git/" (context-prefix context-name)))
  (trash-file-or-directory! (concat "./.git/logs/" (context-prefix context-name)))
  (message "Context %s deleted." context-name))

(defun command/show (context-name)
  (let* ((active-p (context-active-p context-name))
         (prefix (if active-p "refs/" (context-prefix context-name)))
         (dir (concat "./.git/" prefix "heads/"))
         (files (directory-files dir)))
    (unless files
      (error "Context %s not found." context-name))
    (dolist (file files)
      (when (and (file-regular-p (concat dir file)) (not (string-match "^\\\\.*$" file)))
        (message "%sheads/%s" prefix file)))))

(defun command/switch (context-name)
  (when (context-active-p context-name)
    (error "Context %s is already active." context-name))
  (unless (context-exists-p context-name)
    (error "Context %s does not exist." context-name))
  (let* ((active-prefix (context-prefix active-context))
         (to-prefix (context-prefix context-name))
         ;; `rev-parse' to find raw commit SHA1 of the symbolic ref
         (command1 (concat "git rev-parse " to-prefix "HEAD"))
         (to-head (car (split-string (shell-command-to-string-noerror command1) "\n")))
         (command2 (concat "git checkout --detach " to-head)))
    (rename-ref! "HEAD" "HEAD_tmp" t)
    (shell-command-to-string-noerror command2)
    (update-symbolic-ref-context! "HEAD_tmp" active-context)
    (rename-ref! "HEAD_tmp" (concat active-prefix "HEAD"))
    (rename-ref! "refs/heads" (concat active-prefix "heads"))
    (when (file-exists-p "./.git/refs/stash")
      (rename-ref! "refs/stash" (concat active-prefix "stash")))
    (rename-ref! "HEAD")
    (rename-ref! (concat to-prefix "HEAD") "HEAD")
    (update-symbolic-ref-context! "HEAD")
    (rename-ref! (concat to-prefix "heads") "refs/heads")
    (when (file-exists-p (concat ".git/" to-prefix "stash"))
      (rename-ref! (concat to-prefix "stash") "refs/stash"))
    (trash-file-or-directory! (concat "./.git/" to-prefix))
    (trash-file-or-directory! (concat "./.git/logs/" to-prefix)))
  (update-active-context! context-name)
  (message "Context switched to %s." context-name))

;; ---- entrypoint

(let ((command (pop command-line-args-left)))
  (cond ((null command)             (command/list))
        ((string= command "list")   (command/list))
        ((string= command "create") (command/create (pop command-line-args-left)))
        ((string= command "delete") (command/delete (pop command-line-args-left)))
        ((string= command "show")   (command/show (pop command-line-args-left)))
        ((string= command "switch") (command/switch (pop command-line-args-left)))
        (t                          (error "Command %s not found." command))))
