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

(defun trash-directory-recursively! (dir)
  (when DEBUG (message "DEBUG: Deleting directory %s." dir))
  (delete-directory dir t t))

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

(defun rename-ref! (from to &optional copy)
  (let ((case-fold-search nil)
        (from-ref (concat "./.git/" from))
        (from-log (concat "./.git/logs/" from))
        (to-ref (concat "./.git/" to))
        (to-log (concat "./.git/logs/" to))
        (found nil))
    (when (file-exists-p from-ref)
      (setq found t)
      (maybe-make-directory! (file-name-directory to-ref))
      (maybe-make-directory! (file-name-directory to-log))
      (cond ((not copy)
             (rename-file! from-ref to-ref)
             (rename-file! from-log to-log))
            (t
             (copy-file-or-directory! from-ref to-ref)
             (copy-file-or-directory! from-log to-log))))
    (when (file-exists-p "./.git/packed-refs")
      (with-temp-buffer
        (insert-file-contents "./.git/packed-refs")
        (set-buffer-file-coding-system 'utf-8-unix)
        (let ((regex (concat "^\\([0-9a-f]* \\)" (regexp-quote from) "\\(.*\\)$")))
          (while (search-forward-regexp regex nil t)
            (let ((oldstr (match-string 0))
                  (newstr (concat (match-string 1) to (match-string 2))))
              (cond (copy
                     (when DEBUG (message "DEBUG: Copying packed-ref %s to %s." oldstr newstr))
                     (end-of-line)
                     (insert "\n" newstr))
                    (t
                     (when DEBUG (message "DEBUG: Replacing packed-ref %s with %s." oldstr newstr))
                     (replace-match newstr t t)))
              (setq found t))))
        (write-file-silently! "./.git/packed-refs")))
    (unless found
      (error "Internal error: ref %s not found." from))))

;; ----

(defvar command (pop command-line-args-left))
(defvar context-name (pop command-line-args-left))

(cond ((or (null command) (string= command "list"))
       (message "* %s"
                (mapconcat 'identity
                           (cons active-context
                                 (mapcar (lambda (s) (concat "  " s)) inactive-contexts))
                           "\n")))
      ((string= command "create")
       (when (context-exists-p context-name)
         (error "Context %s already exists." context-name))
       (let ((context-prefix (context-prefix context-name))
             (head-ref (with-temp-buffer
                         (insert-file-contents "./.git/HEAD")
                         (when (search-forward-regexp "^ref: refs/\\(heads/.*\\)$" nil t)
                           (match-string 1)))))
         (rename-ref! "HEAD" (concat context-prefix "HEAD") t)
         (when head-ref
           (rename-ref! (concat "refs/" head-ref) (concat context-prefix head-ref) t)))
       (message "Context %s created." context-name))
      ((string= command "delete")
       (when (context-active-p context-name)
         (error "Cannot delete active context %s." context-name))
       (unless (context-exists-p context-name)
         (error "Context %s does not exist." context-name))
       (trash-directory-recursively! (concat "./.git/" (context-prefix context-name)))
       (trash-directory-recursively! (concat "./.git/logs/" (context-prefix context-name)))
       (message "Context %s deleted." context-name))
      ((string= command "switch")
       (when (context-active-p context-name)
         (error "Context %s is already active." context-name))
       (unless (context-exists-p context-name)
         (error "Context %s does not exist." context-name))
       (let ((from-prefix (context-prefix active-context))
             (to-prefix (context-prefix context-name)))
         (rename-ref! "HEAD" (concat from-prefix "HEAD"))
         (rename-ref! "refs/heads" (concat from-prefix "heads"))
         (when (file-exists-p "./.git/refs/stash")
           (rename-ref! "refs/stash" (concat from-prefix "stash")))
         (rename-ref! (concat to-prefix "HEAD") "HEAD")
         (rename-ref! (concat to-prefix "heads") "refs/heads")
         (when (file-exists-p (concat ".git/" to-prefix "stash"))
           (rename-ref! (concat to-prefix "stash") "refs/stash"))
         (trash-directory-recursively! (concat "./.git/" to-prefix))
         (trash-directory-recursively! (concat "./.git/logs/" to-prefix)))
       (setq active-context context-name)
       (message "Context switched to %s." context-name))
      (t
       (error "Command %s not found." command)))

(with-temp-buffer
  (insert active-context)
  (write-file-silently! "./.git/context"))
