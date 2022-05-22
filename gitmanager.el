;;; gitmanager.el --- Manage multiple git repositories with magit -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Nan0Scho1ar
;;
;; Author: Nan0Scho1ar <https://github.com/nan0scho1ar>
;; Maintainer: Nan0Scho1ar <scorch267@gmail.com>
;; Created: April 22, 2022
;; Modified: April 22, 2022
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/nan0scho1ar/gitmanager
;; Package-Requires: ((emacs "27.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;; Emacs version of the gitmanager tool
;; Includes integration with magit.
;;
;;
;;; Code:
(require 'magit)

(defvar gitmanager-mode-map)
(defvar gitmanager-previous-buffer nil)
(defvar gitmanager-cache-dir "/home/nan0scho1ar/.config/gitmanager/")

(defvar-local gitmanager-async-eval-fn nil)
(defvar-local gitmanager-async-eval-args nil)
(defvar-local gitmanager-should-loop nil)
(defvar-local gitmanager-should-exit nil)
(defvar-local gitmanager-buffer-lock nil)
(defvar-local gitmanager-path nil)
(defvar-local gitmanager-paths nil)
(defvar-local gitmanager-paths-completed nil)
(defvar-local gitmanager-out-buffer nil)
(defvar-local gitmanager-post-process nil)

(defface gitmanager-faces-state-clean
  '((((class color) (min-colors 8))
     :foreground "green3"))
  "Branch Clean Face."
  :group 'gitmanager-faces)

(defface gitmanager-faces-state-dirty
  '((((class color) (min-colors 8))
     :foreground "red3"))
  "Branch Dirty Face."
  :group 'gitmanager-faces)

(defface gitmanager-faces-state-out-of-sync
  '((((class color) (min-colors 8))
     :foreground "yellow3"))
  "Branch Out-Of-Sync Face."
  :group 'gitmanager-faces)

(defface gitmanager-faces-state-conflicts
  '((((class color) (min-colors 8))
     :foreground "red3"))
  "Branch Conflicts Face."
  :group 'gitmanager-faces)

(define-derived-mode gitmanager-mode
  fundamental-mode "Gitmanager"
  "Major mode for gitmanager.")


;; BEGIN EXEC AGGREGATE
(defun gitmanager-exec-create-aggregate-output-buffer (outbuffer-name path-list)
  "OUTBUFFER-NAME PATH-LIST."
  (with-current-buffer (get-buffer-create outbuffer-name)
    (erase-buffer)
    (setq gitmanager-buffer-lock     nil
          gitmanager-paths           path-list
          gitmanager-paths-completed '())
    (current-buffer)))


(defun gitmanager-exec-async (cmd filepath output-buffer post-process-fn)
  "Run CMD for each FILEPATH and fill OUTPUT-BUFFER using POST-PROCESS-FN."
  (with-current-buffer (get-buffer-create (format "* %s %s*" filepath cmd))
    (erase-buffer)
    (setq gitmanager-path filepath
          gitmanager-out-buffer output-buffer
          gitmanager-post-process post-process-fn
          default-directory gitmanager-path)
    (set-process-sentinel
     (start-process-shell-command
      (format "%s %s" cmd gitmanager-path) (current-buffer) cmd)
     'gitmanager-exec-sentinel)))

(defun gitmanager-exec-sentinel (process event)
  "Execute when PROCESS triggers EVENT for further processing."
  (let ((buffer (process-buffer process))
        (sent nil))
    (when (not (null buffer))
      (with-current-buffer buffer
        (let ((output (apply gitmanager-post-process (list gitmanager-path event (buffer-string))))
              (path gitmanager-path))
          (when (process-live-p process)
            (kill-process process))
          (with-current-buffer gitmanager-out-buffer
            (while (not sent)
              (unless gitmanager-buffer-lock
                (setq gitmanager-buffer-lock buffer)
                (when (equal gitmanager-buffer-lock buffer)
                  (insert output)
                  (setq gitmanager-paths-completed (cons path gitmanager-paths-completed)
                        gitmanager-buffer-lock nil
                        sent t)))))
          (kill-buffer buffer))))))

(defun gitmanager-exec-map-cmd-async (cmd paths buffname post-proc)
  "Run CMD for each PATHS and fill BUFFNAME using POST-PROC.
Asyncronosly run many commands and aggregate all the
results into a single buffer.
returns results buffer (needs to be awaited)"
  (let* ((outbuffer (gitmanager-exec-create-aggregate-output-buffer buffname paths)))
    (dolist (path paths)
      (gitmanager-exec-async cmd path outbuffer post-proc))
    outbuffer))

;; END EXEC AGGREGATE


;; MAIN LOOP BUFFER

(defun gitmanager-loop-create-buffer (fn args should-loop)
  "FN ARGS SHOULD-LOOP."
  (let ((buffer (generate-new-buffer "* GitManager Async Eval *" )))
    (with-current-buffer buffer
      (setq gitmanager-async-eval-fn fn
            gitmanager-async-eval-args args
            gitmanager-should-loop should-loop
            gitmanager-should-exit t))
    buffer))

;; BEGIN MAIN LOOP

(defun gitmanager-loop (buffer)
  "Loop until gitmanager-async-eval-fn in BUFFER is completed."
  (let ((process (start-process-shell-command "gitmanager-async-handler" buffer "sleep 1")))
    (set-process-sentinel process 'gitmanager-loop-sentinel)))

(defun gitmanager-loop-sentinel (process _)
  "Execute when PROCESS triggers EVENT for further processing."
  (let ((buffer (process-buffer process)))
    (when (not (null buffer))
      (with-current-buffer buffer
        (when gitmanager-async-eval-fn
          (let ((result (apply gitmanager-async-eval-fn gitmanager-async-eval-args)))
            (if gitmanager-should-loop
              (if (equal result 'gitmanager-loop-retry)
                  (setq gitmanager-should-exit nil)
                (setq gitmanager-should-exit t
                      gitmanager-async-eval-fn nil
                      gitmanager-async-eval-args nil))
                (setq gitmanager-async-eval-fn nil
                      gitmanager-async-eval-args nil))))
        (if gitmanager-should-exit
            (kill-buffer buffer)
          (when (buffer-live-p buffer)
            (gitmanager-loop buffer)))))))

;; END MAIN LOOP



;; async await
(defun gitmanager-async-apply (fn &optional args)
  "Asyncronously apply FN ARGS.
This is useful for calling other async functions which must be
awaited without blocking the main thread."
  (unless args (setq args '()))
  (gitmanager-loop (gitmanager-loop-create-buffer fn args nil)))

(defun gitmanager-async-wait-for-buffer-then-apply (buffer fn &optional args)
  "Await BUFFER, then apply FN ARGS."
  (unless args (setq args '()))
  (gitmanager-loop (gitmanager-loop-create-buffer
                    #'gitmanager-async-wait-for-buffer-test
                    (list buffer fn args)
                    t)))

(defun gitmanager-async-wait-for-buffer-test (buffer fn args)
  "Test if BUFFER is finished, then apply FN ARGS.
If it is not finished return retry symbol.
Do not call this directly, instead use
gitmanager-async-wait-for-buffer-then-apply"
  (with-current-buffer buffer
    (if (null (set-difference gitmanager-paths gitmanager-paths-completed))
        (apply fn args)
      ;; Tell async parent loop process to retry
      'gitmanager-loop-retry)))


;; BEGIN gitmanager funcs

(defun gitmanager-tree-is-clean-p (status)
  "Check git STATUS string to see if working tree is clean."
  (string-search "nothing to commit, working tree clean" status))

(defun gitmanager-branch-up-to-date-p (status)
  "Check git STATUS string to see if repo is up to date."
  (string-search "Your branch is up to date" status))

(defun gitmanager-has-conflicts-p (status)
  "Check git STATUS string to see if repo has conflicts."
  (or (string-search "both added" status)
      (string-search "both modified" status)))

(defun gitmanager-repo-check-state (status)
  "Check git STATUS string to determine the state of the repo."
  (cond ((and (gitmanager-tree-is-clean-p status)
              (gitmanager-branch-up-to-date-p status)) 'clean)
        ((gitmanager-tree-is-clean-p status) 'out-of-sync)
        ((gitmanager-has-conflicts-p status) 'merge-conflict)
        (t 'dirty)))

(defun gitmanager-get-repos ()
  "Fetch a list of paths for git repos."
  (mapcar (lambda (x) (s-replace-regexp "\.git$" "" x))
          (set-difference
           (with-temp-buffer
             (insert-file-contents (concat gitmanager-cache-dir "repos.cache"))
             (split-string (buffer-string) "\n" t))
           (with-temp-buffer
             (insert-file-contents (concat gitmanager-cache-dir "repos.exclude"))
             (split-string (buffer-string) "\n" t))
           :test (lambda (a b) (equal a b)))))


;; State

(defun gitmanager-state-async (paths)
  "Asyncronously determine the state of each repo in PATHS."
  (let ((buffname "* Gitmanager *")
        (cmd "git status")
        (post-proc #'gitmanager-state-async-post-proc))
    (gitmanager-exec-map-cmd-async cmd paths buffname post-proc)))

(defun gitmanager-state-async-post-proc (path _ result)
  "Format the PATH and RESULT state for a git repo."
  (let* ((state (gitmanager-repo-check-state result))
         (state-str (cond
                     ((equal state 'clean)
                      (concat
                       (propertize "No Changes Found"
                                   'face 'gitmanager-faces-state-clean)
                       "  |  "))
                     ((equal state 'dirty)
                      (concat
                       (propertize "Changes Detected"
                                   'face 'gitmanager-faces-state-dirty)
                       "  |  "))
                     ((equal state 'out-of-sync)
                      (concat
                       (propertize "Out of Sync"
                                   'face 'gitmanager-faces-state-out-of-sync)
                       "       |  "))
                     ((equal state 'merge-conflict)
                      (concat
                       (propertize "Merge Conflicts"
                                   'face 'gitmanager-faces-state-conflicts)
                       "   |  ")))))
    (concat state-str (propertize (format "%s\n" path)
                                  'face 'italic))))


;; Fetch
(defun gitmanager-fetch-async (paths)
  "Asyncronously fetch each repo in PATHS."
  (let ((buffname "* Gitmanager Fetch Output *")
        (cmd "git fetch")
        (post-proc #'gitmanager-fetch-async-post-proc))
    (gitmanager-exec-map-cmd-async cmd paths buffname post-proc)))

(defun gitmanager-fetch-async-post-proc (path event _)
  "Format the PATH and EVENT for each fetched git repo."
  (propertize
   (format "%S\n" (list path (s-replace-regexp "\n$" "" event)))
   'face 'italic))


(defun gitmanager-fetch-and-state-async (paths)
  "Asyncronously fetch then determine the state of each repo in PATHS."
  (let ((buffname "* Gitmanager *")
        (cmd "git fetch >/dev/null && git status")
        (post-proc #'gitmanager-state-async-post-proc))
    (gitmanager-exec-map-cmd-async cmd paths buffname post-proc)))


(defun gitmanager-fetch-and-state ()
  "Asyncronously fetch then determine the state of each repo in PATHS.
Interactive, will display information and switch to output buffer once complete."
  (interactive)
  (message "Fetching repos...")
  (gitmanager-async-wait-for-buffer-then-apply
   (gitmanager-fetch-and-state-async (gitmanager-get-repos))
   (lambda ()
     (gitmanager-sort-lines-in-buffer)
     (message "Done!")
     (switch-to-buffer "* Gitmanager *")
     (gitmanager-mode))))

(defun gitmanager-sort-lines-in-buffer ()
  "Sort the lines in a buffer in decending order."
  (sort-lines t (point-min) (point-max)))



(defun gitmanager-run-magit (&rest _)
  "Run magit on the path for the currently selected line in Gitmanager."
  (interactive)
  (let ((path (cadr (split-string
                     (buffer-substring
                      (line-beginning-position)
                      (line-end-position))
                     "  |  "))))
    (magit-status path)))

(defun gitmanager-hide (&rest _)
  "Restore the previous buffer from before you opened Gitmanager."
  (interactive)
  (switch-to-buffer gitmanager-previous-buffer))



(defun gitmanager ()
  "Run gitmanager.
Fetches repos, determines their status, then opens the Gitmanager buffer.
Runs in the background until all processes have completed.
Selecting one of the repos in the gitmanager buffer will open it in magit."
  (interactive)
  (with-current-buffer (get-buffer-create "* Gitmanager *")
    (erase-buffer)
    (sit-for 0))
  (gitmanager-async-apply #'gitmanager-fetch-and-state)
  (setq gitmanager-previous-buffer (current-buffer)))
  (message "Fetching repos...")



;; BEGIN Syncronous TODO rewrite async

(defun gitmanager-exec (cmd path)
  "CMD PATH."
  (with-temp-buffer
    (cd path)
    (shell-command
     cmd (current-buffer) gitmanager-error-buffer)
    (buffer-string)))

(defun gitmanager-repo-branch-name (path)
  "Determine the name of the current branch for repo at PATH."
  (s-replace-regexp "\n" ""
                    (gitmanager-exec "git rev-parse --abbrev-ref HEAD" path)))

;; END Synchronous


;; END gitmanager funcs


;; Testing
;; (gitmanager-async-apply
;;  #'gitmanager-state-async (list (gitmanager-get-repos)))



;; (gitmanager-await-buffer-result
;;  (gitmanager-state-async (gitmanager-get-repos)))


;; (gitmanager-fetch-async (gitmanager-get-repos))


;; (message "%s" (with-current-buffer (gitmanager-state-async (gitmanager-get-repos)) completed))


;; TODO BRANCH NAME


;;
;; OTHER
;;
;;
;; (gitmanager-repo-status test-dir)
;; (gitmanager-repo-branch-name test-dir)
;; (gitmanager-repo-state test-dir)

;; (mapcar (lambda (x) (list x (gitmanager-repo-state x)))
;;         (gitmanager-get-repos))


;; (gitmanager-get-repos-cache)
;; (gitmanager-get-repos-exclude)
;; (gitmanager-get-repos)





;; (defun gitmanager-wait-for-async-buffer (buffer)
;;   (let ((remaining t))
;;     (while remaining
;;       (sleep-for 1)
;;       (setq remaining
;;             (with-current-buffer buffer (set-difference paths completed))))
;;     (with-current-buffer buffer (buffer-string))))

;; (defun gitmanager-await-buffer-result (buffer)
;;   (gitmanager-async-apply #'gitmanager-wait-for-async-buffer (list buffer)))







;; begin loopvars (Unused)

;; (setq gitmanager-loop-buffer-name "* GitManager Loop *")

;; (defun gitmanager-create-loop-buffer ()
;;   (let ((buffer (get-buffer-create gitmanager-loop-buffer-name)))
;;     (with-current-buffer buffer
;;       (set (make-local-variable 'async-eval-fn) nil)
;;       (set (make-local-variable 'async-eval-args) nil)
;;       (set (make-local-variable 'loop) nil)
;;       (set (make-local-variable 'should-exit) nil))
;;     buffer))

;; (defun gitmanager-loop-make-local-variable (symbol value)
;;   (with-current-buffer gitmanager-loop-buffer
;;     (set (make-local-variable symbol) value)))

;; (defun gitmanager-loop-apply-fn (fn args)
;;   (gitmanager-loop-make-local-variable 'async-eval-fn fn)
;;   (gitmanager-loop-make-local-variable 'async-eval-args args))

;; (defun gitmanager-loop-stop ()
;;   (gitmanager-loop-make-local-variable 'should-exit t))

;; (defun gitmanager-loop-start ()
;;   (setq gitmanager-loop-buffer (gitmanager-create-loop-buffer))
;;   (gitmanager-loop gitmanager-loop-buffer))

;; end loopvars (Unused)

(provide 'gitmanager)
;;; gitmanager.el ends here
