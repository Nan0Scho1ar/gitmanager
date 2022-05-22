;;; gitmanager.el --- Manage multiple git repositories from within emacs -*- lexical-binding: t; -*-
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
;; Package-Requires: ((emacs "28.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(defvar gitmanager-cache-dir "/home/nan0scho1ar/.config/gitmanager/")

(defvar gitmanager-mode-map)
(defvar gitmanager-previous-buffer nil)

(defface gitmanager-faces-state-clean
  '((((class color) (min-colors 8))
     :foreground "green3"))
  "Branch Clean Face."
  :group 'gitmanager-faces)

(defface gitmanager-faces-state-dirty
  '((((class color) (min-colors 8))
     :foreground "red3"))
  "Branch Clean Face."
  :group 'gitmanager-faces)

(defface gitmanager-faces-state-out-of-sync
  '((((class color) (min-colors 8))
     :foreground "yellow3"))
  "Branch Clean Face."
  :group 'gitmanager-faces)

(defface gitmanager-faces-state-conflicts
  '((((class color) (min-colors 8))
     :foreground "red3"))
  "Branch Clean Face."
  :group 'gitmanager-faces)

(define-derived-mode gitmanager-mode
  fundamental-mode "Gitmanager"
  "Major mode for gitmanager."
  (setq-local case-fold-search nil))

(map! :mode gitmanager-mode :n "RET" #'gitmanager-run-magit)
(map! :mode gitmanager-mode :n "q" #'gitmanager-hide)
(map! :mode gitmanager-mode :n "r" #'gitmanager-fetch-and-state)


;; BEGIN EXEC AGGREGATE

(defun gitmanager-exec-create-aggregate-output-buffer (outbuffer-name paths)
  "OUTBUFFER-NAME PATHS."
  (with-current-buffer (get-buffer-create outbuffer-name)
    (erase-buffer)
    (set (make-local-variable 'buffer-lock) nil)
    (set (make-local-variable 'paths) paths)
    (set (make-local-variable 'completed) '())
    (current-buffer)))

(defun gitmanager-exec-async (cmd path out-buffer post-process)
  "CMD PATH OUT-BUFFER POST-PROCESS."
  (with-current-buffer (get-buffer-create (format "* %s %s*" path cmd))
    (erase-buffer)
    (set (make-local-variable 'path) path)
    (set (make-local-variable 'out-buffer) out-buffer)
    (set (make-local-variable 'post-process) post-process)
    (set (make-local-variable 'default-directory) path)
    (set-process-sentinel
     (start-process-shell-command
      (format "%s %s" cmd path) (current-buffer) cmd)
     'gitmanager-exec-sentinel)))

(defun gitmanager-exec-sentinel (p e)
  "TODO P E."
  (let ((buffer (process-buffer p))
        (sent nil))
    (when (not (null buffer))
      (with-current-buffer buffer
        (let ((output (apply post-process (list path e (buffer-string))))
              (path path))
          (when (process-live-p p)
            (kill-process p))
          (with-current-buffer out-buffer
            (while (not sent)
              (unless buffer-lock
                (set 'buffer-lock buffer)
                (when (equal buffer-lock buffer)
                  (insert output)
                  (setq completed (cons path completed))
                  (setq buffer-lock nil)
                  (setq sent t)))))
          (kill-buffer buffer))))))

(defun gitmanager-exec-map-cmd-async (cmd paths buffname post-proc)
  "CMD PATHS BUFNAME POST-PROC.
Asyncronosly run many commands and aggregate all the results into a single buffer.
returns results buffer (needs to be awaited)"
  (let* ((outbuffer (gitmanager-exec-create-aggregate-output-buffer buffname paths)))
    (dolist (path paths)
      (gitmanager-exec-async cmd path outbuffer post-proc))
    outbuffer))

;; END EXEC AGGREGATE



;; MAIN LOOP BUFFER

(defun gitmanager-create-async-eval-buffer (fn args loop)
  (let ((buffer (generate-new-buffer "* GitManager Async Eval *" )))
    (with-current-buffer buffer
      (set (make-local-variable 'async-eval-fn) fn)
      (set (make-local-variable 'async-eval-args) args)
      (set (make-local-variable 'loop) loop)
      (set (make-local-variable 'should-exit) t))
    buffer))

;; BEGIN MAIN LOOP

(defun gitmanager-loop (buffer)
  (let ((process (start-process-shell-command "gitmanager-async-handler" buffer "sleep 1")))
    (set-process-sentinel process 'gitmanager-loop-sentinel)))

(defun gitmanager-loop-sentinel (process event)
  (let ((buffer (process-buffer process)))
    (when (not (null buffer))
      (with-current-buffer buffer
        (when async-eval-fn
          (let ((result (apply async-eval-fn async-eval-args)))
            (if loop
              (if (equal result 'gitmanager-loop-retry)
                  (setq should-exit nil)
                (setq should-exit t
                      async-eval-fn nil
                      async-eval-args nil))
                (setq async-eval-fn nil
                      async-eval-args nil))))
        (if should-exit
            (kill-buffer buffer)
          (when (buffer-live-p buffer)
            (gitmanager-loop buffer)))))))

;; END MAIN LOOP



;; async await
(defun gitmanager-async-apply (fn args)
  (gitmanager-loop (gitmanager-create-async-eval-buffer fn args nil)))

(defun gitmanager-async-exec (fn)
  (gitmanager-async-apply fn '()))


(defun gitmanager-async-wait-for-buffer-then-apply (buffer fn &optional args)
  (unless args (setq args '()))
  (gitmanager-loop (gitmanager-create-async-eval-buffer
                    #'gitmanager-async-wait-for-buffer-test
                    (list buffer fn args)
                    t)))

(defun gitmanager-async-wait-for-buffer-test (buffer fn args)
  (if (null (with-current-buffer buffer (set-difference paths completed)))
      (with-current-buffer buffer
        (apply fn args))
    ;; Tell async parent loop process to retry
    'gitmanager-loop-retry))


;; BEGIN gitmanager funcs

(defun gitmanager-tree-is-clean-p (state)
  (string-search "nothing to commit, working tree clean" state))

(defun gitmanager-branch-up-to-date-p (state)
  (string-search "Your branch is up to date" state))

(defun gitmanager-has-conflicts-p (state)
  (or (string-search "both added" state)
      (string-search "both modified" state)))

(defun gitmanager-repo-check-state (state)
  (cond ((and (gitmanager-tree-is-clean-p state)
              (gitmanager-branch-up-to-date-p state)) 'clean)
        ((gitmanager-tree-is-clean-p state) 'out-of-sync)
        ((gitmanager-has-conflicts-p state) 'merge-conflict)
        (t 'dirty)))

(defun gitmanager-get-repos ()
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
  (let ((buffname "* Gitmanager *")
        (cmd "git status")
        (post-proc #'gitmanager-state-async-post-proc))
    (gitmanager-exec-map-cmd-async cmd paths buffname post-proc)))

(defun gitmanager-state-async-post-proc (path event result)
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
  (let ((buffname "* Gitmanager Fetch Output *")
        (cmd "git fetch")
        (post-proc #'gitmanager-fetch-async-post-proc))
    (gitmanager-exec-map-cmd-async cmd paths buffname post-proc)))

(defun gitmanager-fetch-async-post-proc (path event result)
  (propertize
   (format "%S\n" (list path (s-replace-regexp "\n$" "" event)))
   'face 'italic))


(defun gitmanager-fetch-and-state-async (paths)
  (let ((buffname "* Gitmanager *")
        (cmd "git fetch >/dev/null && git status")
        (post-proc #'gitmanager-state-async-post-proc))
    (gitmanager-exec-map-cmd-async cmd paths buffname post-proc)))


(defun gitmanager-fetch-and-state ()
  (interactive)
  (message "Fetching repos...")
  (gitmanager-async-wait-for-buffer-then-apply
   (gitmanager-fetch-and-state-async (gitmanager-get-repos))
   (lambda ()
     (sort-lines-in-buffer)
     (message "Done!")
     (switch-to-buffer "* Gitmanager *")
     (gitmanager-mode))))

(defun sort-lines-in-buffer ()
  (sort-lines t (point-min) (point-max)))



(defun gitmanager-run-magit (&rest _)
  (interactive)
  (let ((path (cadr (split-string
                     (buffer-substring
                      (line-beginning-position)
                      (line-end-position))
                     "  |  "))))
    (magit-status path)))

(defun gitmanager-hide (&rest _)
  (interactive)
  (switch-to-buffer gitmanager-previous-buffer))



(defun gitmanager ()
  (interactive)
  (with-current-buffer (get-buffer-create "* Gitmanager *")
      (erase-buffer)
      (sit-for 0))
  (gitmanager-async-exec #'gitmanager-fetch-and-state)
  (setq gitmanager-previous-buffer (current-buffer))
  (message "Fetching repos..."))

;; END gitmanager funcs

;; Testing
;; (gitmanager-async-apply
;;  #'gitmanager-state-async (list (gitmanager-get-repos)))



;; (gitmanager-await-buffer-result
;;  (gitmanager-state-async (gitmanager-get-repos)))


;; (gitmanager-fetch-async (gitmanager-get-repos))


;; (message "%s" (with-current-buffer (gitmanager-state-async (gitmanager-get-repos)) completed))


;; TODO BRANCH NAME
;; (defun gitmanager-repo-branch-name (path)
;;   (s-replace-regexp "\n" ""
;;                     (gitmanager-exec "git rev-parse --abbrev-ref HEAD" path)))


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




;; (defun gitmanager-exec (cmd path)
;;   "CMD PATH."
;;   (with-temp-buffer
;;     (cd path)
;;     (shell-command
;;      cmd (current-buffer) gitmanager-error-buffer)
;;     (buffer-string)))

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
