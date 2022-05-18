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

(defvar gitmanager-status-buffer "* Gitmanager Status *")
(defvar gitmanager-error-buffer "* Gitmanager Error *")
(defvar gitmanager-cache-dir "/home/nan0scho1ar/.config/gitmanager/")

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


(defun gitmanager-create-async-output-buffer (outbuffer-name paths)
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
  "P E."
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


(defun gitmanager-map-cmd-async (cmd paths buffname post-proc)
  "CMD PATHS BUFNAME POST-PROC."
  (let* ((outbuffer (gitmanager-create-async-output-buffer buffname paths)))
    (dolist (path paths)
      (gitmanager-exec-async cmd path outbuffer post-proc))
    outbuffer))

(defun gitmanager-state-async (paths)
  (let ((buffname "* Gitmanager Repo Status Output *")
         (cmd "git status")
         (post-proc #'gitmanager-state-async-post-proc))
    (gitmanager-map-cmd-async cmd paths buffname post-proc)))

(defun gitmanager-state-async-post-proc (path event result)
  (format "%S\n" (list path (gitmanager-repo-check-state result))))

(defun gitmanager-fetch-async (paths)
  (let ((buffname "* Gitmanager Fetch Output *")
        (cmd "git fetch")
        (post-proc #'gitmanager-fetch-async-post-proc))
    (gitmanager-map-cmd-async cmd paths buffname post-proc)))

(defun gitmanager-fetch-async-post-proc (path event result)
  (format "%S\n" (list path (s-replace-regexp "\n$" "" event))))


(defun gitmanager-repo-branch-name (path)
  (s-replace-regexp "\n" ""
                    (gitmanager-exec "git rev-parse --abbrev-ref HEAD" path)))



(defun gitmanager-await (buffer)
  (let ((remaining t))
    (while remaining
      (sleep-for 1)
      (setq remaining
            (with-current-buffer buffer (set-difference paths completed))))
    (with-current-buffer buffer (buffer-string))))



(setq gitmanager-loop-buffer-name "* GitManager Loop *")

(defun gitmanager-create-loop-buffer ()
  (let ((buffer (get-buffer-create gitmanager-loop-buffer-name)))
    (with-current-buffer buffer
      (set (make-local-variable 'async-eval-fn) nil)
      (set (make-local-variable 'async-eval-args) nil)
      (set (make-local-variable 'should-exit) nil))
    buffer))

;; TODO may need buffer lock
(defun gitmanager-loop (buffer)
  (let ((process
         (start-process-shell-command
          "gitmanager-async-handler"
          buffer "sleep 1")))
    (set-process-sentinel process 'gitmanager-loop-sentinel)))


(defun gitmanager-loop-sentinel (process event)
  (let ((buffer (process-buffer process)))
    (when (not (null buffer))
      (with-current-buffer buffer
        (message "Ping!")
        (when async-eval-fn
          (message "Evalauating '%S' with args '%S'"
                   async-eval-fn async-eval-args)
          (message "%S" (apply async-eval-fn async-eval-args))
          (setq async-eval-fn nil
                async-eval-args nil))
        (when (and (buffer-live-p buffer)
                   (not should-exit))
          (gitmanager-loop buffer))))))




(defun gitmanager-loop-make-local-variable (symbol value)
  (with-current-buffer gitmanager-loop-buffer
    (set (make-local-variable symbol) value)))

(defun gitmanager-loop-apply-fn (fn args)
  (gitmanager-loop-make-local-variable 'async-eval-fn fn)
  (gitmanager-loop-make-local-variable 'async-eval-args args))

(defun gitmanager-async-eval-buffer-result (buffer)
  (gitmanager-loop-apply-fn #'gitmanager-await (list buffer)))

(defun gitmanager-loop-stop ()
  (gitmanager-loop-make-local-variable 'should-exit t))

(defun gitmanager-loop-start ()
  (setq gitmanager-loop-buffer (gitmanager-create-loop-buffer))
  (gitmanager-loop gitmanager-loop-buffer))


(gitmanager-loop-start)

(gitmanager-async-eval-buffer-result
 (gitmanager-state-async (gitmanager-get-repos)))

(gitmanager-loop-stop)

;; (gitmanager-fetch-async (gitmanager-get-repos))
;; TODO Detect when complete

;; (message "%s" (with-current-buffer (gitmanager-state-async (gitmanager-get-repos)) completed))

(provide 'gitmanager)
;;; gitmanager.el ends here
