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

(defun gitmanager-get-repos-cache ()
  (with-temp-buffer
    (insert-file-contents (concat gitmanager-cache-dir "repos.cache"))
    (split-string (buffer-string) "\n" t)))

(defun gitmanager-get-repos-exclude ()
  (with-temp-buffer
    (insert-file-contents (concat gitmanager-cache-dir "repos.exclude"))
    (split-string (buffer-string) "\n" t)))

(defun gitmanager-get-repos ()
  (mapcar (lambda (x) (s-replace-regexp "\.git$" "" x))
          (set-difference
           (gitmanager-get-repos-cache)
           (gitmanager-get-repos-exclude)
           :test (lambda (a b) (equal a b)))))

(defun gitmanager-exec (cmd path)
  (with-temp-buffer
    (cd path)
    (shell-command
     cmd (current-buffer) gitmanager-error-buffer)
    (buffer-string)))

(defun gitmanager-repo-branch-name (path)
  (s-replace-regexp "\n" ""
                    (gitmanager-exec "git rev-parse --abbrev-ref HEAD" path)))

(defun gitmanager-repo-status (path)
  (gitmanager-exec "git status" path))

(defun gitmanager-repo-fetch (path)
  (gitmanager-exec "git fetch" path))

(defun gitmanager-repo-state (state)
  (gitmanager-repo-check-state state))

(setq test-dir "/home/nan0scho1ar/repos/me/panacea/")

(gitmanager-repo-status test-dir)
(gitmanager-repo-branch-name test-dir)
(gitmanager-repo-state test-dir)

(mapcar (lambda (x) (list x (gitmanager-repo-state x)))
        (gitmanager-get-repos))


(gitmanager-get-repos-cache)
(gitmanager-get-repos-exclude)
(gitmanager-get-repos)




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
  (with-current-buffer (get-buffer-create (format "* %s Status*" path))
    (erase-buffer)
    (set (make-local-variable 'path) path)
    (set (make-local-variable 'out-buffer) out-buffer)
    (set (make-local-variable 'post-process) post-process)
    (set (make-local-variable 'default-directory) path)
    (set-process-sentinel
     (start-process-shell-command
      (format "%s %s" cmd path) (current-buffer) cmd)
     'gitmanager-sentinel)))

(defun gitmanager-sentinel (p e)
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
    (with-current-buffer (gitmanager-map-cmd-async cmd paths buffname post-proc)
      completed)))

(defun gitmanager-state-async-post-proc (path event result)
  (format "%s\n" (list path (gitmanager-repo-state result))))

(defun gitmanager-fetch-async (paths)
  (let ((buffname "* Gitmanager Fetch Output *")
        (cmd "git fetch")
        (post-proc #'gitmanager-fetch-async-post-proc))
    (gitmanager-map-cmd-async cmd paths buffname post-proc)))

(defun gitmanager-fetch-async-post-proc (path event result)
  (format "Fetching: %s ... %s" path event))


;; (gitmanager-state-async (gitmanager-get-repos))
(gitmanager-fetch-async (gitmanager-get-repos))

;; (message "%s" (with-current-buffer (gitmanager-state-async (gitmanager-get-repos)) completed))

(provide 'gitmanager)
;;; gitmanager.el ends here
