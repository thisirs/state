;;; state.el --- Quick navigation between workspaces

;; Copyright (C) 2013 Sylvain Rousseau <thisirs at gmail dot com>

;; Author: Sylvain Rousseau <thisirs at gmail dot com>
;; Keywords: convenience, workspaces
;; URL: https://github.com/thisirs/state.git

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; This library allows you to switch back and forth between predefined
;; workspaces. See the README file for more information.

;;; Code:

(defvar state-prefix-key "s-s"
  "The key `state-command-prefix' is bound to in the global map.")

(defvar state-original-key "o"
  "Key used to go back to the original state i.e. the state that
is not recognized by any of the states defined in
`state-alist'.")

(defvar state-map
  (let ((map (make-sparse-keymap)))
    (define-key map "h" 'state-goto-here)
    map)
  "State's keymap")

(defvar state-command-prefix)
(define-prefix-command 'state-command-prefix)
(fset 'state-command-prefix state-map)
(setq  state-command-prefix state-map)

(global-set-key (read-kbd-macro state-prefix-key) 'state-command-prefix)

(defvar state-alist
  '((emacs
     (key . "e")
     (switch . "~/.emacs.d/init.el"))
    (gnus
     (key . "g")
     (state-p . (memq major-mode
                      '(message-mode
                        gnus-group-mode
                        gnus-summary-mode
                        gnus-article-mode)))
     (switch . (wconf-fullscreen 'gnus (gnus))))
    (erc
     (key . "i")
     (state-p . (memq (current-buffer)
                      (erc-buffer-list)))
     (switch . (erc-start-or-switch 1)))
    (message
     (key . "m")
     (switch . "*Messages*"))
    (scratch
     (key . "s")
     (switch . "*scratch*"))
    (twit
     (key . "t")
     (state-p . (and (require 'twittering-mode) (twittering-buffer-p)))
     (switch . (wconf-fullscreen 'twit (twit))))
    (org
     (key . "a")
     (state-p . (string-prefix-p "*Org Agenda(" (or (buffer-name) "")))
     (switch . (wconf-fullscreen 'org (org-agenda nil "t")))))
  "Variable that stores all defined states.

It is a list and each element is of the form (ID . SETTINGS)
where SETTINGS is a list of setting. A setting is of the
form (SETTING . ELT) where SETTING is one of the symbols `key',
`state-p' or `switch'.")

(defvar state-from-id ()
  "Alist that stores for each state, the previous state.")

(defmacro wconf-fullscreen (id &rest body)
  `(if (and (consp (get-register ,id))
            (window-configuration-p (car (get-register ,id))))
       (condition-case nil
           (jump-to-register ,id)
         (error
          ,@body
          (delete-other-windows)))
     ,@body
     (delete-other-windows)
     (window-configuration-to-register ,id)))

(defun state-buffer-p (file-name-or-buffer-name)
  "Return non-nil if current buffer is visiting or named
FILE-NAME-OR-BUFFER-NAME."
  (if (file-name-absolute-p file-name-or-buffer-name)
      (and (buffer-file-name)
           (string=
            (file-truename (buffer-file-name))
            (file-truename file-name-or-buffer-name)))
    (equal file-name-or-buffer-name (buffer-name))))

(defun state-current-state ()
  "Return the id corresponding to the current state. If we are
not in a defined state, return `default'."
  (let ((alist state-alist)
        entry found)
    (while (and (not found) (setq entry (pop alist)))
      (let ((switch (cdr (assoc 'switch (cdr entry))))
            (state-p (cdr (assoc 'state-p (cdr entry)))))
        (setq found
              (if state-p
                  (if (functionp state-p)
                      (funcall state-p)
                    (eval state-p))
                (if (stringp switch)
                    (state-buffer-p switch)
                  (error "No state-p setting and switch setting is not a string!"))))))
    (or (and found (car entry)) 'default)))

(defun state-switch-to-state (id)
  "Switch to the state defined by ID."
  (if (eq id 'default)
      (jump-to-register 'default)
    (let* ((settings (cdr (assoc id state-alist)))
           (switch (cdr (assoc 'switch settings))))
      (if (stringp switch)
          (state--switch-for-buffer switch)
        (if (functionp switch)
            (funcall switch id)
          (eval switch))))))

(defun state--switch-for-buffer (file-name-or-buffer-name)
  "Switch to or open FILE-NAME-OR-BUFFER-NAME if it is an
absolute path. If not, FILE-NAME-OR-BUFFER-NAME is assumed to be
a buffer name to which we switch."
  (if (file-name-absolute-p file-name-or-buffer-name)
      (if (find-buffer-visiting file-name-or-buffer-name)
          (switch-to-buffer (find-buffer-visiting file-name-or-buffer-name))
        (find-file-existing file-name-or-buffer-name))
    (switch-to-buffer (get-buffer-create file-name-or-buffer-name))))

(defun state--change-state (id)
  "Return the command that is run when the key corresponding to
`id' is pressed."
  `(lambda ()
     (interactive)
     (let* ((id-from (state-current-state))
            (switch-from
             (cdr (assoc 'switch
                         (cdr (assoc id-from state-alist))))))
       (unless (stringp switch-from)
         (window-configuration-to-register id-from))
       (if (eq id-from ',id)
           (if (assoc ',id state-from-id)
               (state-switch-to-state (cdr (assoc ',id state-from-id)))
             (message "Not coming from anywhere"))
         (push (cons ',id id-from) state-from-id)
         (state-switch-to-state ',id)))))

;;;###autoload
(defun state-install-bindings ()
  "Install key bindings as specified in `state-alist'. Add an
extra key binding corresponding to the default case."
  (setq state-from-id nil)
  (dolist (settings state-alist)
    (let ((id (car settings))
          (key (cdr (assoc 'key (cdr settings)))))
      (if (get-register id)
          (setcdr (get-register id) nil))
      (define-key state-map key (state--change-state id))))
  (define-key state-map "o" (state--change-state 'default)))

(provide 'state)
;;; state.el ends here
