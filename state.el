;;; state.el --- Quick navigation between workspaces  -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2014 Sylvain Rousseau <thisirs at gmail dot com>

;; Author: Sylvain Rousseau <thisirs at gmail dot com>
;; Keywords: convenience, workspaces
;; Package-Requires: ((emacs "24"))
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

;;; Installation:

;; (require 'state)

;; See documentation on https://github.com/thisirs/state#state

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(defvar state-keymap-prefix (kbd "s-s")
  "The prefix command for state's keymap.")

(defvar state-prefix-map (make-sparse-keymap)
  "Prefix map for state mode.")

(defvar state-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map state-keymap-prefix state-prefix-map)
    map)
  "Keymap for state mode.")

(cl-defstruct state
  "Structure representing a state.
Slots:

`name'
    Symbol identifying the state.
`key'
    Key used to switch to this state.
`switch'
    Form that performs the switch.
`exist'
    Form that tells if the state is existing.
`create'
    Form to create the state.
`in'
    Form that returns true if we are in this state.
`bound'
    If non-nil, this state is accessible only from another state.
`priority'
    Priority of state if there is more than one we want to switch to.
`keep'
    What to do when we keep pressing the key after switching.
`before'
    Action to perform before switching to another state.
`origin'
    Store state symbol name we are coming from.
`current'
    Data used to restore this state; usually a wconf."
  name key switch exist create in bound priority keep before origin current)

(defvar state--states nil
  "List of all defined states.")

(defvar state--default-state
  (make-state :name 'default
              :switch '(let ((state (state--get-state-by-name 'default)))
                         (if (window-configuration-p (state-current state))
                             (set-window-configuration (state-current state))))
              :before '(let ((state (state--get-state-by-name 'default)))
                         (when state
                           (setf (state-current state) (current-window-configuration)))))
  "Default state when not in any other state.")

(defun state--filter (collection slot pred-or-value)
  "Return all states found in COLLECTION with SLOT's value satisfying PRED-OR-VALUE.

If PRED-OR-VALUE is a function, call it with slot's value as
first argument. Otherwise, compare slot's value with `equal'."
  (unless (memq slot (mapcar #'car (get 'state 'cl-struct-slots)))
    (error "Unknown slot name %s" slot))
  (let ((predicate (if (functionp pred-or-value)
                       pred-or-value
                     (lambda (v) (equal pred-or-value v))))
        state result)
    (while (setq state (pop collection))
      (if (funcall predicate (funcall (intern (format "state-%s" slot)) state))
          (push state result)))
    result))

(defun state--get-state-by-name (name)
  "Return a state object with name NAME found in `state--states'.
If not found or if NAME is equal to 'default, return the default
state `state--default-state'."
  (if (eq name 'default)
      state--default-state
    (let ((states state--states) state)
      (while (and (setq state (pop states))
                  (not (eq name (state-name state)))))
      state)))

(defun state--get-state-in ()
  "Return the current state or default state if not in any."
  (let ((states state--states) state)
    (while (and (setq state (pop states))
                (not (state-call state 'in))))
    (or state state--default-state)))

(defun state-call (state slot &rest args)
  "Call or eval the value of slot SLOT in state STATE. Call with
ARGS if supplied."
  (let ((value (funcall (intern (format "state-%s" slot)) state)))
    (if (functionp value)
        (apply value args)
      (eval value))))

(defun state--do-switch (key)
  "Perform the switch process when KEY is pressed."
  (let* ((from (state--get-state-in))
         (from-name (state-name from))
         ;; States we might switch to; special case if current state
         ;; is the state we want to switch to (ie switch back)
         (states (if (equal key (state-key from))
                     (list from)
                   (or (state--filter
                        (state--filter state--states 'key key)
                        'bound
                        (lambda (v)
                          (if (symbolp v)
                              (eq v from-name)
                            (if (functionp v)
                                (funcall v)
                              (eval v)))))
                       (state--filter
                        (state--filter state--states 'key key)
                        'bound
                        (lambda (v) (not v))))))
         (to (if (not states)
                 (error "Non-existent state")
               (if (= 1 (length states))
                   (car states)
                 (state--get-state-by-name
                  (intern
                   (completing-read "Choose state: "
                                    (mapcar (lambda (s) (cons (state-name s) s)) states) nil t))))))
         (to-name (state-name to)))
    ;; Test if we are switching back
    (if (eq to-name from-name)
        (progn
          (state-call from 'before)
          (let ((origin (state-origin from)))
            (if (not origin)
                (user-error "Not coming from anywhere")
              (let ((wconf (state-current (state--get-state-by-name origin))))
                (if (not (window-configuration-p wconf))
                    (user-error "No wconf stored for state %s" origin)
                  (set-window-configuration wconf)
                  (message "Back to state %s" origin))))))
      ;; Not switching back but switching to, so save original state
      (setf (state-origin to) from-name)

      ;; Save current wonf to restore it if we switch back
      (setf (state-current from) (current-window-configuration))

      ;; Executes any other user defined "before" form
      (state-call from 'before)

      (if (state-call to 'exist)
          (progn
            (state-call to 'switch)
            (state-call to 'before))
        (state-call to 'create)
        (unless (state-call to 'in)
          (state-call to 'switch))
        (state-call to 'before))
      (message "Switched to state %s" (state-name to))

      ;; If keep in non-nil install transient keymap
      (if (state-keep to)
          (set-transient-map
           (let ((map (make-sparse-keymap)))
             (define-key map (kbd key)
               (lambda ()
                 (interactive)
                 (state-call to 'keep to)))
             map) t)))))

;;;###autoload
(defmacro state-define-state (name &rest args)
  "Define a new state named NAME with property list ARGS."
  (let ((state (or (state--get-state-by-name name) (make-state)))
        (key (plist-get args :key))
        (switch (plist-get args :switch))
        (before (plist-get args :before))
        (in (plist-get args :in))
        (bound (plist-get args :bound))
        (exist (plist-get args :exist))
        (keep (plist-get args :keep))
        (create (plist-get args :create)))

    (setf (state-name state) name)
    (setf (state-key state) key)
    (setf (state-bound state) bound)
    (setf (state-keep state) keep)

    ;; If the create property is nil, infer one base on switch or in
    ;; properties if they are strings. Otherwise leave nil; switch
    ;; is then called even if the state does not exist. Make sure
    ;; switch is able to create if not existing
    (setf (state-create state)
          (or create
              (if (stringp switch)
                  (if (file-name-absolute-p switch)
                      `(find-file-existing ,switch)
                    `(get-buffer-create "*scratch*"))
                (if (stringp in)
                    (if (file-directory-p in)
                        `(dired ,in)
                      `(find-file-existing ,in))))))

    ;; Rewrite in property if it is a string or if switch is a string
    (setf (state-in state)
          (if (stringp in)
              `(string-prefix-p
                (file-truename ,in)
                (file-truename (or (buffer-file-name) default-directory "/")))
            (if (stringp switch)
                (if (file-name-absolute-p switch)
                    `(eq (current-buffer) (find-buffer-visiting ,switch))
                  `(eq (current-buffer) (get-buffer ,switch)))
              (or in (error "No :in property or not able to infer one")))))

    ;; If the exist property is nil, infer one base on switch or in
    ;; properties when they are strings. Otherwise leave nil; create
    ;; is then called every time.
    (setf (state-exist state)
          (or exist
              (if (stringp in)
                  `(catch 'found
                     (progn
                       (mapc (lambda (buf)
                               (if (string-prefix-p
                                    (file-truename ,in)
                                    (file-truename
                                     (with-current-buffer buf
                                       (or (buffer-file-name) default-directory "/"))))
                                   (throw 'found t)))
                             (buffer-list))
                       nil))
                (if (stringp switch)
                    `(get-buffer ,switch)))))

    ;; Rewrite switch property if it is a string or if in is a string
    (setf (state-switch state)
          (if (stringp in)
              `(let ((state (state--get-state-by-name ',name)))
                 (if (window-configuration-p (state-current state))
                     (set-window-configuration (state-current state))
                   (switch-to-buffer
                    (or
                     (catch 'found
                       (progn
                         (mapc (lambda (buf)
                                 (if (string-prefix-p
                                      (file-truename ,in)
                                      (file-truename
                                       (with-current-buffer buf
                                         (or (buffer-file-name) default-directory "/"))))
                                     (throw 'found buf)))
                               (buffer-list))
                         nil))
                     (error "Unable to switch to state %s" ',name)))))
            (if (stringp switch)
                (if (file-name-absolute-p switch)
                    `(find-file-existing ,switch)
                  `(switch-to-buffer ,switch))
              (or switch
                  `(let ((state (state--get-state-by-name ',name)))
                     (if (window-configuration-p (state-current state))
                         (set-window-configuration (state-current state))))))))

    ;; By default, before switching, store the current window
    ;; configuration in the slot curent.
    (setf (state-before state)
          (or before
              `(let ((state (state--get-state-by-name ',name)))
                 (when state
                   (setf (state-current state) (current-window-configuration))))))

    ;; Add to list of states
    (add-to-list 'state--states state)

    ;; Bind if it is not already
    `(define-key state-prefix-map (kbd ,key) (lambda () (interactive) (state--do-switch ,key)))))

;;;###autoload
(define-minor-mode state-mode
  "Minor mode to switch between workspaces."
  :lighter " St"
  :keymap state-mode-map)

;;;###autoload
(define-globalized-minor-mode state-global-mode
  state-mode
  state-on)

;;;###autoload
(defun state-on ()
  "Enable State minor mode."
  (state-mode 1))

(provide 'state)

;;; state.el ends here
