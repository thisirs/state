;;;; (executable-interpret (format "emacs -batch -L %s -l %s" (file-name-directory (locate-library "test-simple.elc")) buffer-file-name))
(require 'test-simple)
(test-simple-start)

(assert-t (load-file "./state.el") "Can't load ./state.el")

(note "state-define-state")

(state-define-state in-directory
  :key "a"
  :in "~/.emacs.d/")
(state-define-state in-file
  :key "b"
  :in "~/.emacs.d/init.el")
(state-define-state switch-file
  :key "c"
  :switch "~/.emacs.d/init.el")
(state-define-state switch-buf
  :key "c"
  :switch "*scratch*")
(state-define-state in-sexp
  :key "d"
  :in (sexp))
(state-define-state in-and-switch
  :key "e"
  :in "in"
  :switch "switch")
(-)
(setq in-directory (state--get-state-by-name 'in-directory))
(setq in-file (state--get-state-by-name 'in-file))
(setq switch-file (state--get-state-by-name 'switch-file))
(setq switch-buf (state--get-state-by-name 'switch-buf))
(setq in-sexp (state--get-state-by-name 'in-sexp))
(setq in-and-switch (state--get-state-by-name 'in-and-switch))

(note "state-define-state:state-key")
(assert-equal "a" (state-key in-directory))

(note "state-define-state:state-create")
(assert-equal '(dired-noselect "~/.emacs.d/")
              (state-create in-directory))
(assert-equal '(find-file-noselect "~/.emacs.d/init.el")
              (state-create in-file))
(assert-equal '(find-file-noselect "~/.emacs.d/init.el")
              (state-create switch-file))
(assert-equal '(get-buffer-create "*scratch*")
              (state-create switch-buf))
(assert-equal '(get-buffer-create "switch")
              (state-create in-and-switch))

(note "state-define-state:state-in")
(assert-equal '(string-prefix-p (file-truename "~/.emacs.d/")
                                (file-truename (or (buffer-file-name) default-directory "/")))
              (state-in in-directory))
(assert-equal '(eq (current-buffer) (find-buffer-visiting "~/.emacs.d/init.el"))
              (state-in switch-file))
(assert-equal '(eq (current-buffer) (get-buffer "*scratch*"))
              (state-in switch-buf))
(assert-equal '(string-prefix-p (file-truename "in")
                                (file-truename (or (buffer-file-name) default-directory "/")))
              (state-in in-and-switch))
(assert-raises error
               (state-define-state
                err
                :key "E"
                :switch func))

(note "state-define-state:state-exist")
(assert-equal '(catch (quote found)
                 (progn
                   (mapc
                    (lambda (buf)
                      (if (string-prefix-p
                           (file-truename "~/.emacs.d/")
                           (file-truename (with-current-buffer buf (or (buffer-file-name) default-directory "/"))))
                          (throw (quote found) t)))
                    (buffer-list))
                   nil))
              (state-exist in-directory))
(assert-equal '(get-buffer "*scratch*")
              (state-exist switch-buf))
(assert-equal '(catch (quote found)
                 (progn
                   (mapc
                    (lambda (buf)
                      (if (string-prefix-p
                           (file-truename "in")
                           (file-truename (with-current-buffer buf (or (buffer-file-name) default-directory "/"))))
                          (throw (quote found) t)))
                    (buffer-list))
                   nil))
              (state-exist in-and-switch))

(note "state-define-state:state-switch")
(assert-equal '(let ((state (state--get-state-by-name (quote in-directory))))
                 (if (window-configuration-p (state-current state))
                     (set-window-configuration (state-current state))
                   (let ((buffer
                          (or (catch (quote found)
                                (progn (mapc
                                        (lambda (buf)
                                          (if (string-prefix-p
                                               (file-truename "~/.emacs.d/")
                                               (file-truename (with-current-buffer buf
                                                                (or (buffer-file-name) default-directory "/"))))
                                              (throw (quote found) buf)))
                                        (buffer-list))
                                       nil))
                              (and (file-directory-p "~/.emacs.d/")
                                   (dired-noselect "~/.emacs.d/"))
                              (error "Unable to switch to state %s" (quote in-directory)))))
                     (delete-other-windows)
                     (switch-to-buffer buffer))))
              (state-switch in-directory))
(assert-equal '(if current-prefix-arg
                   (switch-to-buffer-other-window (find-file-noselect "~/.emacs.d/init.el"))
                 (find-file-existing "~/.emacs.d/init.el"))
              (state-switch switch-file))
(assert-equal '(if current-prefix-arg
                   (switch-to-buffer-other-window "*scratch*")
                 (switch-to-buffer "*scratch*"))
              (state-switch switch-buf))
(assert-equal '(let ((state (state--get-state-by-name (quote in-sexp))))
                 (if (window-configuration-p (state-current state))
                     (set-window-configuration (state-current state))))
              (state-switch in-sexp))
(assert-equal '(if current-prefix-arg
                   (switch-to-buffer-other-window "switch")
                 (switch-to-buffer "switch"))
              (state-switch in-and-switch))

(note "state-define-state:state-before")
(assert-equal
 '(let ((state (state--get-state-by-name (quote in-directory))))
    (when state
      (setf (state-current state) (current-window-configuration))))
 (state-before in-directory))

(note "state-define-state:in:string:directory")
(note "state-define-state:in:string:file")
(end-tests)
