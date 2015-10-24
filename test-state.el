;;;; (executable-interpret (format "emacs -batch -L %s -l %s" (file-name-directory (locate-library "test-simple.elc")) buffer-file-name))
(require 'test-simple)
(test-simple-start)

(assert-t (load-file "./state.el") "Can't load ./state.el")

(note "state-define-state")

(assert-equal '(define-key state-prefix-map (kbd "0")
                 (lambda ()
                   "Switch to state `test'"
                   (interactive)
                   (state--do-switch "0")))
              (macroexpand '(state-define-state test
                              :key "0"
                              :in fake)))

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
(state-define-state create-in-exist-switch-before
  :key "f"
  :create func-create
  :in func-in
  :exist func-exist
  :switch func-switch
  :before func-before)
(setq in-directory (state--get-state-by-name 'in-directory))
(setq in-file (state--get-state-by-name 'in-file))
(setq switch-file (state--get-state-by-name 'switch-file))
(setq switch-buf (state--get-state-by-name 'switch-buf))
(setq in-sexp (state--get-state-by-name 'in-sexp))
(setq in-and-switch (state--get-state-by-name 'in-and-switch))
(setq create-in-exist-switch-before (state--get-state-by-name 'create-in-exist-switch-before))

(note "state-define-state:state-key")
(assert-raises error
               (state-define-state no-key :switch func-switch)
               "no key")
(assert-equal "a" (state-key in-directory))

(note "state-define-state:state-create")
(assert-equal 'func-create (state-create create-in-exist-switch-before))

(assert-equal '(state--create-in-directory "~/.emacs.d/")
              (state-create in-directory))
(assert-equal '(state--create-in-file "~/.emacs.d/init.el")
              (state-create in-file))
(assert-equal '(state--create-switch-file "~/.emacs.d/init.el")
              (state-create switch-file))
(assert-equal '(state--create-switch-buffer "*scratch*")
              (state-create switch-buf))
(assert-equal '(state--create-switch-buffer "switch")
              (state-create in-and-switch))


(note "state-define-state:state-in")
(assert-equal 'func-in (state-in create-in-exist-switch-before))
(assert-equal '(state--in-in-file "~/.emacs.d/")
              (state-in in-directory))
(assert-equal '(state--in-switch-file "~/.emacs.d/init.el")
              (state-in switch-file))
(assert-equal '(state--in-switch-buffer "*scratch*")
              (state-in switch-buf))
(assert-equal '(state--in-in-file "in")
              (state-in in-and-switch))
(assert-raises error
               (state-define-state err
                 :key "E"
                 :switch func)
               "no in")

(note "state-define-state:state-exist")
(assert-equal 'func-exist (state-exist create-in-exist-switch-before))
(assert-equal '(state--exist-in-file "~/.emacs.d/")
              (state-exist in-directory))
(assert-equal '(state--exist-switch-buffer "*scratch*")
              (state-exist switch-buf))
(assert-equal '(state--exist-in-file "in")
              (state-exist in-and-switch))

(note "state-define-state:state-switch")
(assert-equal 'func-switch (state-switch create-in-exist-switch-before))
(assert-equal '(state--switch-in-file "~/.emacs.d/" 'in-directory)
              (state-switch in-directory))
(assert-equal '(state--switch-switch-file "~/.emacs.d/init.el")
              (state-switch switch-file))
(assert-equal '(state--switch-switch-buffer "*scratch*")
              (state-switch switch-buf))
(assert-equal '(state--switch-default 'in-sexp)
              (state-switch in-sexp))
(assert-equal '(state--switch-switch-buffer "switch")
              (state-switch in-and-switch))

(note "state-define-state:state-before")
(assert-equal 'func-before (state-before create-in-exist-switch-before))
(assert-equal '(state--before-default 'in-directory)
              (state-before in-directory))

(note "priority")

(state-define-state 1 :key "a" :in "a" :bound 1 :priority 10)
(state-define-state 2 :key "a" :in "a" :bound 1)
(assert-equal '(2) (mapcar 'state-name (state--select-states "a" 'default)))

(state-define-state 3 :key "b" :in "a" :bound 1 :priority 10)
(state-define-state 4 :key "b" :in "a" :bound 1 :priority 5)
(state-define-state 5 :key "b" :in "a" :bound 1 :priority 7)
(assert-equal '(4) (mapcar 'state-name (state--select-states "b" 'default)))

(state-define-state 6 :key "c" :in "a" :bound 1 :priority 10)
(state-define-state 7 :key "c" :in "a" :bound 1 :priority 5)
(state-define-state 8 :key "c" :in "a" :bound 1 :priority 5)
(assert-equal '(7 8) (sort (mapcar 'state-name (state--select-states "c" 'default)) '<))

(end-tests)
