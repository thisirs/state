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

(defmacro setq-state (var &rest args)
  "[Test helper]Define state and set it to VAR"
  `(progn
     (state-define-state ,var ,@args)
     (setq ,var (state--get-state-by-name ',var))))
(put 'setq-state 'lisp-indent-function 1)

(setq-state in-directory
  :key "a"
  :in "~/.emacs.d/")

(setq-state in-file
  :key "b"
  :in "~/.emacs.d/init.el")
(setq-state switch-file
  :key "c"
  :switch "~/.emacs.d/init.el")

(setq-state switch-buf
  :key "d"
  :switch "*scratch*")

(setq-state in-sexp
  :key "e"
  :in (ignore 1))
(setq-state in-and-switch
  :key "f"
  :in "in"
  :switch "switch")

(fset 'func-in 'ignore)
(setq-state create-in-exist-switch-before
  :key "g"
  :create func-create
  :in func-in
  :exist func-exist
  :switch func-switch
  :before func-before)

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

(note "priority:bound")
(state-define-state 1 :key "C-a" :in "a" :bound 1 :priority 10)
(state-define-state 2 :key "C-a" :in "a" :bound 1)
(assert-equal '(2) (mapcar 'state-name (state--select-states "C-a" 'default)))

(state-define-state 3 :key "C-b" :in "a" :bound 1 :priority 10)
(state-define-state 4 :key "C-b" :in "a" :bound 1 :priority 5)
(state-define-state 5 :key "C-b" :in "a" :bound 1 :priority 7)
(assert-equal '(4) (mapcar 'state-name (state--select-states "C-b" 'default)))

(state-define-state 6 :key "C-c" :in "a" :bound 1 :priority 10)
(state-define-state 7 :key "C-c" :in "a" :bound 1 :priority 5)
(state-define-state 8 :key "C-c" :in "a" :bound 1 :priority 5)
(assert-equal '(7 8) (sort (mapcar 'state-name (state--select-states "C-c" 'default)) '<))


(note "priority:unbound")
(state-define-state  9 :key "C-d" :in "a" :priority 10)
(state-define-state 10 :key "C-d" :in "a")
(assert-equal '(9 10) (sort (mapcar 'state-name (state--select-states "C-d" 'default)) '<))

(note "priority:bound and unbound")
(state-define-state 11 :key "C-d" :in "a" :bound 1 :priority 9999)
(assert-equal '(11) (sort (mapcar 'state-name (state--select-states "C-d" 'default)) '<))

(note "state--get-state-in")
(set-buffer "*scratch*")
(assert-equal switch-buf (state--get-state-in))

(note "state--do-switch: new state")
(setq-state another-switch-buf
  :key "A"
  :switch "*another-scratch*")
(setq scratch-winconf (current-window-configuration))
(state--do-switch "A")
(assert-equal another-switch-buf (state--get-state-in))
;; save previous state to origin
(assert-equal 'switch-buf (state-origin another-switch-buf))
;; save window configuration before switching state
(assert-equal scratch-winconf (state-current switch-buf))

(note "state--do-switch: same state (switch to previous state)")
(state--do-switch "A")
(assert-equal switch-buf (state--get-state-in))

(note "state--do-switch: switch back")
(state--do-switch "A")
(assert-equal another-switch-buf (state--get-state-in))


(end-tests)
