;;;; package.lisp

(defpackage #:chapter-2/guess-my-number
  (:use #:cl))

(defpackage #:chapter-5/wizards-adventure
  (:use #:cl))

(defpackage #:chapter-6/game-repl
  (:use #:cl))

(defpackage #:chapter-7/graph-util
  (:use :cl)
  (:export #:graph->dot
		   #:dot->png
		   #:graph->png
		   #:ugraph->dot
		   #:ugraph->png))

(defpackage #:chapter-8/grand-theft-wumpus
  (:use #:cl
		#:chapter-7/graph-util))

(defpackage #:chapter-9/orc-battle
  (:use #:cl))

(defpackage #:chapter-10/loop-game
  (:use #:cl))

(defpackage #:chapter-11/robots
  (:use #:cl))

(defpackage #:chapter-12/sockets
  (:use #:cl))

(defpackage #:chapter-13/http
  (:use #:cl
		#:usocket)
  (:export #:serve))

(defpackage #:chapter-15/dice_of_doom_v1
  (:use #:cl))

(defpackage #:chapter-17/svg-util
  (:use #:cl)
  (:export #:splint
		   #:pairs
		   #:print-tag
		   #:tag))

(defpackage #:chapter-17/svg
  (:use #:cl
		#:chapter-17/svg-util)
  (:export #:svg
		   #:brightness
		   #:svg-style
		   #:circle
		   #:polygon))

(defpackage #:chapter-17/game-repl-enhanced
  (:use #:cl))

(defpackage #:chapter-18/lazy
  (:use #:cl))

(defpackage #:chapter-18/dice_of_doom_v2
  (:use #:cl
		#:chapter-15/dice_of_doom_v1
		#:chapter-18/lazy))

(defpackage #:chapter-19/dice_of_doom_v3
  (:use #:cl
		#:chapter-18/dice_of_doom_v2
		#:chapter-13/http
		#:chapter-17/svg))

(defpackage #:chapter-20/dice_of_doom_v4
  (:use #:cl
		#:chapter-19/dice_of_doom_v3))
