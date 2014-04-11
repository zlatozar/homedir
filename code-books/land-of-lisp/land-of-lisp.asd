;;;; land-of-lisp.asd

(asdf:defsystem #:land-of-lisp
  :serial t
  :description "Source code from 'Land of Lisp' book"
  :author "Zlatozar Zhelyazkov <zlatozar@gmail.com>"
  :license "MIT"

  :depends-on (#:usocket)

  :components ((:file "package")
			   (:file "guess-my-number")
			   (:file "wizards-adventure")
			   (:file "game-repl")
			   (:file "graph-util/graph-util")
			   (:file "grand-theft-wumpus")
			   (:file "orc-battle")
			   (:file "loop-game")
			   (:file "robots")
			   ;; (:file "sockets/server")
			   ;; (:file "sockets/client")
			   (:file "http")
			   (:file "dice_of_doom_v1")
			   (:file "svg/svg-util")
			   (:file "svg/svg")
			   (:file "game-repl-enhanced")
			   (:file "lazy")
			   (:file "dice_of_doom_v2")
			   (:file "dice_of_doom_v3")
			   (:file "dice_of_doom_v4")))
