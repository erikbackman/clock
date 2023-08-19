(defsystem clock
  :name "clock"
  :version "0.0.1"
  :components ((:file "package")
	       (:file "main"))
  :depends-on (:sdl2 :cl-opengl)
  :build-operation "program-op"
  :build-pathname "clock"
  :entry-point "clock::run")
