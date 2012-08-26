lisp-interface-library
======================

LIL: abstract interfaces and supporting concrete data-structures in Common Lisp

Home Page: http://github.com/fare/lisp-interface-library/

On our "Interface-Passing Style" of programming, see
	http://fare.livejournal.com/155094.html

We are currently writing an article explaining about our library:
	http://github.com/fare/lil-ilc2012/


Features:

* lil sports both pure (persistent) and stateful (ephemeral) variants of datastructures,
 with a common core for read-only operations, and automatic transforms to go from
 stateful to pure (linearization) and back (boxing).
