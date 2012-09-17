lisp-interface-library
======================

LIL: abstract interfaces and supporting concrete data-structures in Common Lisp

Home Page:
	http://github.com/fare/lisp-interface-library/

We are currently writing an article explaining about our library:
	http://github.com/fare/lil-ilc2012/

Draft versions of the compiled PDF and html results are here:
	* PDF: http://common-lisp.net/~frideau/lil-ilc2012/lil-ilc2012.pdf
	* HTML: http://common-lisp.net/~frideau/lil-ilc2012/lil-ilc2012.html

A short introduction on our "Interface-Passing Style" of programming, is also at:
	http://fare.livejournal.com/155094.html

Features:

* lil sports both pure (persistent) and stateful (ephemeral) variants of datastructures,
 with a common core for read-only operations, and automatic transforms to go from
 stateful to pure (linearization) and back (boxing).
