lisp-interface-library
======================

LIL: abstract interfaces and supporting concrete data structures in Common Lisp

Home Page:
	http://github.com/fare/lisp-interface-library/

We wrote an article explaining about our library for ILC'2012:
	http://github.com/fare/lil-ilc2012/

A PDF of a slightly corrected version of the article is here:
	http://common-lisp.net/~frideau/lil-ilc2012/lil-ilc2012.pdf

An HTML	of that version of the article is here:
	http://common-lisp.net/~frideau/lil-ilc2012/lil-ilc2012.html

A short introduction on our "Interface-Passing Style" of programming, is also at:
	http://fare.livejournal.com/155094.html

Features:

* LIL sports both pure (persistent) and stateful (ephemeral) variants
 of data structures, with a common core for read-only operations,
 and automatic transforms to go from stateful to pure and back,
 in the respective packages PURE and STATEFUL.

* LIL supports data structures in traditional Object-Oriented Style,
 in both stateful (the usual OO kind) and pure variants,
 in they respective packages POSH and CLASSY,
 with automatic transforms to go from Interface-Passing Style to
 traditional Object-Oriented Style and back.
