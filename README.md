lisp-interface-library
======================

LIL: abstract interfaces and concrete data structures in Common Lisp

 * Home Page:
	* http://github.com/fare/lisp-interface-library/

 * We wrote an article explaining about our library for ILC'2012:
	* http://github.com/fare/lil-ilc2012/

 * A PDF of a slightly corrected version of the article is here:
	* http://common-lisp.net/~frideau/lil-ilc2012/lil-ilc2012.pdf

 * An HTML of that version of the article is here:
	* http://common-lisp.net/~frideau/lil-ilc2012/lil-ilc2012.html

 * A short introduction on our "Interface-Passing Style" of programming, is also at:
	* http://fare.livejournal.com/155094.html


Features
--------

 * LIL nicely combines ad-hoc polymorphism (CLOS-powered object oriented programming)
   and parametric polymorphism (as in ML functors, C++ templates, etc.).
   To our knowledge, only scalaz matches the expressiveness of LIL.

 * LIL sports both pure (persistent, immutable) and
   stateful (ephemeral, mutable) variants
   of data structures in Interface-Passing Style.
   This variants are in the respective packages PURE and STATEFUL;
   a common core is shared in package INTERFACE, covering read-only operations;
   automatic transforms allow bridging from stateful to pure and back.

 * LIL supports data structures in traditional Object-Oriented Style,
   in both stateful (the usual Object-Oriented kind) and pure variants,
   in the respective packages POSH and CLASSY.
   There too, automatic transforms to go from Interface-Passing Style to
   traditional Object-Oriented Style and back.


Building it
-----------

LIL needs to be built using ASDF 3 or later:

  * http://common-lisp.net/gitweb?p=projects/asdf/asdf.git


LIL was recently converted to use the asdf-package-system,
whereby instead of dependencies being listed in a central .asd file,
each source file has a defpackage form from which the dependencies are deduced.
While we think it's a good way to write Lisp code,
you don't have to use it in your own code to use LIL.
We're hoping that asdf-package-system will soon be part of ASDF itself,
but until an updated version of ASDF is universally available,
you can find it here:

 * http://common-lisp.net/gitweb?p=projects/asdf/asdf-package-system.git


LIL also depends on fare-utils, fare-memoization, closer-mop, alexandria:

 * http://common-lisp.net/gitweb?p=users/frideau/fare-utils.git

 * http://common-lisp.net/gitweb?p=users/frideau/fare-memoization.git

 * http://common-lisp.net/project/closer/repos/closer-mop/

 * http://common-lisp.net/gitweb?p=projects/xcvb/closer-mop.git

 * http://common-lisp.net/gitweb?p=projects/alexandria/alexandria.git
