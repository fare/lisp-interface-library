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

 * Manual: I have never worked on a manual, nor on a tutorial.
   For a quick introduction to how things work, look at the tests in `test/`
   for a few examples.
   Then docstrings and source code can get you somewhat further.
   A manual or a tutorial would be a great contribution to this library.


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

LIL needs to be built using ASDF 3.1 or later:

  * https://common-lisp.net/project/asdf/
  * https://gitlab.common-lisp.net/asdf/asdf.git

Indeed, LIL notably relies on ASDF 3.1's `asdf-package-system`.
whereby instead of dependencies being listed in a central .asd file,
each source file has a defpackage form from which the dependencies are deduced.
While we think it's a great way to write Lisp code,
you don't have to use it in your own code to use LIL.


LIL also depends on fare-utils, fare-memoization, closer-mop, alexandria:

 * https://gitlab.common-lisp.net/frideau/fare-utils.git

 * https://gitlab.common-lisp.net/frideau/fare-memoization.git

 * http://sourceforge.net/p/closer/closer-mop/ci/master/tree/

 * https://gitlab.common-lisp.net/alexandria/alexandria.git
