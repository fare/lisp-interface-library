lisp-interface-library
======================

LIL: abstract interfaces and supporting concrete data structures in Common Lisp

 * Home Page:
	http://github.com/fare/lisp-interface-library/

 * We wrote an article explaining about our library for ILC'2012:
	http://github.com/fare/lil-ilc2012/

 * A PDF of a slightly corrected version of the article is here:
	http://common-lisp.net/~frideau/lil-ilc2012/lil-ilc2012.pdf

 * An HTML of that version of the article is here:
	http://common-lisp.net/~frideau/lil-ilc2012/lil-ilc2012.html

 * A short introduction on our "Interface-Passing Style" of programming, is also at:
	http://fare.livejournal.com/155094.html


Features
--------

 * LIL sports both pure (persistent) and stateful (ephemeral) variants
   of data structures in Interface-Passing Style,
   in the respective packages PURE and STATEFUL,
   with a common core for read-only operations
   and automatic transforms to go from stateful to pure and back.

 * LIL supports data structures in traditional Object-Oriented Style,
   in both stateful (the usual Object-Oriented kind) and pure variants,
   in the respective packages POSH and CLASSY,
   with automatic transforms to go from Interface-Passing Style to
   traditional Object-Oriented Style and back.


Building it
-----------

Note that LIL was recently converted to use the asdf-package-system,
whereby instead of dependencies being listed in a .asd, each file has a defpackage form
from which the dependencies are deduced. We're hoping that this will soon be
part of ASDF itself, but in the meantime, you can find it here::
	http://common-lisp.net/gitweb?p=projects/asdf/asdf-package-system.git

LIL needs to be built using ASDF 3 or later::
	http://common-lisp.net/gitweb?p=projects/asdf/asdf.git

LIL also depends on fare-utils, fare-memoization, closer-moo, alexandria::
	http://common-lisp.net/gitweb?p=users/frideau/fare-utils.git
	http://common-lisp.net/gitweb?p=users/frideau/fare-memoization.git
        http://common-lisp.net/project/closer/repos/closer-mop/
	http://common-lisp.net/gitweb?p=projects/xcvb/closer-mop.git
	http://common-lisp.net/gitweb?p=projects/alexandria/alexandria.git

