                          ABOUT SLATEC

The SLATEC Common Mathematical Library is an extensive public-domain
FORTRAN source code library (consisting of more than 1400 routines and
300,000 lines of code and documentation!) developed and maintained by
a consortium of Department of Energy and Department of Defense
national laboratories.  Version 4.1 of the library was released in
July, 1993.

SLATEC incorporates within itself several other public domain
packages, including EISPACK, FFTPACK, QUADPACK, FNLIB, BSPLINE, SLAP,
and others.

SLATEC is distributed, in the first instance, by the NATIONAL ENERGY
SOFTWARE CENTER, 9700 Cass Ave., Argonne, Illinois 60439.  All
questions about the library should be directed there, and NOT to the
authors of the subroutines, nor to any other distribution point (and
especially not to Numerical Recipes Software!).

The full SLATEC release is also available from "netlib": On the Web,
go to http://netlib.cs.utk.edu or http://netlib2.cs.utk.edu.  By
email, send the one-line message "send index" (not including the
quotes) to netlib@netlib.org.

We at Numerical Recipes Software frequently recommend SLATEC to users
who are looking for freely redistributable source code routines, since
the Numerical Recipes themselves are copyrighted and not freely
redistributable.  Naturally, we think that Numerical Recipes offers
quality and clarity superior to SLATEC; but SLATEC is a good
alternative when public-domain code is required, or for those highly
specialized capabilities that are in SLATEC and not in Numerical
Recipes.  We likewise recommend SLATEC as a good alternative to the
commercial, and therefore restrictively licensed, IMSL and NAG
libraries.

In fact, SLATEC's sheer size poses a significant barrier to its use.
The routines are highly interdependent, so to use a single routine,
you usually have to identify and unpack between 6 and 20 subsidiary
ones.  That is not a problem is you install the whole package as a
(huge!) linkable library, but it does pose difficulties for someone
who wants to use, and possibly redistribute a small subset of the
library.

As a public service, therefore, we have undertaken the arduous task of
unpacking the full SLATEC library into a multiply cross-linked
directory tree.  This tree allows routines and their documentation to
be located, using a Web browser, either by subject or name.  Most
importantly, it allows a routine *and its required subsidiary
routines* to be efficiently located in source-code form.

Once in the SLATEC top directory, there are several things to
remember: (1) Almost *all* the routines require at least several of
the subroutines that we have collected in the file (top level of
SLATEC directory) "CMLUTILS.F".  So, be SURE to download this file,
compile and link it with any other routines.  (2) Documentation for
each routine (each .F file) is in a similarly-named .TXT file, in the
same subject-classified subdirectory of the tree.  (In the SLATEC
library as released, the documentation is embedded in each FORTRAN
file.  (3) When a routine requires *additional* subsidiary routines,
there are links to these routines in a dependencies directory, visible
at the same level as the .F file.  Example: to use the routine CBESJ
(Bessel function of complex argument), you must download the files
CMLUTILS.F (from the top directory), CBESJ.F and CBESJ.TXT (from the
link through GAMS number C10A4, taking you to the directory
DIRS/_CBESJ), and all the routines in "DIRS/_CBESJ/DEPENDS" .

SLATEC is completely in the public domain.  In that spirit, we freely
put the SLATEC HTML finder files GAMS.HTM and TOC.HTM into the public
domain, as well as this information file.  You can thus freely copy
the entire SLATEC directory tree on this CDROM onto any computer or
server that you want.  Please note that this applies only to files
under the SLATEC directory, and NOT to the Numerical Recipes files
elsewhere on this CDROM, which are copyrighted.
