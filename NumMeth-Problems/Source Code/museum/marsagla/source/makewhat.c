/* makewhat.f -- translated by f2c (version 19950110).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__9 = 9;
static integer c__1 = 1;
static integer c__3 = 3;
static integer c__22 = 22;
static integer c__5 = 5;
static integer c__4096 = 4096;
static integer c__27 = 27;
static integer c__23 = 23;
static integer c__16 = 16;
static integer c__15 = 15;
static integer c__17 = 17;
static doublereal c_b389 = 4294967291.;
static doublereal c_b392 = 34359737519.;
static doublereal c_b393 = 34359736739.;
static integer c__20 = 20;
static integer c__13 = 13;
static doublereal c_b546 = 2.;
static integer c__54 = 54;
static integer c__12 = 12;
static integer c__18 = 18;
static doublereal c_b842 = 4294967296.;

/* ****Makewhat.f: program to make random number files for DIEHARD */
/* Main program */ MAIN__()
{
    /* Format strings */
    static char fmt_123[] = "(\002 This program makes a file of random integ\
ers \002,/,\002 for tests by DIEHARD.  Select one from this list:\002,/)";
    static char fmt_124[] = "(\002  1.  A multiply-with-carry (MWC) generato\
r x(n)=a*x(n-1)+carry  mod 2^32\002,/,\002  2.  A MWC generator on pairs of \
16 bits\002,/,\002  3.  The \"Mother of all random number generators\"\002,/,\
\002  4.  The KISS  generator\002,/,\002  5.  The simple but very good gener\
ator COMBO\002,/,\002  6.  The lagged Fibonacci-MWC combination ULTRA\002,/\
,\002  7.  A combination MWC/subtract-with-borrow (SWB) generator, period ~ \
10^364\002,/,\002  8.  An extended congruential generator\002,/,\002  9.  Th\
e Super-Duper generator\002,/,\002 10.  A subtract-with-borrow generator\002\
,/,\002 11.  Any specified congruential generator\002,/,\002 12.  The 31-bit\
 generator ran2 from Numerical Recipes\002,/,\002 13.  Any specified shift-r\
egister generator, 31 or 32 bits\002,/,\002 14.  The system generator in Sun\
 Fortran f77\002,/,\002 15.  Any lagged-Fibonacci generator,  x(n)=x(n-r) op\
 x(n-s)\002,/,\002 16.  An inverse congruential generator\002,/)";

    /* System generated locals */
    olist o__1;

    /* Builtin functions */
    integer s_wsfe(), e_wsfe(), f_open(), s_wsle(), do_lio(), e_wsle(), 
	    s_rsle(), e_rsle();

    /* Local variables */
    extern /* Subroutine */ int makefibo_(), makecmbo_(), makesbmc_(), 
	    makecong_(), makexcng_(), makeinvc_(), makeltra_(), makeshrg_(), 
	    makekiss_(), makemthr_(), makesunr_(), makesupr_(), make1616_();
    static integer jch;
    extern /* Subroutine */ int makeswb_(), makeran2_(), makemwc1_();

    /* Fortran I/O blocks */
    static cilist io___1 = { 0, 6, 0, fmt_123, 0 };
    static cilist io___2 = { 0, 6, 0, fmt_124, 0 };
    static cilist io___3 = { 0, 6, 0, 0, 0 };
    static cilist io___4 = { 0, 5, 0, 0, 0 };


    s_wsfe(&io___1);
    e_wsfe();
    s_wsfe(&io___2);
    e_wsfe();
    o__1.oerr = 0;
    o__1.ounit = 4;
    o__1.ofnmlen = 9;
    o__1.ofnm = "makef.txt";
    o__1.orl = 0;
    o__1.osta = 0;
    o__1.oacc = 0;
    o__1.ofm = 0;
    o__1.oblnk = 0;
    f_open(&o__1);
    s_wsle(&io___3);
    do_lio(&c__9, &c__1, "  Enter your choice, 1 to 16:", 29L);
    e_wsle();
    s_rsle(&io___4);
    do_lio(&c__3, &c__1, (char *)&jch, (ftnlen)sizeof(integer));
    e_rsle();
    if (jch == 1) {
	makemwc1_();
    }
    if (jch == 2) {
	make1616_();
    }
    if (jch == 3) {
	makemthr_();
    }
    if (jch == 4) {
	makekiss_();
    }
    if (jch == 5) {
	makecmbo_();
    }
    if (jch == 6) {
	makeltra_();
    }
    if (jch == 7) {
	makesbmc_();
    }
    if (jch == 8) {
	makexcng_();
    }
    if (jch == 9) {
	makesupr_();
    }
    if (jch == 10) {
	makeswb_();
    }
    if (jch == 11) {
	makecong_();
    }
    if (jch == 12) {
	makeran2_();
    }
    if (jch == 13) {
	makeshrg_();
    }
    if (jch == 14) {
	makesunr_();
    }
    if (jch == 15) {
	makefibo_();
    }
    if (jch == 16) {
	makeinvc_();
    }
} /* MAIN__ */

/* Subroutine */ int makemwc1_()
{
    /* Format strings */
    static char fmt_845[] = "(a78)";
    static char fmt_679[] = "(a1)";
    static char fmt_2991[] = "(a15)";
    static char fmt_281[] = "(\002 (In general, for any choice of `a`, let m\
=a*2^32-1. If both m\002,/,\002 and (m-1)/2 are prime then the period will b\
e (m-1)/2).\002)";
    static char fmt_602[] = "(\002  2,867,200 32-bit random integers (11,468\
,800 bytes)\002,/,\002  have been written to the file \002,a15)";

    /* System generated locals */
    integer i__1;
    doublereal d__1;
    olist o__1;

    /* Builtin functions */
    integer s_rsfe(), do_fio(), e_rsfe(), s_wsfe(), e_wsfe(), s_wsle(), 
	    do_lio(), e_wsle(), f_open(), s_rsle(), e_rsle(), s_wdue(), 
	    do_uio(), e_wdue();

    /* Local variables */
    static char filename[15];
    extern /* Subroutine */ int prod_();
    static char text[80*22];
    static integer a[2], c[4], i, j, n[4096], w[4], x[2], z[4];
    static doublereal aa;
    static integer cc, jk;
    static char op[1];
    static integer xx;
    extern /* Subroutine */ int sum_();

    /* Fortran I/O blocks */
    static cilist io___6 = { 0, 4, 0, fmt_845, 0 };
    static cilist io___8 = { 0, 6, 0, fmt_845, 0 };
    static cilist io___9 = { 0, 6, 0, 0, 0 };
    static cilist io___10 = { 0, 5, 0, fmt_679, 0 };
    static cilist io___12 = { 0, 6, 0, 0, 0 };
    static cilist io___13 = { 0, 5, 0, fmt_2991, 0 };
    static cilist io___15 = { 0, 6, 0, 0, 0 };
    static cilist io___16 = { 0, 6, 0, 0, 0 };
    static cilist io___17 = { 0, 6, 0, 0, 0 };
    static cilist io___18 = { 0, 6, 0, 0, 0 };
    static cilist io___19 = { 0, 6, 0, 0, 0 };
    static cilist io___20 = { 0, 6, 0, 0, 0 };
    static cilist io___21 = { 0, 6, 0, 0, 0 };
    static cilist io___22 = { 0, 6, 0, fmt_281, 0 };
    static cilist io___23 = { 0, 6, 0, 0, 0 };
    static cilist io___24 = { 0, 5, 0, 0, 0 };
    static cilist io___26 = { 0, 6, 0, 0, 0 };
    static cilist io___27 = { 0, 5, 0, 0, 0 };
    static cilist io___30 = { 0, 6, 0, 0, 0 };
    static cilist io___40 = { 0, 2, 0, 0, 0 };
    static cilist io___41 = { 0, 6, 0, 0, 0 };
    static cilist io___42 = { 0, 6, 0, fmt_602, 0 };
    static cilist io___43 = { 0, 6, 0, 0, 0 };


    s_rsfe(&io___6);
    do_fio(&c__22, text, 80L);
    e_rsfe();
    s_wsfe(&io___8);
    do_fio(&c__22, text, 80L);
    e_wsfe();
    s_wsle(&io___9);
    do_lio(&c__9, &c__1, "  Pause:  enter any letter or number key to contin\
ue:", 53L);
    e_wsle();
/* L679: */
    s_rsfe(&io___10);
    do_fio(&c__1, op, 1L);
    e_rsfe();
    s_wsle(&io___12);
    do_lio(&c__9, &c__1, "  Enter filename (<=15 characters) for output:", 
	    46L);
    e_wsle();
    s_rsfe(&io___13);
    do_fio(&c__1, filename, 15L);
    e_rsfe();
    o__1.oerr = 0;
    o__1.ounit = 2;
    o__1.ofnmlen = 15;
    o__1.ofnm = filename;
    o__1.orl = 16384;
    o__1.osta = 0;
    o__1.oacc = "direct";
    o__1.ofm = "unformatted";
    o__1.oblnk = 0;
    f_open(&o__1);
    s_wsle(&io___15);
    do_lio(&c__9, &c__1, "      Select multiplier `a` from this list:", 43L);
    e_wsle();
    s_wsle(&io___16);
    do_lio(&c__9, &c__1, "--------------------------------------------------\
-----", 55L);
    e_wsle();
    s_wsle(&io___17);
    do_lio(&c__9, &c__1, " 1791398085 1929682203 1683268614 1965537969 16753\
93560", 55L);
    e_wsle();
    s_wsle(&io___18);
    do_lio(&c__9, &c__1, " 1967773755 1517746329 1447497129 1655692410 16062\
18150", 55L);
    e_wsle();
    s_wsle(&io___19);
    do_lio(&c__9, &c__1, " 2051013963 1075433238 1557985959 1781943330 18935\
13180", 55L);
    e_wsle();
    s_wsle(&io___20);
    do_lio(&c__9, &c__1, " 1631296680 2131995753 2083801278 1873196400 15541\
15554", 55L);
    e_wsle();
    s_wsle(&io___21);
    do_lio(&c__9, &c__1, "--------------------------------------------------\
-----", 55L);
    e_wsle();
    s_wsfe(&io___22);
    e_wsfe();
    s_wsle(&io___23);
    do_lio(&c__9, &c__1, "   Enter multiplier a:", 22L);
    e_wsle();
    s_rsle(&io___24);
    do_lio(&c__5, &c__1, (char *)&aa, (ftnlen)sizeof(doublereal));
    e_rsle();
    s_wsle(&io___26);
    do_lio(&c__9, &c__1, "  Enter a seed integer x and initial carry c:", 45L)
	    ;
    e_wsle();
    s_rsle(&io___27);
    do_lio(&c__3, &c__1, (char *)&xx, (ftnlen)sizeof(integer));
    do_lio(&c__3, &c__1, (char *)&cc, (ftnlen)sizeof(integer));
    e_rsle();
    s_wsle(&io___30);
    do_lio(&c__9, &c__1, "       Please wait..............", 32L);
    e_wsle();
    x[0] = xx & 65535;
    x[1] = xx >> 16;
    c[0] = cc & 65535;
    c[1] = cc >> 16;
    c[2] = 0;
    c[3] = 0;
    i__1 = (integer) aa;
    a[0] = i__1 & 65535;
    i__1 = (integer) aa;
    a[1] = i__1 >> 16;
    jk = 0;
    for (i = 1; i <= 700; ++i) {
	++jk;
	for (j = 1; j <= 4096; ++j) {
	    prod_(x, a, z);
	    sum_(z, c, w);
	    x[0] = w[0];
	    x[1] = w[1];
	    c[0] = w[2];
	    c[1] = w[3];
	    n[j - 1] = (x[1] << 16) + x[0];
/* L3: */
	}
/* L2: */
	io___40.cirec = jk;
	s_wdue(&io___40);
	do_uio(&c__4096, (char *)&n[0], (ftnlen)sizeof(integer));
	e_wdue();
    }
    s_wsle(&io___41);
    do_lio(&c__9, &c__1, "            FINISHED", 20L);
    e_wsle();
    s_wsfe(&io___42);
    do_fio(&c__1, filename, 15L);
    e_wsfe();
    s_wsle(&io___43);
    do_lio(&c__9, &c__1, "  The period is", 15L);
    d__1 = aa * 2147483648. - 1.;
    do_lio(&c__5, &c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
    e_wsle();
} /* makemwc1_ */

/* Subroutine */ int make1616_()
{
    /* Format strings */
    static char fmt_846[] = "(a78)";
    static char fmt_800[] = "(f40.0)";
    static char fmt_801[] = "(\002  The correct last 4 digits are\002,i8)";

    /* System generated locals */
    integer i__1;
    doublereal d__1;
    olist o__1;

    /* Builtin functions */
    integer f_open(), s_rsfe(), do_fio(), e_rsfe(), s_wsfe(), e_wsfe();
    /* Subroutine */ int s_paus();
    integer s_wsle(), do_lio(), e_wsle(), s_rsle(), e_rsle(), s_wdue(), 
	    do_uio(), e_wdue();

    /* Local variables */
    static char text[80*27];
    static integer a, b, i, j, n[4096], x, y, jk;
    static char dum[80];

    /* Fortran I/O blocks */
    static cilist io___44 = { 0, 4, 0, fmt_846, 0 };
    static cilist io___47 = { 0, 4, 0, fmt_846, 0 };
    static cilist io___49 = { 0, 6, 0, fmt_846, 0 };
    static cilist io___50 = { 0, 6, 0, fmt_846, 0 };
    static cilist io___51 = { 0, 6, 0, 0, 0 };
    static cilist io___52 = { 0, 4, 0, fmt_846, 0 };
    static cilist io___53 = { 0, 6, 0, fmt_846, 0 };
    static cilist io___54 = { 0, 6, 0, 0, 0 };
    static cilist io___55 = { 0, 5, 0, 0, 0 };
    static cilist io___58 = { 0, 6, 0, 0, 0 };
    static cilist io___59 = { 0, 5, 0, 0, 0 };
    static cilist io___62 = { 0, 6, 0, 0, 0 };
    static cilist io___66 = { 0, 2, 0, 0, 0 };
    static cilist io___67 = { 0, 6, 0, 0, 0 };
    static cilist io___68 = { 0, 6, 0, 0, 0 };
    static cilist io___69 = { 0, 6, 0, 0, 0 };
    static cilist io___70 = { 0, 6, 0, 0, 0 };
    static cilist io___71 = { 0, 6, 0, 0, 0 };
    static cilist io___72 = { 0, 6, 0, fmt_800, 0 };
    static cilist io___73 = { 0, 6, 0, 0, 0 };
    static cilist io___74 = { 0, 6, 0, fmt_801, 0 };


    o__1.oerr = 0;
    o__1.ounit = 2;
    o__1.ofnmlen = 10;
    o__1.ofnm = "mwc1616.32";
    o__1.orl = 16384;
    o__1.osta = 0;
    o__1.oacc = "direct";
    o__1.ofm = "unformatted";
    o__1.oblnk = 0;
    f_open(&o__1);
    s_rsfe(&io___44);
    for (j = 1; j <= 22; ++j) {
	do_fio(&c__1, dum, 80L);
    }
    e_rsfe();
    s_rsfe(&io___47);
    do_fio(&c__27, text, 80L);
    e_rsfe();
    s_wsfe(&io___49);
    for (j = 1; j <= 21; ++j) {
	do_fio(&c__1, text + (j - 1) * 80, 80L);
    }
    e_wsfe();
    s_paus("", 0L);
    s_wsfe(&io___50);
    for (j = 22; j <= 27; ++j) {
	do_fio(&c__1, text + (j - 1) * 80, 80L);
    }
    e_wsfe();
    s_wsle(&io___51);
    do_lio(&c__9, &c__1, "      Select multipliers a and b, a<>b, from this \
list:", 55L);
    e_wsle();
    s_rsfe(&io___52);
    for (j = 1; j <= 10; ++j) {
	do_fio(&c__1, text + (j - 1) * 80, 80L);
    }
    e_rsfe();
    s_wsfe(&io___53);
    for (j = 1; j <= 10; ++j) {
	do_fio(&c__1, text + (j - 1) * 80, 80L);
    }
    e_wsfe();
    s_wsle(&io___54);
    do_lio(&c__9, &c__1, "      Enter a and b: (my favorites are 18000 and 3\
0903)", 55L);
    e_wsle();
    s_rsle(&io___55);
    do_lio(&c__3, &c__1, (char *)&a, (ftnlen)sizeof(integer));
    do_lio(&c__3, &c__1, (char *)&b, (ftnlen)sizeof(integer));
    e_rsle();
    s_wsle(&io___58);
    do_lio(&c__9, &c__1, " Enter two (<=31 bit) seed integers, not zero", 45L)
	    ;
    e_wsle();
    s_rsle(&io___59);
    do_lio(&c__3, &c__1, (char *)&x, (ftnlen)sizeof(integer));
    do_lio(&c__3, &c__1, (char *)&y, (ftnlen)sizeof(integer));
    e_rsle();
    s_wsle(&io___62);
    do_lio(&c__9, &c__1, "       Please wait..............", 32L);
    e_wsle();
    jk = 0;
    for (i = 1; i <= 700; ++i) {
	++jk;
	for (j = 1; j <= 4096; ++j) {
	    x = a * (x & 65535) + (x >> 16);
	    y = b * (y & 65535) + (y >> 16);
/* L3: */
	    n[j - 1] = (x << 16) + (y & 65535);
	}
/* L2: */
	io___66.cirec = jk;
	s_wdue(&io___66);
	do_uio(&c__4096, (char *)&n[0], (ftnlen)sizeof(integer));
	e_wdue();
    }
    s_wsle(&io___67);
    do_lio(&c__9, &c__1, "            FINISHED", 20L);
    e_wsle();
    s_wsle(&io___68);
    do_lio(&c__9, &c__1, "  2,867,200 32-bit random integers (11,468,800 byt\
es)", 53L);
    e_wsle();
    s_wsle(&io___69);
    do_lio(&c__9, &c__1, "  have been written to the file mwc1616.32        \
  ", 52L);
    e_wsle();
    s_wsle(&io___70);
    do_lio(&c__9, &c__1, "  They are the concatenated output of two 16-bit", 
	    48L);
    e_wsle();
    s_wsle(&io___71);
    do_lio(&c__9, &c__1, "  multiply-with-carry generators.  The period is", 
	    48L);
    e_wsle();
    s_wsfe(&io___72);
    d__1 = (a * 32768. - 1.) * (b * 32768. - 1.);
    do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
    e_wsfe();
    s_wsle(&io___73);
    do_lio(&c__9, &c__1, "  (the last 4 digits may be lost to roundoff).", 
	    46L);
    e_wsle();
/* L801: */
    s_wsfe(&io___74);
    i__1 = ((a << 15) - 1) % 10000 * (((b << 15) - 1) % 10000) % 10000;
    do_fio(&c__1, (char *)&i__1, (ftnlen)sizeof(integer));
    e_wsfe();
    return 0;
} /* make1616_ */

/* Subroutine */ int makemthr_()
{
    /* Initialized data */

    static integer a = 2111111111;
    static integer b = 1492;
    static integer c = 1776;
    static integer d = 5115;
    static doublereal da = 2111111111.;
    static doublereal db = 1492.;
    static doublereal dc = 1776.;
    static doublereal dd = 5115.;

    /* Format strings */
    static char fmt_845[] = "(a78)";
    static char fmt_602[] = "(\002  2,867,200 32-bit random integers (11,468\
,800 bytes)\002,/,\002  have been written to the file \002,a15)";

    /* System generated locals */
    doublereal d__1;
    olist o__1;

    /* Builtin functions */
    integer s_rsfe(), do_fio(), e_rsfe(), s_wsfe(), e_wsfe(), f_open(), 
	    s_wsle(), do_lio(), e_wsle(), s_rsle(), e_rsle();
    double d_int();
    integer s_wdue(), do_uio(), e_wdue();

    /* Local variables */
    static doublereal filename;
    static char text[80*23];
    static integer i, j, n[4096], v, w, x, y, z, carry, ij, jk;
    static doublereal dv, dw, dx, dy, dz;
    static char dum[80];

    /* Fortran I/O blocks */
    static cilist io___83 = { 0, 4, 0, fmt_845, 0 };
    static cilist io___86 = { 0, 4, 0, fmt_845, 0 };
    static cilist io___88 = { 0, 6, 0, fmt_845, 0 };
    static cilist io___89 = { 0, 6, 0, 0, 0 };
    static cilist io___90 = { 0, 5, 0, 0, 0 };
    static cilist io___95 = { 0, 6, 0, 0, 0 };
    static cilist io___107 = { 0, 2, 0, 0, 0 };
    static cilist io___108 = { 0, 6, 0, 0, 0 };
    static cilist io___109 = { 0, 6, 0, 0, 0 };
    static cilist io___110 = { 0, 6, 0, fmt_602, 0 };
    static cilist io___112 = { 0, 6, 0, 0, 0 };


    s_rsfe(&io___83);
    for (j = 1; j <= 59; ++j) {
	do_fio(&c__1, dum, 80L);
    }
    e_rsfe();
    s_rsfe(&io___86);
    do_fio(&c__23, text, 80L);
    e_rsfe();
    s_wsfe(&io___88);
    do_fio(&c__23, text, 80L);
    e_wsfe();
/* L745: */
    o__1.oerr = 0;
    o__1.ounit = 2;
    o__1.ofnmlen = 7;
    o__1.ofnm = "mthr.32";
    o__1.orl = 16384;
    o__1.osta = 0;
    o__1.oacc = "direct";
    o__1.ofm = "unformatted";
    o__1.oblnk = 0;
    f_open(&o__1);
    s_wsle(&io___89);
    do_lio(&c__9, &c__1, " Enter four seed integers:", 26L);
    e_wsle();
    s_rsle(&io___90);
    do_lio(&c__3, &c__1, (char *)&x, (ftnlen)sizeof(integer));
    do_lio(&c__3, &c__1, (char *)&y, (ftnlen)sizeof(integer));
    do_lio(&c__3, &c__1, (char *)&z, (ftnlen)sizeof(integer));
    do_lio(&c__3, &c__1, (char *)&w, (ftnlen)sizeof(integer));
    e_rsle();
    s_wsle(&io___95);
    do_lio(&c__9, &c__1, "       Please wait..............", 32L);
    e_wsle();
    jk = 0;
    for (ij = 1; ij <= 700; ++ij) {
	++jk;
	for (i = 1; i <= 4096; ++i) {
	    if (x < 0) {
		dx = x + 4294967296.;
	    } else {
		dx = (doublereal) x;
	    }
	    if (y < 0) {
		dy = y + 4294967296.;
	    } else {
		dy = (doublereal) y;
	    }
	    if (z < 0) {
		dz = z + 4294967296.;
	    } else {
		dz = (doublereal) z;
	    }
	    if (w < 0) {
		dw = w + 4294967296.;
	    } else {
		dw = (doublereal) w;
	    }
	    v = a * x + b * y + c * z + d * w + carry;
	    x = y;
	    y = z;
	    z = w;
	    w = v;
	    dv = da * dx + db * dy + dc * dz + dd * dw + carry;
	    d__1 = dv / 4294967296.;
	    carry = (integer) d_int(&d__1);
/* L2: */
	    n[i - 1] = w;
	}
/* L3: */
	io___107.cirec = jk;
	s_wdue(&io___107);
	do_uio(&c__4096, (char *)&n[0], (ftnlen)sizeof(integer));
	e_wdue();
    }
    s_wsle(&io___108);
    do_lio(&c__9, &c__1, "              ", 14L);
    e_wsle();
    s_wsle(&io___109);
    do_lio(&c__9, &c__1, "            FINISHED", 20L);
    e_wsle();
    s_wsfe(&io___110);
    do_fio(&c__1, (char *)&filename, (ftnlen)sizeof(doublereal));
    e_wsfe();
    s_wsle(&io___112);
    do_lio(&c__9, &c__1, "  The period is about 2^158.97", 30L);
    e_wsle();
    return 0;
} /* makemthr_ */

/* Subroutine */ int makekiss_()
{
    /* Format strings */
    static char fmt_845[] = "(a78)";

    /* System generated locals */
    olist o__1;

    /* Builtin functions */
    integer f_open(), s_rsfe(), do_fio(), e_rsfe(), s_wsfe(), e_wsfe(), 
	    s_wsle(), do_lio(), e_wsle(), s_rsle(), e_rsle(), s_wdue(), 
	    do_uio(), e_wdue();

    /* Local variables */
    static char text[80*22];
    static integer b[4096], i, j, k, m, w, x, y, z, carry, jj, jk;
    static char dum[80];

    /* Fortran I/O blocks */
    static cilist io___113 = { 0, 4, 0, fmt_845, 0 };
    static cilist io___116 = { 0, 4, 0, fmt_845, 0 };
    static cilist io___118 = { 0, 6, 0, fmt_845, 0 };
    static cilist io___119 = { 0, 6, 0, 0, 0 };
    static cilist io___120 = { 0, 5, 0, 0, 0 };
    static cilist io___126 = { 0, 6, 0, 0, 0 };
    static cilist io___133 = { 0, 2, 0, 0, 0 };
    static cilist io___134 = { 0, 6, 0, 0, 0 };
    static cilist io___135 = { 0, 6, 0, 0, 0 };
    static cilist io___136 = { 0, 6, 0, 0, 0 };


    o__1.oerr = 0;
    o__1.ounit = 2;
    o__1.ofnmlen = 7;
    o__1.ofnm = "kiss.32";
    o__1.orl = 16384;
    o__1.osta = 0;
    o__1.oacc = "direct";
    o__1.ofm = "unformatted";
    o__1.oblnk = 0;
    f_open(&o__1);
    s_rsfe(&io___113);
    for (j = 1; j <= 82; ++j) {
	do_fio(&c__1, dum, 80L);
    }
    e_rsfe();
    s_rsfe(&io___116);
    do_fio(&c__22, text, 80L);
    e_rsfe();
    s_wsfe(&io___118);
    do_fio(&c__22, text, 80L);
    e_wsfe();
    s_wsle(&io___119);
    do_lio(&c__9, &c__1, " Enter four seed integers, not zero", 35L);
    e_wsle();
    s_rsle(&io___120);
    do_lio(&c__3, &c__1, (char *)&x, (ftnlen)sizeof(integer));
    do_lio(&c__3, &c__1, (char *)&y, (ftnlen)sizeof(integer));
    do_lio(&c__3, &c__1, (char *)&z, (ftnlen)sizeof(integer));
    do_lio(&c__3, &c__1, (char *)&w, (ftnlen)sizeof(integer));
    e_rsle();
    carry = 0;
    s_wsle(&io___126);
    do_lio(&c__9, &c__1, "       Please wait..............", 32L);
    e_wsle();
    jk = 0;
    for (i = 1; i <= 700; ++i) {
	++jk;
	for (jj = 1; jj <= 4096; ++jj) {
	    x = x * 69069 + 1;
	    y ^= y << 13;
	    y ^= y >> 17;
	    y ^= y << 5;
	    k = (z >> 2) + (w >> 3) + (carry >> 2);
	    m = w + w + z + carry;
	    z = w;
	    w = m;
	    carry = k >> 30;
/* L33: */
	    b[jj - 1] = x + y + w;
	}
/* L2: */
	io___133.cirec = jk;
	s_wdue(&io___133);
	do_uio(&c__4096, (char *)&b[0], (ftnlen)sizeof(integer));
	e_wdue();
    }
    s_wsle(&io___134);
    do_lio(&c__9, &c__1, "            FINISHED", 20L);
    e_wsle();
    s_wsle(&io___135);
    do_lio(&c__9, &c__1, "  2,867,200 32-bit random integers (11,468,800 byt\
es)", 53L);
    e_wsle();
    s_wsle(&io___136);
    do_lio(&c__9, &c__1, "  have been written to the file kiss.32          ", 
	    49L);
    e_wsle();
    return 0;
} /* makekiss_ */

/* Subroutine */ int makecmbo_()
{
    /* Format strings */
    static char fmt_845[] = "(a78)";

    /* System generated locals */
    olist o__1;

    /* Builtin functions */
    integer s_rsfe(), do_fio(), e_rsfe(), s_wsfe(), e_wsfe(), s_wsle(), 
	    do_lio(), e_wsle(), s_rsle(), e_rsle(), f_open(), s_wdue(), 
	    do_uio(), e_wdue();

    /* Local variables */
    static char text[80*16];
    static integer b[4096], i, j, v, x, y, z, jk;
    static char dum[80];

    /* Fortran I/O blocks */
    static cilist io___137 = { 0, 4, 0, fmt_845, 0 };
    static cilist io___140 = { 0, 4, 0, fmt_845, 0 };
    static cilist io___142 = { 0, 6, 0, fmt_845, 0 };
    static cilist io___143 = { 0, 6, 0, 0, 0 };
    static cilist io___144 = { 0, 6, 0, 0, 0 };
    static cilist io___145 = { 0, 5, 0, 0, 0 };
    static cilist io___149 = { 0, 6, 0, 0, 0 };
    static cilist io___154 = { 0, 1, 0, 0, 0 };
    static cilist io___155 = { 0, 6, 0, 0, 0 };
    static cilist io___156 = { 0, 6, 0, 0, 0 };
    static cilist io___157 = { 0, 6, 0, 0, 0 };


/* ****Simple combo, period> 2^60.5 */
/* ***x(n)=x(n-1)*x(n-2) mod 2^32 added to */
/******period of x(n)=x(n-1)*x(n-2) is 3*2^29 if seeds odd, and one is +or
-3 mod*/
/* *****easy to ensure: replace seed x with 3*x*x. */
/* ****mwc z=30903*iand(z,65535)+ishft(z,-16) */
    s_rsfe(&io___137);
    for (j = 1; j <= 104; ++j) {
	do_fio(&c__1, dum, 80L);
    }
    e_rsfe();
    s_rsfe(&io___140);
    do_fio(&c__16, text, 80L);
    e_rsfe();
    s_wsfe(&io___142);
    do_fio(&c__16, text, 80L);
    e_wsfe();
    s_wsle(&io___143);
    do_lio(&c__9, &c__1, "              ", 14L);
    e_wsle();
    s_wsle(&io___144);
    do_lio(&c__9, &c__1, "  Enter three seed integers:", 28L);
    e_wsle();
    s_rsle(&io___145);
    do_lio(&c__3, &c__1, (char *)&x, (ftnlen)sizeof(integer));
    do_lio(&c__3, &c__1, (char *)&y, (ftnlen)sizeof(integer));
    do_lio(&c__3, &c__1, (char *)&z, (ftnlen)sizeof(integer));
    e_rsle();
    x = x + x + 1;
    x = x * 3 * x;
    y = y + y + 1;
    o__1.oerr = 0;
    o__1.ounit = 1;
    o__1.ofnmlen = 8;
    o__1.ofnm = "combo.32";
    o__1.orl = 16384;
    o__1.osta = 0;
    o__1.oacc = "direct";
    o__1.ofm = "unformatted";
    o__1.oblnk = 0;
    f_open(&o__1);
    s_wsle(&io___149);
    do_lio(&c__9, &c__1, "   Please wait..................", 32L);
    e_wsle();
    jk = 0;
    for (i = 1; i <= 700; ++i) {
	++jk;
	for (j = 1; j <= 4096; ++j) {
	    v = x * y;
	    x = y;
	    y = v;
	    z = (z & 65535) * 30903 + (z >> 16);
/* L2: */
	    b[j - 1] = y + z;
	}
/* L3: */
	io___154.cirec = jk;
	s_wdue(&io___154);
	do_uio(&c__4096, (char *)&b[0], (ftnlen)sizeof(integer));
	e_wdue();
    }
    s_wsle(&io___155);
    do_lio(&c__9, &c__1, "            FINISHED", 20L);
    e_wsle();
    s_wsle(&io___156);
    do_lio(&c__9, &c__1, "  2,867,200 32-bit random integers (11,468,800 byt\
es)", 53L);
    e_wsle();
    s_wsle(&io___157);
    do_lio(&c__9, &c__1, "  have been written to the file combo.32          ",
	     50L);
    e_wsle();
    return 0;
} /* makecmbo_ */

/* Subroutine */ int makeltra_()
{
    /* Initialized data */

    static integer r = 99;
    static integer s = 33;

    /* Format strings */
    static char fmt_845[] = "(a78)";
    static char fmt_25[] = "(5i12)";
    static char fmt_271[] = "(\002 p-value for KSTEST on uniformity of seeds\
: \002,f8.6)";

    /* System generated locals */
    integer i__1;
    olist o__1;

    /* Builtin functions */
    integer s_rsfe(), do_fio(), e_rsfe(), s_wsfe(), e_wsfe(), f_open(), 
	    s_wsle(), do_lio(), e_wsle(), s_rsle(), e_rsle(), s_wdue(), 
	    do_uio(), e_wdue();

    /* Local variables */
    static integer juni, mwcs;
    static char text[80*15];
    static integer i, j, k, l, bb[4096], ig, ii, jk, ip, jp, js, ju[99];
    static real px, xx[607];
    extern /* Subroutine */ int kstest_();
    static char dum[80];

    /* Fortran I/O blocks */
    static cilist io___160 = { 0, 4, 0, fmt_845, 0 };
    static cilist io___163 = { 0, 4, 0, fmt_845, 0 };
    static cilist io___165 = { 0, 6, 0, fmt_845, 0 };
    static cilist io___166 = { 0, 6, 0, 0, 0 };
    static cilist io___167 = { 0, 5, 0, 0, 0 };
    static cilist io___176 = { 0, 6, 0, 0, 0 };
    static cilist io___177 = { 0, 6, 0, fmt_25, 0 };
    static cilist io___180 = { 0, 6, 0, fmt_271, 0 };
    static cilist io___181 = { 0, 6, 0, 0, 0 };
    static cilist io___187 = { 0, 1, 0, 0, 0 };
    static cilist io___188 = { 0, 6, 0, 0, 0 };
    static cilist io___189 = { 0, 6, 0, 0, 0 };
    static cilist io___190 = { 0, 6, 0, 0, 0 };
    static cilist io___191 = { 0, 6, 0, 0, 0 };
    static cilist io___192 = { 0, 6, 0, 0, 0 };


    s_rsfe(&io___160);
    for (j = 1; j <= 120; ++j) {
	do_fio(&c__1, dum, 80L);
    }
    e_rsfe();
    s_rsfe(&io___163);
    do_fio(&c__15, text, 80L);
    e_rsfe();
    s_wsfe(&io___165);
    do_fio(&c__15, text, 80L);
    e_wsfe();
    o__1.oerr = 0;
    o__1.ounit = 1;
    o__1.ofnmlen = 8;
    o__1.ofnm = "ultra.32";
    o__1.orl = 16384;
    o__1.osta = 0;
    o__1.oacc = "direct";
    o__1.ofm = "unformatted";
    o__1.oblnk = 0;
    f_open(&o__1);
    s_wsle(&io___166);
    do_lio(&c__9, &c__1, "   Enter four positive integers for seeds:", 42L);
    e_wsle();
    s_rsle(&io___167);
    do_lio(&c__3, &c__1, (char *)&i, (ftnlen)sizeof(integer));
    do_lio(&c__3, &c__1, (char *)&j, (ftnlen)sizeof(integer));
    do_lio(&c__3, &c__1, (char *)&k, (ftnlen)sizeof(integer));
    do_lio(&c__3, &c__1, (char *)&l, (ftnlen)sizeof(integer));
    e_rsle();
    mwcs = i + j + k + l;
    i__1 = r;
    for (ii = 1; ii <= i__1; ++ii) {
	i = (i & 65535) * 18273 + (i >> 16);
	j = (j & 65535) * 23163 + (j >> 16);
	k = (k & 65535) * 24984 + (k >> 16);
	l = (l & 65535) * 28854 + (l >> 16);
	js = (i << 16) + (j & 65535) + (k << 16) + (l & 65535);
	js |= 1;
	xx[ii - 1] = js * (float)2.3283064365386963e-10 + (float).5;
/* L92: */
	ju[ii - 1] = js;
    }
    s_wsle(&io___176);
    do_lio(&c__9, &c__1, "    The seed table:", 19L);
    e_wsle();
    s_wsfe(&io___177);
    i__1 = r;
    for (ig = 1; ig <= i__1; ++ig) {
	do_fio(&c__1, (char *)&ju[ig - 1], (ftnlen)sizeof(integer));
    }
    e_wsfe();
    kstest_(xx, &r, &px);
    s_wsfe(&io___180);
    do_fio(&c__1, (char *)&px, (ftnlen)sizeof(real));
    e_wsfe();
    s_wsle(&io___181);
    do_lio(&c__9, &c__1, "    Please wait................", 31L);
    e_wsle();
    ip = r;
    jp = s;
    jk = 0;
    for (i = 1; i <= 700; ++i) {
	++jk;
	for (j = 1; j <= 4096; ++j) {
	    juni = ju[ip - 1] * ju[jp - 1];
	    ju[ip - 1] = juni;
	    --ip;
	    if (ip == 0) {
		ip = 97;
	    }
	    --jp;
	    if (jp == 0) {
		jp = 97;
	    }
	    mwcs = (mwcs & 65535) * 30903 + (mwcs >> 16);
/* L3: */
	    bb[j - 1] = juni + mwcs;
	}
/* L2: */
	io___187.cirec = jk;
	s_wdue(&io___187);
	do_uio(&c__4096, (char *)&bb[0], (ftnlen)sizeof(integer));
	e_wdue();
    }
    s_wsle(&io___188);
    do_lio(&c__9, &c__1, "     ", 5L);
    e_wsle();
    s_wsle(&io___189);
    do_lio(&c__9, &c__1, "     FINISHED", 13L);
    e_wsle();
    s_wsle(&io___190);
    do_lio(&c__9, &c__1, " 2,867,200  32-bit random integers (11,468,800 byt\
es)", 53L);
    e_wsle();
    s_wsle(&io___191);
    do_lio(&c__9, &c__1, "  have been written to the file ultra.32  ", 42L);
    e_wsle();
    s_wsle(&io___192);
    do_lio(&c__9, &c__1, "::::::::::::::::::::::::::::::::::::::::::::::", 
	    46L);
    e_wsle();
    return 0;
} /* makeltra_ */

/* Subroutine */ int makesbmc_()
{
    /* Initialized data */

    static integer r = 37;
    static integer s = 24;

    /* Format strings */
    static char fmt_845[] = "(a78)";
    static char fmt_25[] = "(5i12)";
    static char fmt_271[] = "(\002 p-value for KSTEST on uniformity of seeds\
: \002,f8.6)";
    static char fmt_349[] = "(\002     The binary file swbmwc.32\002/,\002  \
   has been created with 32-bit integers\002,/,\002     from the subtract-wi\
th-borrow sequence\002,/,\002    x(n)=x(n-24)-x(n-37)-borrow mod 2^32\002,/\
,\002   combined with the multiply-with-carry sequence\002,/,\002   y(n)=309\
03*y(n-1)+carry mod 2^16,\002,/,\002  the overall sequence having period abo\
ut 10^364.\002)";

    /* System generated locals */
    integer i__1;
    olist o__1;

    /* Builtin functions */
    integer s_rsfe(), do_fio(), e_rsfe(), s_wsfe(), e_wsfe(), s_wsle(), 
	    do_lio(), e_wsle(), s_rsle(), e_rsle(), f_open(), s_wdue(), 
	    do_uio(), e_wdue();

    /* Local variables */
    static integer juni, mwcs;
    static char text[80*9];
    static integer i, j, k, l, m, x, y, bb[4096], ig, ii, jj, jk, ip, jp, js, 
	    ju[37];
    static real px, xx[37];
    extern /* Subroutine */ int kstest_();
    static char dum[80];

    /* Fortran I/O blocks */
    static cilist io___195 = { 0, 4, 0, fmt_845, 0 };
    static cilist io___198 = { 0, 4, 0, fmt_845, 0 };
    static cilist io___200 = { 0, 6, 0, fmt_845, 0 };
    static cilist io___201 = { 0, 6, 0, 0, 0 };
    static cilist io___202 = { 0, 5, 0, 0, 0 };
    static cilist io___206 = { 0, 6, 0, 0, 0 };
    static cilist io___214 = { 0, 6, 0, 0, 0 };
    static cilist io___215 = { 0, 6, 0, fmt_25, 0 };
    static cilist io___218 = { 0, 6, 0, fmt_271, 0 };
    static cilist io___226 = { 0, 1, 0, 0, 0 };
    static cilist io___227 = { 0, 6, 0, 0, 0 };
    static cilist io___228 = { 0, 6, 0, 0, 0 };
    static cilist io___229 = { 0, 6, 0, fmt_349, 0 };


    s_rsfe(&io___195);
    for (j = 1; j <= 135; ++j) {
	do_fio(&c__1, dum, 80L);
    }
    e_rsfe();
    s_rsfe(&io___198);
    do_fio(&c__9, text, 80L);
    e_rsfe();
    s_wsfe(&io___200);
    do_fio(&c__9, text, 80L);
    e_wsfe();
    s_wsle(&io___201);
    do_lio(&c__9, &c__1, "    Enter four positive integers for seeds:", 43L);
    e_wsle();
    s_rsle(&io___202);
    do_lio(&c__3, &c__1, (char *)&i, (ftnlen)sizeof(integer));
    do_lio(&c__3, &c__1, (char *)&j, (ftnlen)sizeof(integer));
    do_lio(&c__3, &c__1, (char *)&k, (ftnlen)sizeof(integer));
    do_lio(&c__3, &c__1, (char *)&l, (ftnlen)sizeof(integer));
    e_rsle();
    o__1.oerr = 0;
    o__1.ounit = 1;
    o__1.ofnmlen = 9;
    o__1.ofnm = "swbmwc.32";
    o__1.orl = 16384;
    o__1.osta = 0;
    o__1.oacc = "direct";
    o__1.ofm = "unformatted";
    o__1.oblnk = 0;
    f_open(&o__1);
    s_wsle(&io___206);
    do_lio(&c__9, &c__1, "    Please wait................", 31L);
    e_wsle();
    mwcs = i + j + k + l;
    i__1 = r;
    for (ii = 1; ii <= i__1; ++ii) {
	js = (float)0.;
	for (jj = 1; jj <= 32; ++jj) {
	    m = i * j % 179 * k % 179;
	    i = j;
	    j = k;
	    k = m;
	    l = (l * 53 + 1) % 169;
	    js <<= 1;
	    if (l * m % 64 >= 32) {
		++js;
	    }
/* L93: */
	}
	xx[ii - 1] = js * (float)2.3283064365386963e-10 + (float).5;
/* L92: */
	ju[ii - 1] = js;
    }
    s_wsle(&io___214);
    do_lio(&c__9, &c__1, "    The seed table:", 19L);
    e_wsle();
    s_wsfe(&io___215);
    i__1 = r;
    for (ig = 1; ig <= i__1; ++ig) {
	do_fio(&c__1, (char *)&ju[ig - 1], (ftnlen)sizeof(integer));
    }
    e_wsfe();
    kstest_(xx, &r, &px);
    s_wsfe(&io___218);
    do_fio(&c__1, (char *)&px, (ftnlen)sizeof(real));
    e_wsfe();
    ip = r;
    jp = s;
    jk = 0;
    for (i = 1; i <= 700; ++i) {
	++jk;
	for (j = 1; j <= 4096; ++j) {
	    x = ju[ip - 1];
	    y = ju[jp - 1];
	    juni = x - y;
	    if (x >> 1 > y >> 1) {
		--juni;
	    }
	    ju[ip - 1] = juni;
	    --ip;
	    if (ip == 0) {
		ip = r;
	    }
	    --jp;
	    if (jp == 0) {
		jp = r;
	    }
	    mwcs = (mwcs & 65535) * 30903 + (mwcs >> 16);
/* L3: */
	    bb[j - 1] = mwcs + juni;
	}
/* L2: */
	io___226.cirec = jk;
	s_wdue(&io___226);
	do_uio(&c__4096, (char *)&bb[0], (ftnlen)sizeof(integer));
	e_wdue();
    }
    s_wsle(&io___227);
    do_lio(&c__9, &c__1, "     ", 5L);
    e_wsle();
    s_wsle(&io___228);
    do_lio(&c__9, &c__1, "     FINISHED", 13L);
    e_wsle();
    s_wsfe(&io___229);
    e_wsfe();
    return 0;
} /* makesbmc_ */

/* Subroutine */ int makexcng_()
{
    /* Format strings */
    static char fmt_845[] = "(a78)";

    /* System generated locals */
    doublereal d__1;
    olist o__1;

    /* Builtin functions */
    integer f_open(), s_rsfe(), do_fio(), e_rsfe(), s_wsfe(), e_wsfe(), 
	    s_wsle(), do_lio(), e_wsle(), s_rsle(), e_rsle();
    double d_mod();
    integer s_wdue(), do_uio(), e_wdue();

    /* Local variables */
    static char text[80*17];
    static doublereal a, b, c;
    static integer i, j, n[4096];
    static doublereal x;
    static real r3, r4;
    static integer ij, jk, jch;
    static char dum[80];

    /* Fortran I/O blocks */
    static cilist io___230 = { 0, 4, 0, fmt_845, 0 };
    static cilist io___233 = { 0, 4, 0, fmt_845, 0 };
    static cilist io___235 = { 0, 6, 0, fmt_845, 0 };
    static cilist io___236 = { 0, 6, 0, 0, 0 };
    static cilist io___237 = { 0, 5, 0, 0, 0 };
    static cilist io___241 = { 0, 6, 0, 0, 0 };
    static cilist io___250 = { 0, 2, 0, 0, 0 };
    static cilist io___251 = { 0, 6, 0, 0, 0 };
    static cilist io___252 = { 0, 6, 0, 0, 0 };
    static cilist io___253 = { 0, 6, 0, 0, 0 };
    static cilist io___254 = { 0, 6, 0, 0, 0 };


    o__1.oerr = 0;
    o__1.ounit = 2;
    o__1.ofnmlen = 9;
    o__1.ofnm = "excong.32";
    o__1.orl = 16384;
    o__1.osta = 0;
    o__1.oacc = "direct";
    o__1.ofm = "unformatted";
    o__1.oblnk = 0;
    f_open(&o__1);
    s_rsfe(&io___230);
    for (j = 1; j <= 144; ++j) {
	do_fio(&c__1, dum, 80L);
    }
    e_rsfe();
    s_rsfe(&io___233);
    do_fio(&c__17, text, 80L);
    e_rsfe();
    s_wsfe(&io___235);
    do_fio(&c__17, text, 80L);
    e_wsfe();
    s_wsle(&io___236);
    do_lio(&c__9, &c__1, " Enter three seed integers, not all zero:", 41L);
    e_wsle();
    s_rsle(&io___237);
    do_lio(&c__5, &c__1, (char *)&a, (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&b, (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&c, (ftnlen)sizeof(doublereal));
    e_rsle();
    s_wsle(&io___241);
    do_lio(&c__9, &c__1, "       Please wait..............", 32L);
    e_wsle();
    r3 = (float).12500000308864409;
    r4 = (float).12500000592626775;
    jk = 0;
    for (ij = 1; ij <= 700; ++ij) {
	++jk;
	for (i = 1; i <= 4096; ++i) {
	    switch ((int)jch) {
		case 1:  goto L51;
		case 2:  goto L52;
		case 3:  goto L53;
		case 4:  goto L54;
	    }
L51:
	    d__1 = a * 1776. + b * 1476. + c * 1176.;
	    x = d_mod(&d__1, &c_b389);
	    goto L166;
L52:
	    d__1 = (a + b + c) * 8192;
	    x = d_mod(&d__1, &c_b389);
	    goto L166;
L53:
	    d__1 = a * 2001. + b * 1998. + c * 1995.;
	    x = d_mod(&d__1, &c_b392);
	    x *= r3;
	    goto L166;
L54:
	    d__1 = (a + b + c) * 524288.;
	    x = d_mod(&d__1, &c_b393);
	    x *= r4;
L166:
	    a = b;
	    b = c;
	    c = x;
	    if (x < (float)2147483648.) {
		n[i - 1] = (integer) x;
	    } else {
		n[i - 1] = (integer) (x - 4294967296.);
	    }
/* L2: */
	}
/* L3: */
	io___250.cirec = jk;
	s_wdue(&io___250);
	do_uio(&c__4096, (char *)&n[0], (ftnlen)sizeof(integer));
	e_wdue();
    }
    s_wsle(&io___251);
    do_lio(&c__9, &c__1, "              ", 14L);
    e_wsle();
    s_wsle(&io___252);
    do_lio(&c__9, &c__1, "            FINISHED", 20L);
    e_wsle();
    s_wsle(&io___253);
    do_lio(&c__9, &c__1, "  2,867,200 32-bit random integers (11,468,800 byt\
es)", 53L);
    e_wsle();
    s_wsle(&io___254);
    do_lio(&c__9, &c__1, "  have been written to the file excong.32          "
	    , 51L);
    e_wsle();
    return 0;
} /* makexcng_ */

/* Subroutine */ int makesupr_()
{
    /* Format strings */
    static char fmt_845[] = "(a78)";
    static char fmt_821[] = "(a1)";

    /* System generated locals */
    olist o__1;

    /* Builtin functions */
    integer f_open(), s_rsfe(), do_fio(), e_rsfe(), s_wsfe(), e_wsfe(), 
	    s_wsle(), do_lio(), e_wsle(), s_rsle(), e_rsle(), s_wdue(), 
	    do_uio(), e_wdue();

    /* Local variables */
    static char text[80*20];
    static integer i, j, n[4096], x, y, jk;
    static char op[1], dum[80];

    /* Fortran I/O blocks */
    static cilist io___255 = { 0, 4, 0, fmt_845, 0 };
    static cilist io___258 = { 0, 4, 0, fmt_845, 0 };
    static cilist io___260 = { 0, 6, 0, fmt_845, 0 };
    static cilist io___261 = { 0, 6, 0, 0, 0 };
    static cilist io___262 = { 0, 5, 0, 0, 0 };
    static cilist io___265 = { 0, 6, 0, 0, 0 };
    static cilist io___266 = { 0, 6, 0, 0, 0 };
    static cilist io___267 = { 0, 5, 0, fmt_821, 0 };
    static cilist io___269 = { 0, 6, 0, 0, 0 };
    static cilist io___273 = { 0, 2, 0, 0, 0 };
    static cilist io___274 = { 0, 2, 0, 0, 0 };
    static cilist io___275 = { 0, 6, 0, 0, 0 };
    static cilist io___276 = { 0, 6, 0, 0, 0 };
    static cilist io___277 = { 0, 6, 0, 0, 0 };


    o__1.oerr = 0;
    o__1.ounit = 2;
    o__1.ofnmlen = 11;
    o__1.ofnm = "suprdupr.32";
    o__1.orl = 16384;
    o__1.osta = 0;
    o__1.oacc = "direct";
    o__1.ofm = "unformatted";
    o__1.oblnk = 0;
    f_open(&o__1);
    s_rsfe(&io___255);
    for (j = 1; j <= 161; ++j) {
	do_fio(&c__1, dum, 80L);
    }
    e_rsfe();
    s_rsfe(&io___258);
    do_fio(&c__20, text, 80L);
    e_rsfe();
    s_wsfe(&io___260);
    do_fio(&c__20, text, 80L);
    e_wsfe();
    s_wsle(&io___261);
    do_lio(&c__9, &c__1, " Enter two  seed integers, the second not zero:", 
	    47L);
    e_wsle();
    s_rsle(&io___262);
    do_lio(&c__3, &c__1, (char *)&x, (ftnlen)sizeof(integer));
    do_lio(&c__3, &c__1, (char *)&y, (ftnlen)sizeof(integer));
    e_rsle();
    s_wsle(&io___265);
    do_lio(&c__9, &c__1, "  Choose your method of combination,", 36L);
    e_wsle();
    s_wsle(&io___266);
    do_lio(&c__9, &c__1, "  + for addition, x for exclusive-or, in column 1:",
	     50L);
    e_wsle();
    s_rsfe(&io___267);
    do_fio(&c__1, op, 1L);
    e_rsfe();
    s_wsle(&io___269);
    do_lio(&c__9, &c__1, "       Please wait..............", 32L);
    e_wsle();
    jk = 0;
    if (*(unsigned char *)op == '+') {
	for (i = 1; i <= 700; ++i) {
	    ++jk;
	    for (j = 1; j <= 4096; ++j) {
		x = x * 69069 + 1;
		y ^= y << 13;
		y ^= y >> 17;
		y ^= y << 5;
/* L3: */
		n[j - 1] = x + y;
	    }
/* L2: */
	    io___273.cirec = jk;
	    s_wdue(&io___273);
	    do_uio(&c__4096, (char *)&n[0], (ftnlen)sizeof(integer));
	    e_wdue();
	}
    }
    if (*(unsigned char *)op == 'x') {
	for (i = 1; i <= 700; ++i) {
	    ++jk;
	    for (j = 1; j <= 4096; ++j) {
		x = x * 69069 + 1;
		y ^= y << 13;
		y ^= y >> 17;
		y ^= y << 5;
/* L73: */
		n[j - 1] = x ^ y;
	    }
/* L72: */
	    io___274.cirec = jk;
	    s_wdue(&io___274);
	    do_uio(&c__4096, (char *)&n[0], (ftnlen)sizeof(integer));
	    e_wdue();
	}
    }
    s_wsle(&io___275);
    do_lio(&c__9, &c__1, "            FINISHED", 20L);
    e_wsle();
    s_wsle(&io___276);
    do_lio(&c__9, &c__1, "  2,867,200 32-bit random integers (11,468,800 byt\
es)", 53L);
    e_wsle();
    s_wsle(&io___277);
    do_lio(&c__9, &c__1, "  have been written to the file suprdupr.32  ", 45L)
	    ;
    e_wsle();
    return 0;
} /* makesupr_ */

/* Subroutine */ int makeswb_()
{
    /* Initialized data */

    static integer rc[5] = { 43,37,24,21,48 };
    static integer sc[5] = { 22,24,19,6,8 };
    static integer jix[5] = { -6,0,0,0,2147483647 };
    static char swb[42*5+1] = "   1:   x(n)=x(n-43)-x(n-22)-c mod 2^32-5    \
2:   x(n)=x(n-37)-x(n-24)-c mod 2^32      3:   x(n)=x(n-24)-x(n-19)-c mod 2^\
32      4:   x(n)=x(n-21)-x(n- 6)-c mod 2^32      5:   x(n)=x(n-48)-x(n- 8)-\
c mod 2^31   ";
    static char period[42*5+1] = "  (2^32-5)^43 - (2^32-5)^22, about 10^414 \
  2^1178 - 2^762, about 10^354              (2^759 - 2^599)/3, about 10^228 \
          (2^666 - 2^186)/3, about 10^200           (2^1478 - 2^247)/105, ab\
out 10^445      ";

    /* Format strings */
    static char fmt_845[] = "(a78)";
    static char fmt_945[] = "(10x,a42)";
    static char fmt_645[] = "(a15)";
    static char fmt_348[] = "(\002    Enter four positive integers for see\
ds:\002)";
    static char fmt_349[] = "(\002     The binary file \002,a15,/,\002     h\
as been created with 32-bit integers\002,/,\002     from the subtract-with-b\
orrow sequence\002,/,a41)";
    static char fmt_350[] = "(15x,\002The period is\002,/,a41)";

    /* System generated locals */
    integer i__1;
    olist o__1;

    /* Builtin functions */
    integer s_rsfe(), do_fio(), e_rsfe(), s_wsfe(), e_wsfe(), s_wsle(), 
	    do_lio(), e_wsle(), s_rsle(), e_rsle(), f_open(), s_wdue(), 
	    do_uio(), e_wdue();

    /* Local variables */
    static char filename[15];
    static integer juni;
    static char text[80*17];
    static integer i, j, k, l, m, r, s, x, y, bb[4096], ii, jj, jk, ip, jp, 
	    js, ju[48], ijk, jjn, jchoice;
    static char dum[80];

    /* Fortran I/O blocks */
    static cilist io___281 = { 0, 4, 0, fmt_845, 0 };
    static cilist io___284 = { 0, 4, 0, fmt_845, 0 };
    static cilist io___286 = { 0, 6, 0, fmt_845, 0 };
    static cilist io___287 = { 0, 6, 0, 0, 0 };
    static cilist io___288 = { 0, 6, 0, fmt_945, 0 };
    static cilist io___290 = { 0, 6, 0, 0, 0 };
    static cilist io___291 = { 0, 6, 0, 0, 0 };
    static cilist io___292 = { 0, 5, 0, 0, 0 };
    static cilist io___296 = { 0, 6, 0, 0, 0 };
    static cilist io___297 = { 0, 5, 0, fmt_645, 0 };
    static cilist io___299 = { 0, 6, 0, fmt_348, 0 };
    static cilist io___300 = { 0, 5, 0, 0, 0 };
    static cilist io___304 = { 0, 6, 0, 0, 0 };
    static cilist io___319 = { 0, 1, 0, 0, 0 };
    static cilist io___320 = { 0, 6, 0, 0, 0 };
    static cilist io___321 = { 0, 6, 0, 0, 0 };
    static cilist io___322 = { 0, 6, 0, fmt_349, 0 };
    static cilist io___323 = { 0, 6, 0, fmt_350, 0 };
    static cilist io___325 = { 0, 6, 0, 0, 0 };


    s_rsfe(&io___281);
    for (j = 1; j <= 181; ++j) {
	do_fio(&c__1, dum, 80L);
    }
    e_rsfe();
    s_rsfe(&io___284);
    do_fio(&c__17, text, 80L);
    e_rsfe();
    s_wsfe(&io___286);
    do_fio(&c__17, text, 80L);
    e_wsfe();
    s_wsle(&io___287);
    do_lio(&c__9, &c__1, "    Choose your generator from this list:", 41L);
    e_wsle();
    s_wsfe(&io___288);
    do_fio(&c__5, swb, 42L);
    e_wsfe();
    s_wsle(&io___290);
    do_lio(&c__9, &c__1, "(Choice 5 provides 31-bit integers that are left-j\
ustified.)", 60L);
    e_wsle();
    s_wsle(&io___291);
    do_lio(&c__9, &c__1, " enter 1,2,3,4 or 5:", 20L);
    e_wsle();
    s_rsle(&io___292);
    do_lio(&c__3, &c__1, (char *)&jchoice, (ftnlen)sizeof(integer));
    e_rsle();
    r = rc[jchoice - 1];
    s = sc[jchoice - 1];
    s_wsle(&io___296);
    do_lio(&c__9, &c__1, "     Enter output file name (<=15 characters):", 
	    46L);
    e_wsle();
    s_rsfe(&io___297);
    do_fio(&c__1, filename, 15L);
    e_rsfe();
    o__1.oerr = 0;
    o__1.ounit = 1;
    o__1.ofnmlen = 15;
    o__1.ofnm = filename;
    o__1.orl = 16384;
    o__1.osta = 0;
    o__1.oacc = "direct";
    o__1.ofm = "unformatted";
    o__1.oblnk = 0;
    f_open(&o__1);
    s_wsfe(&io___299);
    e_wsfe();
    s_rsle(&io___300);
    do_lio(&c__3, &c__1, (char *)&i, (ftnlen)sizeof(integer));
    do_lio(&c__3, &c__1, (char *)&j, (ftnlen)sizeof(integer));
    do_lio(&c__3, &c__1, (char *)&k, (ftnlen)sizeof(integer));
    do_lio(&c__3, &c__1, (char *)&l, (ftnlen)sizeof(integer));
    e_rsle();
    s_wsle(&io___304);
    do_lio(&c__9, &c__1, "    Please wait................", 31L);
    e_wsle();
    i__1 = i;
    for (ijk = 1; ijk <= i__1; ++ijk) {
/* L90: */
	jjn = 1;
    }
/* 90            jjn=irand(1) */
    i__1 = r;
    for (ii = 1; ii <= i__1; ++ii) {
	js = (float)0.;
	for (jj = 1; jj <= 32; ++jj) {
	    m = i * j % 179 * k % 179;
	    i = j;
	    j = k;
	    k = m;
	    l = (l * 53 + 1) % 169;
	    js <<= 1;
	    if (l * m % 64 >= 32) {
		++js;
	    }
/* L93: */
	}
	if (jchoice == 5) {
	    js ^= 1;
	}
/* L92: */
	ju[ii - 1] = js;
    }
    ip = r;
    jp = s;
    jk = 0;
    for (i = 1; i <= 700; ++i) {
	++jk;
	for (j = 1; j <= 4096; ++j) {
	    x = ju[ip - 1];
	    y = ju[jp - 1];
	    juni = x - y;
	    if (x >> 1 < y >> 1) {
		juni += jix[jchoice - 1];
	    }
	    ju[ip - 1] = juni;
	    --ip;
	    if (ip == 0) {
		ip = r;
	    }
	    --jp;
	    if (jp == 0) {
		jp = r;
	    }
	    if (jchoice == 5) {
		juni += juni;
	    }
/* L3: */
	    bb[j - 1] = juni;
	}
/* L2: */
	io___319.cirec = jk;
	s_wdue(&io___319);
	do_uio(&c__4096, (char *)&bb[0], (ftnlen)sizeof(integer));
	e_wdue();
    }
    s_wsle(&io___320);
    do_lio(&c__9, &c__1, "     ", 5L);
    e_wsle();
    s_wsle(&io___321);
    do_lio(&c__9, &c__1, "     FINISHED", 13L);
    e_wsle();
    s_wsfe(&io___322);
    do_fio(&c__1, filename, 15L);
    do_fio(&c__1, swb + (jchoice - 1) * 42, 42L);
    e_wsfe();
    s_wsfe(&io___323);
    do_fio(&c__1, period + (jchoice - 1) * 42, 42L);
    e_wsfe();
    s_wsle(&io___325);
    do_lio(&c__9, &c__1, "++++++++++++++++++++++++++++++++++++++++++++++++++\
+++++", 55L);
    e_wsle();
    return 0;
} /* makeswb_ */

/* Subroutine */ int makecong_()
{
    /* Format strings */
    static char fmt_845[] = "(a78)";
    static char fmt_645[] = "(a15)";
    static char fmt_221[] = "(\002  2,867,200 32-bit random integers (11,468\
,800 bytes)\002,/,\002  have been written to the file \002,a15)";
    static char fmt_983[] = "(a1)";

    /* System generated locals */
    integer i__1, i__2;
    doublereal d__1, d__2;
    olist o__1;

    /* Builtin functions */
    integer s_rsfe(), do_fio(), e_rsfe(), s_wsfe(), e_wsfe(), s_wsle(), 
	    do_lio(), e_wsle(), s_rsle(), e_rsle();
    double pow_di();
    integer f_open();
    double d_mod();
    integer s_wdue(), do_uio(), e_wdue();

    /* Local variables */
    static integer jlat, klim;
    static char filename[15], text[80*13];
    extern /* Subroutine */ int plot1_();
    static integer a, b, i, j, k, r, s;
    static real x[2000], y[2000];
    static integer jseed, j1, j2, kount, bb[4096];
    static doublereal da, db, dj, dl, dm;
    static integer jk;
    static doublereal dr, dx;
    static char op[1];
    static doublereal yy, dum;

    /* Fortran I/O blocks */
    static cilist io___326 = { 0, 4, 0, fmt_845, 0 };
    static cilist io___329 = { 0, 4, 0, fmt_845, 0 };
    static cilist io___331 = { 0, 6, 0, fmt_845, 0 };
    static cilist io___332 = { 0, 6, 0, 0, 0 };
    static cilist io___333 = { 0, 6, 0, 0, 0 };
    static cilist io___334 = { 0, 6, 0, 0, 0 };
    static cilist io___335 = { 0, 5, 0, 0, 0 };
    static cilist io___340 = { 0, 6, 0, 0, 0 };
    static cilist io___341 = { 0, 5, 0, 0, 0 };
    static cilist io___346 = { 0, 6, 0, 0, 0 };
    static cilist io___347 = { 0, 6, 0, 0, 0 };
    static cilist io___348 = { 0, 6, 0, 0, 0 };
    static cilist io___349 = { 0, 5, 0, 0, 0 };
    static cilist io___351 = { 0, 6, 0, 0, 0 };
    static cilist io___352 = { 0, 5, 0, fmt_645, 0 };
    static cilist io___354 = { 0, 6, 0, 0, 0 };
    static cilist io___361 = { 0, 1, 0, 0, 0 };
    static cilist io___362 = { 0, 6, 0, fmt_221, 0 };
    static cilist io___363 = { 0, 6, 0, 0, 0 };
    static cilist io___364 = { 0, 6, 0, 0, 0 };
    static cilist io___365 = { 0, 5, 0, fmt_983, 0 };


    s_rsfe(&io___326);
    for (j = 1; j <= 198; ++j) {
	do_fio(&c__1, (char *)&dum, (ftnlen)sizeof(doublereal));
    }
    e_rsfe();
    s_rsfe(&io___329);
    do_fio(&c__13, text, 80L);
    e_rsfe();
    s_wsfe(&io___331);
    do_fio(&c__13, text, 80L);
    e_wsfe();
    s_wsle(&io___332);
    do_lio(&c__9, &c__1, " Enter a,b,r and s for the generator", 36L);
    e_wsle();
    s_wsle(&io___333);
    do_lio(&c__9, &c__1, "  x(n)=a*x(n-1)+b mod 2^r+s", 27L);
    e_wsle();
    s_wsle(&io___334);
    do_lio(&c__9, &c__1, " To avoid overflow, make sure a*2^r < 2^53) ", 44L);
    e_wsle();
    s_rsle(&io___335);
    do_lio(&c__3, &c__1, (char *)&a, (ftnlen)sizeof(integer));
    do_lio(&c__3, &c__1, (char *)&b, (ftnlen)sizeof(integer));
    do_lio(&c__3, &c__1, (char *)&r, (ftnlen)sizeof(integer));
    do_lio(&c__3, &c__1, (char *)&s, (ftnlen)sizeof(integer));
    e_rsle();
    s_wsle(&io___340);
    do_lio(&c__9, &c__1, " Enter a seed integer:", 22L);
    e_wsle();
    s_rsle(&io___341);
    do_lio(&c__3, &c__1, (char *)&jseed, (ftnlen)sizeof(integer));
    e_rsle();
    da = (doublereal) a;
    db = (doublereal) b;
    dm = pow_di(&c_b546, &r) + s;
    s_wsle(&io___346);
    do_lio(&c__9, &c__1, " If you want the 2-lattice without generating", 45L)
	    ;
    e_wsle();
    s_wsle(&io___347);
    do_lio(&c__9, &c__1, " the binary file, enter 0, else enter 1:", 40L);
    e_wsle();
    s_wsle(&io___348);
    do_lio(&c__9, &c__1, "   Create binary file? 0 for NO, 1 for YES:", 43L);
    e_wsle();
    s_rsle(&io___349);
    do_lio(&c__3, &c__1, (char *)&jlat, (ftnlen)sizeof(integer));
    e_rsle();
    if (jlat == 1) {
	s_wsle(&io___351);
	do_lio(&c__9, &c__1, "     Enter output file name (<=15 characters):",
		 46L);
	e_wsle();
	s_rsfe(&io___352);
	do_fio(&c__1, filename, 15L);
	e_rsfe();
	o__1.oerr = 0;
	o__1.ounit = 1;
	o__1.ofnmlen = 15;
	o__1.ofnm = filename;
	o__1.orl = 16384;
	o__1.osta = 0;
	o__1.oacc = "direct";
	o__1.ofm = "unformatted";
	o__1.oblnk = 0;
	f_open(&o__1);
	s_wsle(&io___354);
	do_lio(&c__9, &c__1, "   Please wait................", 30L);
	e_wsle();
	dj = (doublereal) jseed;
	i__1 = 32 - r;
	dr = pow_di(&c_b546, &i__1);
	jk = 0;
	for (i = 1; i <= 700; ++i) {
	    ++jk;
	    for (j = 1; j <= 4096; ++j) {
		d__1 = da * dj + db;
		dj = d_mod(&d__1, &dm);
		dx = dr * dj;
		if (dx < 2147483648.) {
		    bb[j - 1] = (integer) dx;
		} else {
		    bb[j - 1] = (integer) (dx - 4294967296.);
		}
/* L3: */
	    }
/* L2: */
	    io___361.cirec = jk;
	    s_wdue(&io___361);
	    do_uio(&c__4096, (char *)&bb[0], (ftnlen)sizeof(integer));
	    e_wdue();
	}
    }
    s_wsfe(&io___362);
    do_fio(&c__1, filename, 15L);
    e_wsfe();
    s_wsle(&io___363);
    do_lio(&c__9, &c__1, " To display the 2-lattice of this generator, ", 45L)
	    ;
    e_wsle();
    s_wsle(&io___364);
    do_lio(&c__9, &c__1, "  hit any letter or number key:", 31L);
    e_wsle();
    s_rsfe(&io___365);
    do_fio(&c__1, op, 1L);
    e_rsfe();
    i__1 = r - 12;
    dl = pow_di(&c_b546, &i__1);
    kount = 0;
    klim = (integer) (da * dl / dm);
    i__1 = klim;
    for (k = 1; k <= i__1; ++k) {
/* Computing MAX */
	d__1 = 0., d__2 = (k * dm - b) / da;
	j1 = (integer) max(d__1,d__2);
	j2 = (integer) ((k * dm - b + dl) / da);
	i__2 = j2;
	for (j = j1; j <= i__2; ++j) {
	    d__1 = da * j + db;
	    yy = d_mod(&d__1, &dm);
	    if (yy < dl) {
		++kount;
		x[kount - 1] = j / dm;
		y[kount - 1] = yy / dm;
	    }
/* L39: */
	}
/* L29: */
    }
    plot1_(x, y, &kount, &c__54);
    return 0;
} /* makecong_ */

/* Subroutine */ int makeran2_()
{
    /* Format strings */
    static char fmt_845[] = "(a78)";

    /* System generated locals */
    olist o__1;

    /* Builtin functions */
    integer f_open(), s_rsfe(), do_fio(), e_rsfe(), s_wsfe(), e_wsfe(), 
	    s_wsle(), do_lio(), e_wsle(), s_rsle(), e_rsle(), s_wdue(), 
	    do_uio(), e_wdue();

    /* Local variables */
    static integer idum;
    static char text[80*16];
    extern integer iran2_();
    static integer b[4096], i, j, jseed, ij, jk;
    static char dum[80];

    /* Fortran I/O blocks */
    static cilist io___376 = { 0, 4, 0, fmt_845, 0 };
    static cilist io___379 = { 0, 4, 0, fmt_845, 0 };
    static cilist io___381 = { 0, 6, 0, fmt_845, 0 };
    static cilist io___382 = { 0, 6, 0, 0, 0 };
    static cilist io___383 = { 0, 5, 0, 0, 0 };
    static cilist io___390 = { 0, 2, 0, 0, 0 };
    static cilist io___391 = { 0, 6, 0, 0, 0 };
    static cilist io___392 = { 0, 6, 0, 0, 0 };


    o__1.oerr = 0;
    o__1.ounit = 2;
    o__1.ofnmlen = 7;
    o__1.ofnm = "ran2.32";
    o__1.orl = 16384;
    o__1.osta = 0;
    o__1.oacc = "direct";
    o__1.ofm = "unformatted";
    o__1.oblnk = 0;
    f_open(&o__1);
    s_rsfe(&io___376);
    for (j = 1; j <= 211; ++j) {
	do_fio(&c__1, dum, 80L);
    }
    e_rsfe();
    s_rsfe(&io___379);
    do_fio(&c__16, text, 80L);
    e_rsfe();
    s_wsfe(&io___381);
    do_fio(&c__16, text, 80L);
    e_wsfe();
    s_wsle(&io___382);
    do_lio(&c__9, &c__1, " enter a seed integer", 21L);
    e_wsle();
    s_rsle(&io___383);
    do_lio(&c__3, &c__1, (char *)&jseed, (ftnlen)sizeof(integer));
    e_rsle();
    idum = -abs(jseed);
    jk = 0;
    for (ij = 1; ij <= 700; ++ij) {
	++jk;
	for (i = 1; i <= 4096; ++i) {
/* L2: */
	    b[i - 1] = iran2_(&idum) << 1;
	}
/* L3: */
	io___390.cirec = jk;
	s_wdue(&io___390);
	do_uio(&c__4096, (char *)&b[0], (ftnlen)sizeof(integer));
	e_wdue();
    }
    s_wsle(&io___391);
    do_lio(&c__9, &c__1, "  Now 2,867,200 left-adjusted integers from ran2", 
	    48L);
    e_wsle();
    s_wsle(&io___392);
    do_lio(&c__9, &c__1, "  (11,486,800 bytes) have been written to ran2.32", 
	    49L);
    e_wsle();
    return 0;
} /* makeran2_ */

integer iran2_(idum)
integer *idum;
{
    /* Initialized data */

    static integer idum2 = 123456789;
    static integer iv[32] = { 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0 };
    static integer iy = 0;

    /* System generated locals */
    integer ret_val, i__1;

    /* Local variables */
    static integer j, k;

    if (*idum <= 0) {
/* Computing MAX */
	i__1 = -(*idum);
	*idum = max(i__1,1);
	idum2 = *idum;
	for (j = 40; j >= 1; --j) {
	    k = *idum / 53668;
	    *idum = (*idum - k * 53668) * 40014 - k * 12211;
	    if (*idum < 0) {
		*idum += 2147483563;
	    }
	    if (j <= 32) {
		iv[j - 1] = *idum;
	    }
/* L11: */
	}
	iy = iv[0];
    }
    k = *idum / 53668;
    *idum = (*idum - k * 53668) * 40014 - k * 12211;
    if (*idum < 0) {
	*idum += 2147483563;
    }
    k = idum2 / 52774;
    idum2 = (idum2 - k * 52774) * 40692 - k * 3791;
    if (idum2 < 0) {
	idum2 += 2147483399;
    }
    j = iy / 67108862 + 1;
    iy = iv[j - 1] - idum2;
    iv[j - 1] = *idum;
    if (iy < 1) {
	iy += 2147483562;
    }
    ret_val = iy;
/*      ran2=min(AM*iy,RNMX) */
    return ret_val;
} /* iran2_ */

/* Subroutine */ int makeshrg_()
{
    /* Format strings */
    static char fmt_845[] = "(a78)";
    static char fmt_645[] = "(a15)";
    static char fmt_221[] = "(\002  2,867,200 32-bit random integers (11,468\
,800 bytes)\002,/,\002  have been written to the file \002,a15)";
    static char fmt_225[] = "(\002 The period of your generator is 2^\002,\
i2,\002-1.\002)";
    static char fmt_226[] = "(\002 The period of your generator is 2^32-2^21\
-2^11+1.\002)";

    /* System generated locals */
    integer i__1, i__2;
    olist o__1;

    /* Builtin functions */
    integer s_rsfe(), do_fio(), e_rsfe(), s_wsfe(), e_wsfe(), s_wsle(), 
	    do_lio(), e_wsle(), f_open(), s_rsle(), e_rsle(), s_wdue(), 
	    do_uio(), e_wdue();

    /* Local variables */
    static integer mask;
    static char filename[15], text[80*12];
    static integer b[4096], i, j, l, r, nbits, i1, l2, jk;
    static char dum[80];

    /* Fortran I/O blocks */
    static cilist io___398 = { 0, 4, 0, fmt_845, 0 };
    static cilist io___401 = { 0, 4, 0, fmt_845, 0 };
    static cilist io___403 = { 0, 6, 0, fmt_845, 0 };
    static cilist io___404 = { 0, 6, 0, 0, 0 };
    static cilist io___405 = { 0, 5, 0, fmt_645, 0 };
    static cilist io___407 = { 0, 6, 0, 0, 0 };
    static cilist io___408 = { 0, 5, 0, 0, 0 };
    static cilist io___411 = { 0, 6, 0, 0, 0 };
    static cilist io___412 = { 0, 6, 0, 0, 0 };
    static cilist io___413 = { 0, 6, 0, 0, 0 };
    static cilist io___414 = { 0, 5, 0, 0, 0 };
    static cilist io___418 = { 0, 6, 0, 0, 0 };
    static cilist io___419 = { 0, 5, 0, 0, 0 };
    static cilist io___420 = { 0, 6, 0, 0, 0 };
    static cilist io___424 = { 0, 1, 0, 0, 0 };
    static cilist io___425 = { 0, 6, 0, 0, 0 };
    static cilist io___426 = { 0, 6, 0, 0, 0 };
    static cilist io___427 = { 0, 6, 0, 0, 0 };
    static cilist io___428 = { 0, 6, 0, 0, 0 };
    static cilist io___429 = { 0, 5, 0, 0, 0 };
    static cilist io___431 = { 0, 6, 0, 0, 0 };
    static cilist io___432 = { 0, 5, 0, 0, 0 };
    static cilist io___433 = { 0, 6, 0, 0, 0 };
    static cilist io___434 = { 0, 1, 0, 0, 0 };
    static cilist io___435 = { 0, 1, 0, 0, 0 };
    static cilist io___436 = { 0, 6, 0, 0, 0 };
    static cilist io___437 = { 0, 6, 0, fmt_221, 0 };
    static cilist io___438 = { 0, 6, 0, fmt_225, 0 };
    static cilist io___439 = { 0, 6, 0, fmt_226, 0 };


/* *** creates shift register random number file */
/*  use 13,18 or 7,24 or 6,25 or 3,28 or reverse for 31 bits */
/* use 15,17 or 13,17,5 for 32 bits */
    s_rsfe(&io___398);
    for (j = 1; j <= 227; ++j) {
	do_fio(&c__1, dum, 80L);
    }
    e_rsfe();
    s_rsfe(&io___401);
    do_fio(&c__12, text, 80L);
    e_rsfe();
    s_wsfe(&io___403);
    do_fio(&c__12, text, 80L);
    e_wsfe();
    s_wsle(&io___404);
    do_lio(&c__9, &c__1, "  Enter output file name (<= 15 characters):", 44L);
    e_wsle();
    s_rsfe(&io___405);
    do_fio(&c__1, filename, 15L);
    e_rsfe();
    o__1.oerr = 0;
    o__1.ounit = 1;
    o__1.ofnmlen = 15;
    o__1.ofnm = filename;
    o__1.orl = 16384;
    o__1.osta = 0;
    o__1.oacc = "direct";
    o__1.ofm = "unformatted";
    o__1.oblnk = 0;
    f_open(&o__1);
    s_wsle(&io___407);
    do_lio(&c__9, &c__1, "   Choose number of bits, 31 or 32:", 35L);
    e_wsle();
    s_rsle(&io___408);
    do_lio(&c__3, &c__1, (char *)&nbits, (ftnlen)sizeof(integer));
    e_rsle();
    jk = 0;
    if (nbits == 31) {
	s_wsle(&io___411);
	do_lio(&c__9, &c__1, "  Choose Left,Right shifts from these choices:",
		 46L);
	e_wsle();
	s_wsle(&io___412);
	do_lio(&c__9, &c__1, "    13,18  18,13  24,7  7,24 6,25  25,6  28,3 \
 3,28", 51L);
	e_wsle();
	s_wsle(&io___413);
	do_lio(&c__9, &c__1, "  enter two integers, L and R, in free format:",
		 46L);
	e_wsle();
	s_rsle(&io___414);
	do_lio(&c__3, &c__1, (char *)&l, (ftnlen)sizeof(integer));
	do_lio(&c__3, &c__1, (char *)&r, (ftnlen)sizeof(integer));
	e_rsle();
	mask = 2147483647;
	s_wsle(&io___418);
	do_lio(&c__9, &c__1, " Enter seed integer, not zero:", 30L);
	e_wsle();
	s_rsle(&io___419);
	do_lio(&c__3, &c__1, (char *)&j, (ftnlen)sizeof(integer));
	e_rsle();
	s_wsle(&io___420);
	do_lio(&c__9, &c__1, "  Please wait..............", 27L);
	e_wsle();
	for (i1 = 1; i1 <= 700; ++i1) {
	    ++jk;
	    for (i = 1; i <= 4096; ++i) {
		i__1 = (j ^ j << l) & mask;
		j = i__1 ^ i__1 >> r;
/* L3: */
		b[i - 1] = j + j;
	    }
/* L2: */
	    io___424.cirec = jk;
	    s_wdue(&io___424);
	    do_uio(&c__4096, (char *)&b[0], (ftnlen)sizeof(integer));
	    e_wdue();
	}
    }
    if (nbits == 32) {
	s_wsle(&io___425);
	do_lio(&c__9, &c__1, " For 32 bit integers, you have three choices.", 
		45L);
	e_wsle();
	s_wsle(&io___426);
	do_lio(&c__9, &c__1, "  two shifts: (L,R)=(17,15) or (15,17) and", 
		42L);
	e_wsle();
	s_wsle(&io___427);
	do_lio(&c__9, &c__1, "  three shifts (L1,R,L2)=(13,17,5).", 35L);
	e_wsle();
	s_wsle(&io___428);
	do_lio(&c__9, &c__1, " Enter three integers, 17 15 0 or 15 17 0 or 1\
3 17 5:", 53L);
	e_wsle();
	s_rsle(&io___429);
	do_lio(&c__3, &c__1, (char *)&l, (ftnlen)sizeof(integer));
	do_lio(&c__3, &c__1, (char *)&r, (ftnlen)sizeof(integer));
	do_lio(&c__3, &c__1, (char *)&l2, (ftnlen)sizeof(integer));
	e_rsle();
	s_wsle(&io___431);
	do_lio(&c__9, &c__1, " Enter seed integer, not zero:", 30L);
	e_wsle();
	s_rsle(&io___432);
	do_lio(&c__3, &c__1, (char *)&j, (ftnlen)sizeof(integer));
	e_rsle();
	s_wsle(&io___433);
	do_lio(&c__9, &c__1, "  Please wait..............", 27L);
	e_wsle();
	if (l2 == 0) {
	    for (i1 = 1; i1 <= 700; ++i1) {
		++jk;
		for (i = 1; i <= 4096; ++i) {
		    i__1 = j ^ j << l;
		    j = i__1 ^ i__1 >> r;
/* L5: */
		    b[i - 1] = j;
		}
/* L4: */
		io___434.cirec = jk;
		s_wdue(&io___434);
		do_uio(&c__4096, (char *)&b[0], (ftnlen)sizeof(integer));
		e_wdue();
	    }
	} else if (l2 == 5) {
	    for (i1 = 1; i1 <= 700; ++i1) {
		++jk;
		for (i = 1; i <= 4096; ++i) {
		    i__1 = j ^ j << l;
		    i__2 = i__1 ^ i__1 >> r;
		    j = i__2 ^ i__2 << l2;
/* L7: */
		    b[i - 1] = j;
		}
/* L6: */
		io___435.cirec = jk;
		s_wdue(&io___435);
		do_uio(&c__4096, (char *)&b[0], (ftnlen)sizeof(integer));
		e_wdue();
	    }
	}
    }
    s_wsle(&io___436);
    do_lio(&c__9, &c__1, "      FINISHED", 14L);
    e_wsle();
    s_wsfe(&io___437);
    do_fio(&c__1, filename, 15L);
    e_wsfe();
    if (nbits == 31 || l2 == 5) {
	s_wsfe(&io___438);
	do_fio(&c__1, (char *)&nbits, (ftnlen)sizeof(integer));
	e_wsfe();
    }
    if (l == 15 || l == 17) {
	s_wsfe(&io___439);
	e_wsfe();
    }
    return 0;
} /* makeshrg_ */

/* Subroutine */ int makesunr_()
{
    /* Format strings */
    static char fmt_845[] = "(a78)";

    /* System generated locals */
    olist o__1;

    /* Builtin functions */
    integer f_open(), s_rsfe(), do_fio(), e_rsfe(), s_wsfe(), e_wsfe(), 
	    s_wsle(), do_lio(), e_wsle(), s_rsle(), e_rsle(), s_wdue(), 
	    do_uio(), e_wdue();

    /* Local variables */
    static char text[80*18];
    static integer b[4096], i, j, jseed, jk, ijk;
    static char dum[80];

    /* Fortran I/O blocks */
    static cilist io___440 = { 0, 4, 0, fmt_845, 0 };
    static cilist io___443 = { 0, 4, 0, fmt_845, 0 };
    static cilist io___445 = { 0, 6, 0, fmt_845, 0 };
    static cilist io___446 = { 0, 6, 0, 0, 0 };
    static cilist io___447 = { 0, 5, 0, 0, 0 };
    static cilist io___449 = { 0, 6, 0, 0, 0 };
    static cilist io___454 = { 0, 2, 0, 0, 0 };
    static cilist io___455 = { 0, 6, 0, 0, 0 };
    static cilist io___456 = { 0, 6, 0, 0, 0 };
    static cilist io___457 = { 0, 6, 0, 0, 0 };
    static cilist io___458 = { 0, 6, 0, 0, 0 };


    o__1.oerr = 0;
    o__1.ounit = 2;
    o__1.ofnmlen = 9;
    o__1.ofnm = "sunran.32";
    o__1.orl = 16384;
    o__1.osta = 0;
    o__1.oacc = "direct";
    o__1.ofm = "unformatted";
    o__1.oblnk = 0;
    f_open(&o__1);
    s_rsfe(&io___440);
    for (j = 1; j <= 239; ++j) {
	do_fio(&c__1, dum, 80L);
    }
    e_rsfe();
    s_rsfe(&io___443);
    do_fio(&c__18, text, 80L);
    e_rsfe();
    s_wsfe(&io___445);
    do_fio(&c__18, text, 80L);
    e_wsfe();
    s_wsle(&io___446);
    do_lio(&c__9, &c__1, " Enter one  seed integer:", 25L);
    e_wsle();
    s_rsle(&io___447);
    do_lio(&c__3, &c__1, (char *)&jseed, (ftnlen)sizeof(integer));
    e_rsle();
    s_wsle(&io___449);
    do_lio(&c__9, &c__1, "       Please wait..............", 32L);
    e_wsle();
/*       ijk=irand(jseed) */
    ijk = 1;
    jk = 0;
    for (j = 1; j <= 700; ++j) {
	++jk;
	for (i = 1; i <= 4096; ++i) {
/* L2: */
	    b[i - 1] = 1;
	}
/* 2      b(i)=irand(1) */
/* L3: */
	io___454.cirec = jk;
	s_wdue(&io___454);
	do_uio(&c__4096, (char *)&b[0], (ftnlen)sizeof(integer));
	e_wdue();
    }
    s_wsle(&io___455);
    do_lio(&c__9, &c__1, "            FINISHED", 20L);
    e_wsle();
    s_wsle(&io___456);
    do_lio(&c__9, &c__1, " 2,867,200  32-bit random integers (11,468,800 byt\
es)", 53L);
    e_wsle();
    s_wsle(&io___457);
    do_lio(&c__9, &c__1, "  have been written to the file sunran.32  ", 43L);
    e_wsle();
    s_wsle(&io___458);
    do_lio(&c__9, &c__1, "  Note: the rightmost bit is always 0", 37L);
    e_wsle();
    return 0;
} /* makesunr_ */

/* Subroutine */ int makefibo_()
{
    /* Format strings */
    static char fmt_845[] = "(a78)";
    static char fmt_346[] = "(\002 Enter lags r and s from this list:\002,/\
,\002    17,5  33,13  39,14  55,24  63,31  73,25  97,33 607,273\002,/,\002  \
   and op code: 1 for + , 2 for -, 3 for *, 4 for xor:\002)";
    static char fmt_645[] = "(a15)";
    static char fmt_348[] = "(\002    Enter four positive integers for see\
ds:\002)";
    static char fmt_349[] = "(\002     The binary file \002,a15,/,\002     h\
as been created with 32-bit integers\002,/,\002     from the lagged-Fibonacc\
i sequence\002)";
    static char fmt_351[] = "(15x,\002x(n) = x(n-\002,i3,\002) + x(n-\002,\
i3,\002) mod 2^32\002)";
    static char fmt_352[] = "(15x,\002x(n) = x(n-\002,i3,\002) - x(n-\002,\
i3,\002) mod 2^32\002)";
    static char fmt_353[] = "(15x,\002x(n) = x(n-\002,i3,\002) * x(n-\002,\
i3,\002) mod 2^32\002)";
    static char fmt_354[] = "(15x,\002x(n) = x(n-\002,i3,\002) xor x(n-\002,\
i3,\002)\002)";
    static char fmt_355[] = "(\002   The period of the sequence is \002)";

    /* System generated locals */
    olist o__1;

    /* Builtin functions */
    integer s_rsfe(), do_fio(), e_rsfe(), s_wsfe(), e_wsfe(), s_rsle(), 
	    do_lio(), e_rsle(), s_wsle(), e_wsle(), f_open(), s_wdue(), 
	    do_uio(), e_wdue();
    double pow_di();

    /* Local variables */
    static char filename[15];
    static integer juni;
    static char text[80*9];
    static integer i, j, k, l, m, r, s, bb[4096], ii, jj, jk, ip, jp, js, op, 
	    ju[607];
    static doublereal period;
    static char dum[80];

    /* Fortran I/O blocks */
    static cilist io___459 = { 0, 4, 0, fmt_845, 0 };
    static cilist io___462 = { 0, 4, 0, fmt_845, 0 };
    static cilist io___464 = { 0, 6, 0, fmt_845, 0 };
    static cilist io___465 = { 0, 6, 0, fmt_346, 0 };
    static cilist io___466 = { 0, 5, 0, 0, 0 };
    static cilist io___470 = { 0, 6, 0, 0, 0 };
    static cilist io___471 = { 0, 5, 0, fmt_645, 0 };
    static cilist io___473 = { 0, 6, 0, fmt_348, 0 };
    static cilist io___474 = { 0, 5, 0, 0, 0 };
    static cilist io___478 = { 0, 6, 0, 0, 0 };
    static cilist io___489 = { 0, 1, 0, 0, 0 };
    static cilist io___491 = { 0, 6, 0, 0, 0 };
    static cilist io___492 = { 0, 6, 0, 0, 0 };
    static cilist io___493 = { 0, 6, 0, fmt_349, 0 };
    static cilist io___494 = { 0, 6, 0, 0, 0 };
    static cilist io___495 = { 0, 6, 0, fmt_351, 0 };
    static cilist io___496 = { 0, 6, 0, fmt_352, 0 };
    static cilist io___497 = { 0, 6, 0, fmt_353, 0 };
    static cilist io___498 = { 0, 6, 0, fmt_354, 0 };
    static cilist io___499 = { 0, 6, 0, fmt_355, 0 };
    static cilist io___500 = { 0, 6, 0, 0, 0 };


    s_rsfe(&io___459);
    for (j = 1; j <= 257; ++j) {
	do_fio(&c__1, dum, 80L);
    }
    e_rsfe();
    s_rsfe(&io___462);
    do_fio(&c__9, text, 80L);
    e_rsfe();
    s_wsfe(&io___464);
    do_fio(&c__9, text, 80L);
    e_wsfe();
    s_wsfe(&io___465);
    e_wsfe();
    s_rsle(&io___466);
    do_lio(&c__3, &c__1, (char *)&r, (ftnlen)sizeof(integer));
    do_lio(&c__3, &c__1, (char *)&s, (ftnlen)sizeof(integer));
    do_lio(&c__3, &c__1, (char *)&op, (ftnlen)sizeof(integer));
    e_rsle();
    s_wsle(&io___470);
    do_lio(&c__9, &c__1, "     Enter output file name (<=15 characters):", 
	    46L);
    e_wsle();
    s_rsfe(&io___471);
    do_fio(&c__1, filename, 15L);
    e_rsfe();
    o__1.oerr = 0;
    o__1.ounit = 1;
    o__1.ofnmlen = 15;
    o__1.ofnm = filename;
    o__1.orl = 16384;
    o__1.osta = 0;
    o__1.oacc = "direct";
    o__1.ofm = "unformatted";
    o__1.oblnk = 0;
    f_open(&o__1);
    s_wsfe(&io___473);
    e_wsfe();
    s_rsle(&io___474);
    do_lio(&c__3, &c__1, (char *)&i, (ftnlen)sizeof(integer));
    do_lio(&c__3, &c__1, (char *)&j, (ftnlen)sizeof(integer));
    do_lio(&c__3, &c__1, (char *)&k, (ftnlen)sizeof(integer));
    do_lio(&c__3, &c__1, (char *)&l, (ftnlen)sizeof(integer));
    e_rsle();
    s_wsle(&io___478);
    do_lio(&c__9, &c__1, "    Please wait................", 31L);
    e_wsle();
/*              do 90 ijk=1,i */
/* 90            jjn=irand(1) */
    for (ii = 1; ii <= 97; ++ii) {
	js = (float)0.;
	for (jj = 1; jj <= 32; ++jj) {
	    m = i * j % 179 * k % 179;
	    i = j;
	    j = k;
	    k = m;
	    l = (l * 53 + 1) % 169;
	    js <<= 1;
	    if (l * m % 64 >= 32) {
		++js;
	    }
/* L93: */
	}
	if (op == 3) {
	    js ^= 1;
	}
/* L92: */
	ju[ii - 1] = js;
    }
    ip = r;
    jp = s;
    jk = 0;
    for (i = 1; i <= 700; ++i) {
	++jk;
	for (j = 1; j <= 4096; ++j) {
	    if (op == 1) {
		juni = ju[ip - 1] + ju[jp - 1];
	    } else if (op == 2) {
		juni = ju[ip - 1] - ju[jp - 1];
	    } else if (op == 3) {
		juni = ju[ip - 1] * ju[jp - 1];
	    } else {
		juni = ju[ip - 1] ^ ju[jp - 1];
	    }
	    ju[ip - 1] = juni;
	    --ip;
	    if (ip == 0) {
		ip = 97;
	    }
	    --jp;
	    if (jp == 0) {
		jp = 97;
	    }
/* L3: */
	    bb[j - 1] = juni;
	}
/* L2: */
	io___489.cirec = jk;
	s_wdue(&io___489);
	do_uio(&c__4096, (char *)&bb[0], (ftnlen)sizeof(integer));
	e_wdue();
    }
    period = (pow_di(&c_b546, &r) - 1) * 4294967296.;
    if (op == 3) {
	period *= (float).25;
    }
    if (op == 4) {
	period = pow_di(&c_b546, &r) - 1;
    }
    s_wsle(&io___491);
    do_lio(&c__9, &c__1, "     ", 5L);
    e_wsle();
    s_wsle(&io___492);
    do_lio(&c__9, &c__1, "     FINISHED", 13L);
    e_wsle();
    s_wsfe(&io___493);
    do_fio(&c__1, filename, 15L);
    e_wsfe();
    if (op == 1) {
	s_wsle(&io___494);
	do_lio(&c__9, &c__1, "++++++++++++++++++++++++++++++++++++++++++++++\
+++++++++", 55L);
	e_wsle();
	s_wsfe(&io___495);
	do_fio(&c__1, (char *)&r, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&s, (ftnlen)sizeof(integer));
	e_wsfe();
    } else if (op == 2) {
	s_wsfe(&io___496);
	do_fio(&c__1, (char *)&r, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&s, (ftnlen)sizeof(integer));
	e_wsfe();
    } else if (op == 3) {
	s_wsfe(&io___497);
	do_fio(&c__1, (char *)&r, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&s, (ftnlen)sizeof(integer));
	e_wsfe();
    } else {
	s_wsfe(&io___498);
	do_fio(&c__1, (char *)&r, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&s, (ftnlen)sizeof(integer));
	e_wsfe();
    }
    s_wsfe(&io___499);
    e_wsfe();
    s_wsle(&io___500);
    do_lio(&c__9, &c__1, "     ", 5L);
    do_lio(&c__5, &c__1, (char *)&period, (ftnlen)sizeof(doublereal));
    e_wsle();
    return 0;
} /* makefibo_ */

/* Subroutine */ int prod_(x, y, z)
integer *x, *y, *z;
{
    static integer d, r[4], s[4];

    /* Parameter adjustments */
    --z;
    --y;
    --x;

    /* Function Body */
    d = x[1] * y[1];
    z[1] = d & 65535;
    d = (d >> 16) + x[1] * y[2];
    r[1] = d & 65535;
    r[2] = d >> 16;
    d = x[2] * y[1];
    s[1] = d & 65535;
    d = (d >> 16) + x[2] * y[2];
    s[2] = d & 65535;
    s[3] = d >> 16;
    d = r[1] + s[1];
    z[2] = d & 65535;
    d = (d >> 16) + r[2] + s[2];
    z[3] = d & 65535;
    z[4] = (d >> 16) + s[3];
    return 0;
} /* prod_ */

/* Subroutine */ int sum_(x, y, z)
integer *x, *y, *z;
{
    static integer d;

    /* Parameter adjustments */
    --z;
    --y;
    --x;

    /* Function Body */
    d = x[1] + y[1];
    z[1] = d & 65535;
    d = (d >> 16) + x[2] + y[2];
    z[2] = d & 65535;
    d = (d >> 16) + x[3] + y[3];
    z[3] = d & 65535;
    d = (d >> 16) + x[4] + y[4];
    z[4] = d & 65535;
    return 0;
} /* sum_ */

/* Subroutine */ int makeinvc_()
{
    /* Initialized data */

    static integer a = 69069;
    static integer b = 362436069;
    static doublereal dz = 123456789.;

    /* Format strings */
    static char fmt_845[] = "(a78)";
    static char fmt_471[] = "(\002 The file invcong.32 now contains 11,428,8\
00 bytes of 32-bit\002,/,\002 integers from the inverse congruential generat\
or.\002)";

    /* System generated locals */
    doublereal d__1;
    olist o__1;

    /* Builtin functions */
    integer f_open(), s_rsfe(), do_fio(), e_rsfe(), s_wsfe(), e_wsfe(), 
	    s_wsle(), do_lio(), e_wsle(), s_rsle(), e_rsle();
    double d_mod();
    integer s_wdue(), do_uio(), e_wdue();

    /* Local variables */
    static char text[80*15];
    static integer i, j, n[4096], icong;
    static doublereal a0, b0, b1, b2, a1, a2;
    static integer ij, jk;
    static char dum[80];

    /* Fortran I/O blocks */
    static cilist io___509 = { 0, 4, 0, fmt_845, 0 };
    static cilist io___512 = { 0, 4, 0, fmt_845, 0 };
    static cilist io___514 = { 0, 6, 0, fmt_845, 0 };
    static cilist io___515 = { 0, 6, 0, 0, 0 };
    static cilist io___516 = { 0, 5, 0, 0, 0 };
    static cilist io___517 = { 0, 6, 0, 0, 0 };
    static cilist io___518 = { 0, 6, 0, 0, 0 };
    static cilist io___529 = { 0, 1, 0, 0, 0 };
    static cilist io___530 = { 0, 6, 0, fmt_471, 0 };


    b0 = 4294967296.;
    o__1.oerr = 0;
    o__1.ounit = 1;
    o__1.ofnmlen = 10;
    o__1.ofnm = "invcong.32";
    o__1.orl = 16384;
    o__1.osta = 0;
    o__1.oacc = "direct";
    o__1.ofm = "unformatted";
    o__1.oblnk = 0;
    f_open(&o__1);
    s_rsfe(&io___509);
    for (j = 1; j <= 266; ++j) {
	do_fio(&c__1, dum, 80L);
    }
    e_rsfe();
    s_rsfe(&io___512);
    do_fio(&c__15, text, 80L);
    e_rsfe();
    s_wsfe(&io___514);
    do_fio(&c__15, text, 80L);
    e_wsfe();
    s_wsle(&io___515);
    do_lio(&c__9, &c__1, "  Enter a and b and seed integer, free format:", 
	    46L);
    e_wsle();
    s_rsle(&io___516);
    do_lio(&c__3, &c__1, (char *)&a, (ftnlen)sizeof(integer));
    do_lio(&c__3, &c__1, (char *)&b, (ftnlen)sizeof(integer));
    do_lio(&c__5, &c__1, (char *)&dz, (ftnlen)sizeof(doublereal));
    e_rsle();
    s_wsle(&io___517);
    do_lio(&c__9, &c__1, "    This may take a long time, as this is a very", 
	    48L);
    e_wsle();
    s_wsle(&io___518);
    do_lio(&c__9, &c__1, "    very slow generator.  Please wait ...........", 
	    49L);
    e_wsle();
    jk = 0;
    for (ij = 1; ij <= 700; ++ij) {
	++jk;
	for (i = 1; i <= 4096; ++i) {
	    b1 = dz;
	    a0 = 0.;
	    a1 = 1.;
	    b0 = 4294967296.;
	    while(b1 > 0.) {
		b2 = d_mod(&b0, &b1);
		a2 = a0 - a1 * ((b0 - b2) / b1);
		b0 = b1;
		b1 = b2;
		a0 = a1;
		a1 = a2;
	    }
	    d__1 = a * b0 * a0 + b;
	    dz = d_mod(&d__1, &c_b842);
	    if (dz < 0.) {
		dz += 4294967296.;
	    }
	    if (dz > 2147483648.) {
		icong = (integer) (dz - 4294967296.);
	    } else {
		icong = (integer) dz;
	    }
/* L2: */
	    n[i - 1] = icong;
	}
/* L3: */
	io___529.cirec = jk;
	s_wdue(&io___529);
	do_uio(&c__4096, (char *)&n[0], (ftnlen)sizeof(integer));
	e_wdue();
    }
    s_wsfe(&io___530);
    e_wsfe();
} /* makeinvc_ */

/* Subroutine */ int asort_(list, n)
real *list;
integer *n;
{
    static integer i, j, k, l, m;
    static real t;
    static integer ij, il[33], iu[33];
    static real tt;

    /* Parameter adjustments */
    --list;

    /* Function Body */
    m = 1;
    i = 1;
    j = *n;
L5:
    if (i >= j) {
	goto L70;
    }
L10:
    k = i;
    ij = (i + j) / 2;
    t = list[ij];
    if (list[i] <= t) {
	goto L20;
    }
    list[ij] = list[i];
    list[i] = t;
    t = list[ij];
L20:
    l = j;
    if (list[j] >= t) {
	goto L40;
    }
    list[ij] = list[j];
    list[j] = t;
    t = list[ij];
    if (list[i] <= t) {
	goto L40;
    }
    list[ij] = list[i];
    list[i] = t;
    t = list[ij];
    goto L40;
L30:
    list[l] = list[k];
    list[k] = tt;
L40:
    --l;
    if (list[l] > t) {
	goto L40;
    }
    tt = list[l];
L50:
    ++k;
    if (list[k] < t) {
	goto L50;
    }
    if (k <= l) {
	goto L30;
    }
    if (l - i <= j - k) {
	goto L60;
    }
    il[m - 1] = i;
    iu[m - 1] = l;
    i = k;
    ++m;
    goto L80;
L60:
    il[m - 1] = k;
    iu[m - 1] = j;
    j = l;
    ++m;
    goto L80;
L70:
    --m;
    if (m <= 0) {
	return 0;
    }
    i = il[m - 1];
    j = iu[m - 1];
L80:
    if (j - i >= 11) {
	goto L10;
    }
    if (i == 1) {
	goto L5;
    }
    --i;
L90:
    ++i;
    if (i == j) {
	goto L70;
    }
    t = list[i + 1];
    if (list[i] <= t) {
	goto L90;
    }
    k = i;
L100:
    list[k + 1] = list[k];
    --k;
    if (t < list[k]) {
	goto L100;
    }
    list[k + 1] = t;
    goto L90;
} /* asort_ */

/* Subroutine */ int kstest_(y, n, p)
real *y;
integer *n;
real *p;
{
    /* Initialized data */

    static integer l[80]	/* was [8][10] */ = { 40,46,37,34,27,24,20,20,
	    88,59,43,37,29,27,20,22,92,63,48,41,30,30,25,24,82,59,42,37,26,28,
	    26,22,62,48,33,30,23,23,22,18,49,34,22,20,16,17,17,12,17,17,7,8,4,
	    7,5,1,40,18,19,14,16,13,10,9,59,20,10,4,1,1,0,-1,41,43,36,112,15,
	    95,32,58 };

    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    double log(), exp(), sqrt();

    /* Local variables */
    static real a, e;
    static integer i, j, m;
    static real t, z;
    extern /* Subroutine */ int asort_();
    extern doublereal sp_();

/*      TO TEST WHETHER A SET OF N REAL NUMBERS IS DRAWN */
/*      FROM A UNIFORM DISTRIBUTION (KOLMOROGOV-SMIRNOV METHOD) */
/*      THE TEST IS BASED ON THE DISTANCE BETWEEN THE EMPIRICAL */
/*      AND THEORETICAL DISTRIBUTION FUNCTIONS */
/*       USAGE: CALL KSTEST(Y,N,P) */
/*      Y ...   ARRAY OF REAL NUMBERS HYPOTHETICALLY DRAWN */
/*              FROM A UNIFORM DISTRIBUTION ON (0,1) */
/*      N ...   NUMBER OF ELEMENTS IN 'Y' */
/*      P IS THE PROBABILITY ASSOCIATED WITH THE OBSERVED VALUE */
/*      OF THE ANDERSON-DARLING STATISTIC: N TIMES THE INTEGRAL */
/*      OF (FN(X)-X)**2/(X*(1-X)) */
    /* Parameter adjustments */
    --y;

    /* Function Body */
    asort_(&y[1], n);
    z = -(*n) * (*n + (float)0.);
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
	t = y[i] * ((float)1. - y[*n + 1 - i]);
	if (t < (float)1e-20) {
	    t = (float)1e-20;
	}
/* L2: */
	z -= (i + i - 1) * log(t);
    }
    z /= *n;
    *p = (float)0.;
    if (z < (float).01) {
	goto L5;
    }
    if (z > (float)2.) {
	goto L3;
    }
    *p = exp((float)-1.2337 / z) * (float)2. * (z / (float)8. + (float)1. - z 
	    * (float).04958 * z / (z + (float)1.325)) / sqrt(z);
    goto L5;
L3:
    if (z > (float)4.) {
	goto L4;
    }
    *p = (float)1. - exp(z * (float)-1.091638) * (float).6621361 - exp(z * (
	    float)-2.005138) * (float).95059;
    goto L5;
L4:
    *p = (float)1. - exp(z * (float)-1.050321) * (float).4938691 - exp(z * (
	    float)-1.527198) * (float).5946335;
L5:
/* Computing MIN */
    i__1 = *n - 2;
    m = min(i__1,8);
    e = (float)0.;
    for (j = 1; j <= 10; ++j) {
/* L6: */
	e += l[m + (j << 3) - 9] * sp_(p, &j) * (float)1e-4;
    }
    if (*n > 10) {
	e = e * (float)10. / *n;
    }
    a = *p + e;
    return 0;
} /* kstest_ */

doublereal sp_(x, i)
real *x;
integer *i;
{
    /* System generated locals */
    real ret_val, r__1;

    /* Local variables */
    static real t;

    ret_val = (float)0.;
    switch ((int)*i) {
	case 1:  goto L7;
	case 2:  goto L7;
	case 3:  goto L7;
	case 4:  goto L7;
	case 5:  goto L7;
	case 6:  goto L7;
	case 7:  goto L7;
	case 8:  goto L8;
	case 9:  goto L9;
	case 10:  goto L10;
    }
L7:
    t = (r__1 = *x * (float)10. - (float).5 - *i, dabs(r__1));
    if (t > (float)1.5) {
	return ret_val;
    }
    if (t <= (float).5) {
	ret_val = (float)1.5 - t * (float)2. * t;
    } else {
	ret_val = (float)2.25 - t * ((float)3. - t);
    }
    return ret_val;
L8:
    if (*x <= (float).8 || *x >= (float)1.) {
	return ret_val;
    }
/* Computing 2nd power */
    r__1 = *x - (float).9;
    ret_val = r__1 * r__1 * (float)100. - (float)1.;
    return ret_val;
L9:
    if (*x <= (float)0. || *x >= (float).05) {
	return ret_val;
    }
    if (*x <= (float).01) {
	ret_val = *x * (float)-100.;
    } else {
	ret_val = (*x - (float).05) * (float)25.;
    }
    return ret_val;
L10:
    if (*x <= (float).98 || *x >= (float)1.) {
	return ret_val;
    }
    ret_val = (float).1 - (r__1 = *x - (float).99, dabs(r__1)) * (float)10.;
    return ret_val;
} /* sp_ */

/* Subroutine */ int plot1_0_(n__, x, y, n, nc, z)
int n__;
real *x, *y;
integer *n, *nc;
real *z;
{
    /* Initialized data */

    static struct {
	char e_1[4860];
	} equiv_1 = { {' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 
		' '} };


    /* Format strings */
    static char fmt_22[] = "(\002   X RANGE:\002,g12.3,\002 TO \002,g12.3\
,\002  Y,Z RANGE:\002,g12.3,\002  TO \002,g12.3)";
    static char fmt_21[] = "(10x,90a1)";

    /* System generated locals */
    integer i__1;
    real r__1, r__2;
    static char equiv_0[4860];

    /* Builtin functions */
    integer s_wsfe(), do_fio(), e_wsfe();
    /* Subroutine */ int s_copy();

    /* Local variables */
#define c_ (equiv_0)
#define d_ (equiv_0)
#define e ((char *)&equiv_1)
#define f ((char *)&equiv_1)
    static integer i, j, k, m;
    static logical p2;
    static real yb, xd, yd;
    static integer nr;
    static real xl, xr, yt;

    /* Fortran I/O blocks */
    static cilist io___561 = { 0, 6, 0, fmt_22, 0 };
    static cilist io___567 = { 0, 6, 0, fmt_21, 0 };


    /* Parameter adjustments */
    --x;
    --y;
    if (z) {
	--z;
	}

    /* Function Body */
    switch(n__) {
	case 1: goto L_plot2;
	}

    p2 = FALSE_;
    goto L1;

L_plot2:
    p2 = TRUE_;
L1:
    nr = *nc * 48 / 100;
    if (*nc > 60) {
	nr = *nc * 6 / 10;
    }
    xl = x[1];
    xr = xl;
    yb = y[1];
    yt = yb;
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
/* Computing MIN */
	r__1 = xl, r__2 = x[i];
	xl = dmin(r__1,r__2);
/* Computing MAX */
	r__1 = xr, r__2 = x[i];
	xr = dmax(r__1,r__2);
	if (p2) {
/* Computing MIN */
	    r__1 = yb, r__2 = z[i];
	    yb = dmin(r__1,r__2);
	}
	if (p2) {
/* Computing MAX */
	    r__1 = yt, r__2 = z[i];
	    yt = dmax(r__1,r__2);
	}
/* Computing MIN */
	r__1 = yb, r__2 = y[i];
	yb = dmin(r__1,r__2);
/* L2: */
/* Computing MAX */
	r__1 = yt, r__2 = y[i];
	yt = dmax(r__1,r__2);
    }
    s_wsfe(&io___561);
    do_fio(&c__1, (char *)&xl, (ftnlen)sizeof(real));
    do_fio(&c__1, (char *)&xr, (ftnlen)sizeof(real));
    do_fio(&c__1, (char *)&yb, (ftnlen)sizeof(real));
    do_fio(&c__1, (char *)&yt, (ftnlen)sizeof(real));
    e_wsfe();
    xd = (float)1.;
    if (xl != xr) {
	xd = *nc / (xr - xl);
    }
    yd = (float)1.;
    if (yb != yt) {
	yd = nr / (yt - yb);
    }
    s_copy(d_, e, 4860L, 4860L);
    i__1 = *n;
    for (m = 1; m <= i__1; ++m) {
	i = (x[m] - xl) * xd + (float)1.;
	j = (y[m] - yb) * yd + (float)1.;
	*(unsigned char *)&c_[i + j * 90 - 91] = '*';
	if (! p2) {
	    goto L4;
	}
	k = (z[m] - yb) * yd + (float)1.;
	*(unsigned char *)&c_[i + k * 90 - 91] = '+';
	if (j == k) {
	    *(unsigned char *)&c_[i + k * 90 - 91] = '0';
	}
L4:
	;
    }
    for (j = nr; j >= 1; --j) {
/* L5: */
	s_wsfe(&io___567);
	i__1 = *nc;
	for (i = 1; i <= i__1; ++i) {
	    do_fio(&c__1, c_ + (i + j * 90 - 91), 1L);
	}
	e_wsfe();
    }
    return 0;
} /* plot1_ */

#undef f
#undef e
#undef d_
#undef c_


/* Subroutine */ int plot1_(x, y, n, nc)
real *x, *y;
integer *n, *nc;
{
    return plot1_0_(0, x, y, n, nc, (real *)0);
    }

/* Subroutine */ int plot2_(x, y, z, n, nc)
real *x, *y, *z;
integer *n, *nc;
{
    return plot1_0_(1, x, y, n, nc, z);
    }

