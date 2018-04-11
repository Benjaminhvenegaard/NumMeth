/* getrnor.f -- translated by f2c (version 19950110).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__9 = 9;
static integer c__1 = 1;
static integer c__3 = 3;
static integer c__1000 = 1000;
static integer c__4096 = 4096;

/* Main program */ MAIN__()
{
    /* Format strings */
    static char fmt_12345[] = "(10x,\002This program will read a binary file\
 of\002,/,10x,\00232-bit random integers and write a binary file\002,/,10x\
,\002of standard normal random variables, using the\002,/,10x,\002ziggurat m\
ethod of Marsaglia and Tsang, \002,/,10x,\002SIAM J. Scient. and Stat. Compu\
ting, v5, 349-359, 1984\002,/)";
    static char fmt_20[] = "(a15)";
    static char fmt_25[] = "(i10,\002000 standard normal random variables\
\002,/,\002      have been written to the file \002,a15)";

    /* System generated locals */
    integer i__1;
    olist o__1;
    cllist cl__1;

    /* Builtin functions */
    integer s_wsfe(), e_wsfe(), s_wsle(), do_lio(), e_wsle(), s_rsfe(), 
	    do_fio(), e_rsfe(), f_open(), s_rsle(), e_rsle(), s_wdue(), 
	    do_uio(), e_wdue(), f_clos();

    /* Local variables */
    static char filename[15];
    static integer i, k, n;
    static real x[1000];
    static integer jk;
    extern doublereal zignor_();
    static char fileout[15];

    /* Fortran I/O blocks */
    static cilist io___1 = { 0, 6, 0, fmt_12345, 0 };
    static cilist io___2 = { 0, 6, 0, 0, 0 };
    static cilist io___3 = { 0, 5, 0, fmt_20, 0 };
    static cilist io___5 = { 0, 6, 0, 0, 0 };
    static cilist io___6 = { 0, 5, 0, fmt_20, 0 };
    static cilist io___8 = { 0, 6, 0, 0, 0 };
    static cilist io___9 = { 0, 5, 0, 0, 0 };
    static cilist io___11 = { 0, 6, 0, 0, 0 };
    static cilist io___16 = { 0, 2, 0, 0, 0 };
    static cilist io___17 = { 0, 6, 0, 0, 0 };
    static cilist io___18 = { 0, 6, 0, fmt_25, 0 };
    static cilist io___19 = { 0, 6, 0, 0, 0 };


/* **** converts binary file of 32-bit integers to */
/* **** single precision standard normal variables. */
    s_wsfe(&io___1);
    e_wsfe();
    s_wsle(&io___2);
    do_lio(&c__9, &c__1, " Enter name of source file ( binary, 32-bit intege\
rs):", 54L);
    e_wsle();
    s_rsfe(&io___3);
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
    s_wsle(&io___5);
    do_lio(&c__9, &c__1, " Enter name of output file:", 27L);
    e_wsle();
    s_rsfe(&io___6);
    do_fio(&c__1, fileout, 15L);
    e_rsfe();
    o__1.oerr = 0;
    o__1.ounit = 2;
    o__1.ofnmlen = 15;
    o__1.ofnm = fileout;
    o__1.orl = 4000;
    o__1.osta = 0;
    o__1.oacc = "direct";
    o__1.ofm = "unformatted";
    o__1.oblnk = 0;
    f_open(&o__1);
    s_wsle(&io___8);
    do_lio(&c__9, &c__1, " Enter n for output of RNOR'S (in thousands):", 45L)
	    ;
    e_wsle();
    s_rsle(&io___9);
    do_lio(&c__3, &c__1, (char *)&n, (ftnlen)sizeof(integer));
    e_rsle();
    s_wsle(&io___11);
    do_lio(&c__9, &c__1, "           Please wait ...............", 38L);
    e_wsle();
    jk = 0;
    i__1 = n;
    for (i = 1; i <= i__1; ++i) {
	for (k = 1; k <= 1000; ++k) {
/* L3: */
	    x[k - 1] = zignor_();
	}
	++jk;
/* L2: */
	io___16.cirec = jk;
	s_wdue(&io___16);
	do_uio(&c__1000, (char *)&x[0], (ftnlen)sizeof(real));
	e_wdue();
    }
    cl__1.cerr = 0;
    cl__1.cunit = 2;
    cl__1.csta = 0;
    f_clos(&cl__1);
    s_wsle(&io___17);
    do_lio(&c__9, &c__1, "           Finished", 19L);
    e_wsle();
    s_wsfe(&io___18);
    do_fio(&c__1, (char *)&n, (ftnlen)sizeof(integer));
    do_fio(&c__1, fileout, 15L);
    e_wsfe();
    goto L123;
/* L11111: */
    s_wsle(&io___19);
    do_lio(&c__9, &c__1, "  ERROR:  not enough integers in source file", 44L);
    e_wsle();
L123:
    ;
} /* MAIN__ */

integer jtbl_()
{
    /* Initialized data */

    static integer j = 4097;
    static integer jk = 1;

    /* System generated locals */
    integer ret_val;

    /* Builtin functions */
    integer s_rdue(), do_uio(), e_rdue();

    /* Local variables */
    static integer b[4096];

    /* Fortran I/O blocks */
    static cilist io___22 = { 0, 1, 0, 0, 0 };


    if (j > 4096) {
	io___22.cirec = jk;
	s_rdue(&io___22);
	do_uio(&c__4096, (char *)&b[0], (ftnlen)sizeof(integer));
	e_rdue();
	j = 1;
	++jk;
    }
    ret_val = b[j - 1];
    ++j;
    return ret_val;
} /* jtbl_ */

/* ***Ziggurat rnor. Takes 2.2 microsecs(Microsoft, 5.9 in Lahey) */
/* ****************  takes 2.0 if jsr is only rng, 2.2 if jsr+icng */
/* ***        mpynor takes 3.1, rnor92 takes 3.8 */
doublereal zignor_()
{
    /* Initialized data */

    static real v[65] = { (float).340945,(float).4573146,(float).5397793,(
	    float).6062427,(float).6631691,(float).7136975,(float).7596125,(
	    float).8020356,(float).8417227,(float).8792102,(float).9148948,(
	    float).9490791,(float).9820005,(float)1.0138492,(float)1.044781,(
	    float)1.0749254,(float)1.1043917,(float)1.1332738,(float)1.161653,
	    (float)1.189601,(float)1.2171815,(float)1.2444516,(float)
	    1.2714635,(float)1.298265,(float)1.3249008,(float)1.3514125,(
	    float)1.3778399,(float)1.4042211,(float)1.4305929,(float)
	    1.4569915,(float)1.4834527,(float)1.5100122,(float)1.5367061,(
	    float)1.5635712,(float)1.5906454,(float)1.617968,(float)1.6455802,
	    (float)1.6735255,(float)1.7018503,(float)1.7306045,(float)
	    1.7598422,(float)1.7896223,(float)1.8200099,(float)1.851077,(
	    float)1.8829044,(float)1.9155831,(float)1.9492166,(float)
	    1.9839239,(float)2.0198431,(float)2.0571356,(float)2.095993,(
	    float)2.136645,(float)2.1793713,(float)2.2245175,(float)2.2725186,
	    (float)2.3239338,(float)2.3795008,(float)2.4402218,(float)
	    2.5075117,(float)2.5834658,(float)2.6713916,(float)2.7769942,(
	    float)2.7769942,(float)2.7769942,(float)2.7769942 };

    /* System generated locals */
    real ret_val, r__1, r__2;

    /* Builtin functions */
    double exp(), log(), r_sign();

    /* Local variables */
    extern integer jtbl_();
    static integer i, j;
    static real s, x, y;

/* --FAST PART */
    i = jtbl_();
    j = i & 63;
    ret_val = i * (float)4.656613e-10 * v[j + 1];
    if (dabs(ret_val) <= v[j]) {
	return ret_val;
    }
/* -------SLOW PART; aa is a*f(0) */
    y = jtbl_() * (float)2.3283064365386963e-10 + (float).5;
    s = x + y;
    if (s > (float)1.301198) {
	goto L11;
    }
    if (s <= (float).9689279) {
	return ret_val;
    }
/* Computing 2nd power */
    r__1 = (float).4878992 - x * (float).4878992;
    if (y > (float)12.67706 - exp(r__1 * r__1 * (float)-.5) * (float)12.37586)
	     {
	goto L11;
    }
/* Computing 2nd power */
    r__1 = v[j + 1];
/* Computing 2nd power */
    r__2 = ret_val;
    if (exp(r__1 * r__1 * (float)-.5) + y * (float).01958303 / v[j + 1] <= 
	    exp(r__2 * r__2 * (float)-.5)) {
	return ret_val;
    }
/* ---------------TAIL PART; .3601016 is 1/xn */
L22:
    x = log(jtbl_() * (float)2.3283064365386963e-10 + (float).5) * (float)
	    .3601016;
    if (log(jtbl_() * (float)2.3283064365386963e-10 + (float).5) * (float)-2. 
	    <= x * x) {
	goto L22;
    }
/* L33: */
    r__1 = (float)2.776994 - x;
    ret_val = r_sign(&r__1, &ret_val);
    return ret_val;
L11:
    r__1 = (float).4878992 - x * (float).4878992;
    ret_val = r_sign(&r__1, &ret_val);
    return ret_val;
} /* zignor_ */

