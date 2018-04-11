/* meld.f -- translated by f2c (version 19950110).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__9 = 9;
static integer c__1 = 1;
static integer c__4096 = 4096;

/* Main program */ MAIN__()
{
    /* Format strings */
    static char fmt_845[] = "(a15)";
    static char fmt_35[] = "(a1)";
    static char fmt_99[] = "(\002  The files \002,a15,\002 and \002,a15,\002\
 have been merged\002,/,\002  and  9,994,240 bytes have been written to file \
\002,a15)";

    /* System generated locals */
    olist o__1;

    /* Builtin functions */
    integer s_wsle(), do_lio(), e_wsle(), s_rsfe(), do_fio(), e_rsfe(), 
	    f_open(), s_rdue(), do_uio(), e_rdue(), s_wdue(), e_wdue(), 
	    s_wsfe(), e_wsfe();

    /* Local variables */
    static char file1[15], file2[15], file3[15];
    static integer a[4096], b[4096], c[4096], i, k, jk;
    static char op[1];

    /* Fortran I/O blocks */
    static cilist io___1 = { 0, 6, 0, 0, 0 };
    static cilist io___2 = { 0, 6, 0, 0, 0 };
    static cilist io___3 = { 0, 6, 0, 0, 0 };
    static cilist io___4 = { 0, 5, 0, fmt_845, 0 };
    static cilist io___6 = { 0, 6, 0, 0, 0 };
    static cilist io___7 = { 0, 5, 0, fmt_845, 0 };
    static cilist io___9 = { 0, 6, 0, 0, 0 };
    static cilist io___10 = { 0, 5, 0, fmt_845, 0 };
    static cilist io___12 = { 0, 6, 0, 0, 0 };
    static cilist io___13 = { 0, 6, 0, 0, 0 };
    static cilist io___14 = { 0, 5, 0, fmt_35, 0 };
    static cilist io___16 = { 0, 6, 0, 0, 0 };
    static cilist io___19 = { 0, 1, 0, 0, 0 };
    static cilist io___21 = { 0, 2, 0, 0, 0 };
    static cilist io___25 = { 0, 3, 0, 0, 0 };
    static cilist io___26 = { 0, 1, 0, 0, 0 };
    static cilist io___27 = { 0, 2, 0, 0, 0 };
    static cilist io___28 = { 0, 3, 0, 0, 0 };
    static cilist io___29 = { 0, 6, 0, fmt_99, 0 };


/* L1: */
    s_wsle(&io___1);
    do_lio(&c__9, &c__1, " This program merges, via addition mod 2^32 or", 
	    46L);
    e_wsle();
    s_wsle(&io___2);
    do_lio(&c__9, &c__1, "  exclusive-or, the 32-bit integers in two binary \
files", 55L);
    e_wsle();
    s_wsle(&io___3);
    do_lio(&c__9, &c__1, " Enter first file name (<= 15 characters):", 42L);
    e_wsle();
    s_rsfe(&io___4);
    do_fio(&c__1, file1, 15L);
    e_rsfe();
    s_wsle(&io___6);
    do_lio(&c__9, &c__1, " Enter second file name (<= 15 characters):", 43L);
    e_wsle();
    s_rsfe(&io___7);
    do_fio(&c__1, file2, 15L);
    e_rsfe();
    s_wsle(&io___9);
    do_lio(&c__9, &c__1, "  Now choose a name for your output file:", 41L);
    e_wsle();
    s_rsfe(&io___10);
    do_fio(&c__1, file3, 15L);
    e_rsfe();
    o__1.oerr = 0;
    o__1.ounit = 1;
    o__1.ofnmlen = 15;
    o__1.ofnm = file1;
    o__1.orl = 16384;
    o__1.osta = 0;
    o__1.oacc = "direct";
    o__1.ofm = "unformatted";
    o__1.oblnk = 0;
    f_open(&o__1);
    o__1.oerr = 0;
    o__1.ounit = 2;
    o__1.ofnmlen = 15;
    o__1.ofnm = file2;
    o__1.orl = 16384;
    o__1.osta = 0;
    o__1.oacc = "direct";
    o__1.ofm = "unformatted";
    o__1.oblnk = 0;
    f_open(&o__1);
    s_wsle(&io___12);
    do_lio(&c__9, &c__1, " Choose your operation, + for add, x for exclusive\
-or:", 54L);
    e_wsle();
    s_wsle(&io___13);
    do_lio(&c__9, &c__1, " Enter choice in column 1:", 26L);
    e_wsle();
    s_rsfe(&io___14);
    do_fio(&c__1, op, 1L);
    e_rsfe();
    o__1.oerr = 0;
    o__1.ounit = 3;
    o__1.ofnmlen = 15;
    o__1.ofnm = file3;
    o__1.orl = 16384;
    o__1.osta = 0;
    o__1.oacc = "direct";
    o__1.ofm = "unformatted";
    o__1.oblnk = 0;
    f_open(&o__1);
    s_wsle(&io___16);
    do_lio(&c__9, &c__1, "  Please wait...............", 28L);
    e_wsle();
    jk = 0;
    if (*(unsigned char *)op == '+') {
	for (i = 1; i <= 610; ++i) {
	    ++jk;
	    io___19.cirec = jk;
	    s_rdue(&io___19);
	    do_uio(&c__4096, (char *)&a[0], (ftnlen)sizeof(integer));
	    e_rdue();
	    io___21.cirec = jk;
	    s_rdue(&io___21);
	    do_uio(&c__4096, (char *)&b[0], (ftnlen)sizeof(integer));
	    e_rdue();
	    for (k = 1; k <= 4096; ++k) {
/* L3: */
		c[k - 1] = a[k - 1] + b[k - 1];
	    }
/* L2: */
	    io___25.cirec = jk;
	    s_wdue(&io___25);
	    do_uio(&c__4096, (char *)&c[0], (ftnlen)sizeof(integer));
	    e_wdue();
	}
    } else if (*(unsigned char *)op == 'x') {
	for (i = 1; i <= 610; ++i) {
	    ++jk;
	    io___26.cirec = jk;
	    s_rdue(&io___26);
	    do_uio(&c__4096, (char *)&a[0], (ftnlen)sizeof(integer));
	    e_rdue();
	    io___27.cirec = jk;
	    s_rdue(&io___27);
	    do_uio(&c__4096, (char *)&b[0], (ftnlen)sizeof(integer));
	    e_rdue();
	    for (k = 1; k <= 4096; ++k) {
/* L93: */
		c[k - 1] = a[k - 1] ^ b[k - 1];
	    }
/* L92: */
	    io___28.cirec = jk;
	    s_wdue(&io___28);
	    do_uio(&c__4096, (char *)&c[0], (ftnlen)sizeof(integer));
	    e_wdue();
	}
    }
    s_wsfe(&io___29);
    do_fio(&c__1, file1, 15L);
    do_fio(&c__1, file2, 15L);
    do_fio(&c__1, file3, 15L);
    e_wsfe();
} /* MAIN__ */

