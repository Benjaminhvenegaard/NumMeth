/* asc2bin.f -- translated by f2c (version 19950110).
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
    static char fmt_852[] = "(a1)";
    static char fmt_818[] = "(a15)";
    static char fmt_21[] = "(10z8)";
    static char fmt_23[] = "(\002  OK, binary file \002,a15,\002 has been cr\
eated.\002)";

    /* System generated locals */
    olist o__1;
    cllist cl__1;

    /* Builtin functions */
    integer s_wsle(), do_lio(), e_wsle(), s_rsfe(), do_fio(), e_rsfe(), 
	    f_open(), s_wdue(), do_uio(), e_wdue(), s_wsfe(), e_wsfe(), 
	    f_clos();

    /* Local variables */
    static integer i, m[4096], jk;
    static char dum[1], ascfile[15], binfile[15];

    /* Fortran I/O blocks */
    static cilist io___1 = { 0, 6, 0, 0, 0 };
    static cilist io___2 = { 0, 6, 0, 0, 0 };
    static cilist io___3 = { 0, 6, 0, 0, 0 };
    static cilist io___4 = { 0, 6, 0, 0, 0 };
    static cilist io___5 = { 0, 5, 0, fmt_852, 0 };
    static cilist io___7 = { 0, 6, 0, 0, 0 };
    static cilist io___8 = { 0, 6, 0, 0, 0 };
    static cilist io___9 = { 0, 6, 0, 0, 0 };
    static cilist io___10 = { 0, 6, 0, 0, 0 };
    static cilist io___11 = { 0, 6, 0, 0, 0 };
    static cilist io___12 = { 0, 6, 0, 0, 0 };
    static cilist io___13 = { 0, 6, 0, 0, 0 };
    static cilist io___14 = { 0, 6, 0, 0, 0 };
    static cilist io___15 = { 0, 6, 0, 0, 0 };
    static cilist io___16 = { 0, 6, 0, 0, 0 };
    static cilist io___17 = { 0, 6, 0, 0, 0 };
    static cilist io___18 = { 0, 6, 0, 0, 0 };
    static cilist io___19 = { 0, 6, 0, 0, 0 };
    static cilist io___20 = { 0, 6, 0, 0, 0 };
    static cilist io___21 = { 0, 5, 0, fmt_852, 0 };
    static cilist io___22 = { 0, 6, 0, 0, 0 };
    static cilist io___23 = { 0, 6, 0, 0, 0 };
    static cilist io___24 = { 0, 6, 0, 0, 0 };
    static cilist io___25 = { 0, 6, 0, 0, 0 };
    static cilist io___26 = { 0, 6, 0, 0, 0 };
    static cilist io___27 = { 0, 6, 0, 0, 0 };
    static cilist io___28 = { 0, 6, 0, 0, 0 };
    static cilist io___29 = { 0, 6, 0, 0, 0 };
    static cilist io___30 = { 0, 6, 0, 0, 0 };
    static cilist io___31 = { 0, 6, 0, 0, 0 };
    static cilist io___32 = { 0, 6, 0, 0, 0 };
    static cilist io___33 = { 0, 6, 0, 0, 0 };
    static cilist io___34 = { 0, 6, 0, 0, 0 };
    static cilist io___35 = { 0, 6, 0, 0, 0 };
    static cilist io___36 = { 0, 6, 0, 0, 0 };
    static cilist io___37 = { 0, 6, 0, 0, 0 };
    static cilist io___38 = { 0, 5, 0, fmt_818, 0 };
    static cilist io___40 = { 0, 6, 0, 0, 0 };
    static cilist io___41 = { 0, 5, 0, fmt_818, 0 };
    static cilist io___43 = { 0, 6, 0, 0, 0 };
    static cilist io___46 = { 0, 1, 0, fmt_21, 0 };
    static cilist io___48 = { 0, 2, 0, 0, 0 };
    static cilist io___49 = { 0, 6, 0, fmt_23, 0 };


    s_wsle(&io___1);
    do_lio(&c__9, &c__1, "  This program reads an ascii file of 32-bit integ\
ers", 53L);
    e_wsle();
    s_wsle(&io___2);
    do_lio(&c__9, &c__1, "  and converts it to a binary file for use in DIEH\
ARD.", 54L);
    e_wsle();
    s_wsle(&io___3);
    do_lio(&c__9, &c__1, "      ", 6L);
    e_wsle();
    s_wsle(&io___4);
    do_lio(&c__9, &c__1, "  To continue, hit space ret", 28L);
    e_wsle();
    s_rsfe(&io___5);
    do_fio(&c__1, dum, 1L);
    e_rsfe();
    s_wsle(&io___7);
    do_lio(&c__9, &c__1, "  You must first create the ascii file.  To do tha\
t,", 52L);
    e_wsle();
    s_wsle(&io___8);
    do_lio(&c__9, &c__1, " generate your 32-bit integers and write them to a\
 file,", 56L);
    e_wsle();
    s_wsle(&io___9);
    do_lio(&c__9, &c__1, " in hex format, 80 characters (ten 32-bit integers\
) per", 55L);
    e_wsle();
    s_wsle(&io___10);
    do_lio(&c__9, &c__1, " line.  For example, in Fortran, if your array is,\
 say,", 55L);
    e_wsle();
    s_wsle(&io___11);
    do_lio(&c__9, &c__1, " mran(5000), then the statements", 32L);
    e_wsle();
    s_wsle(&io___12);
    do_lio(&c__9, &c__1, "            write(1,21) mran  ", 30L);
    e_wsle();
    s_wsle(&io___13);
    do_lio(&c__9, &c__1, "     21     format(10z8)", 24L);
    e_wsle();
    s_wsle(&io___14);
    do_lio(&c__9, &c__1, " will cause your 5000 integers to be written to the"
	    , 51L);
    e_wsle();
    s_wsle(&io___15);
    do_lio(&c__9, &c__1, " file designated unit 1.   Of course you must have\
 first", 56L);
    e_wsle();
    s_wsle(&io___16);
    do_lio(&c__9, &c__1, " opened that unit with a statement such as", 42L);
    e_wsle();
    s_wsle(&io___17);
    do_lio(&c__9, &c__1, "             open(1,file='whatever')", 36L);
    e_wsle();
    s_wsle(&io___18);
    do_lio(&c__9, &c__1, "      ", 6L);
    e_wsle();
    s_wsle(&io___19);
    do_lio(&c__9, &c__1, "  You must first create the ascii file.  To do tha\
t,", 52L);
    e_wsle();
    s_wsle(&io___20);
    do_lio(&c__9, &c__1, "  To continue, hit space ret", 28L);
    e_wsle();
    s_rsfe(&io___21);
    do_fio(&c__1, dum, 1L);
    e_rsfe();
    s_wsle(&io___22);
    do_lio(&c__9, &c__1, "  Since DIEHARD expects BIG files, you will get few"
	    , 51L);
    e_wsle();
    s_wsle(&io___23);
    do_lio(&c__9, &c__1, "  results from a file of a mere 5000 integers.  Yo\
u are", 55L);
    e_wsle();
    s_wsle(&io___24);
    do_lio(&c__9, &c__1, "  presumably creating a file of random numbers to \
test,", 55L);
    e_wsle();
    s_wsle(&io___25);
    do_lio(&c__9, &c__1, "  and you need about 2.9 million for DIEHARD.  Thi\
s may", 55L);
    e_wsle();
    s_wsle(&io___26);
    do_lio(&c__9, &c__1, "  be done with a double loop.  A (Fortran) program\
 with", 55L);
    e_wsle();
    s_wsle(&io___27);
    do_lio(&c__9, &c__1, "  this structure would do it:", 29L);
    e_wsle();
    s_wsle(&io___28);
    do_lio(&c__9, &c__1, "             integer*4 m(4096)      ", 36L);
    e_wsle();
    s_wsle(&io___29);
    do_lio(&c__9, &c__1, "             open(1,file='ascfile') ", 36L);
    e_wsle();
    s_wsle(&io___30);
    do_lio(&c__9, &c__1, "             do 2 i=1,700           ", 36L);
    e_wsle();
    s_wsle(&io___31);
    do_lio(&c__9, &c__1, "                do 3 j=1,4096       ", 36L);
    e_wsle();
    s_wsle(&io___32);
    do_lio(&c__9, &c__1, "      3         m(j)=NEXTRANDOM32BIT     ", 41L);
    e_wsle();
    s_wsle(&io___33);
    do_lio(&c__9, &c__1, "      2      write(1,21)            ", 36L);
    e_wsle();
    s_wsle(&io___34);
    do_lio(&c__9, &c__1, "      21     format(10z8)           ", 36L);
    e_wsle();
    s_wsle(&io___35);
    do_lio(&c__9, &c__1, "             end                    ", 36L);
    e_wsle();
    s_wsle(&io___36);
    do_lio(&c__9, &c__1, "  OK, I assume you have created your ascii file. ", 
	    49L);
    e_wsle();
    s_wsle(&io___37);
    do_lio(&c__9, &c__1, "  Now enter the name of that file (<=15 characters\
):", 52L);
    e_wsle();
    s_rsfe(&io___38);
    do_fio(&c__1, ascfile, 15L);
    e_rsfe();
    o__1.oerr = 0;
    o__1.ounit = 1;
    o__1.ofnmlen = 15;
    o__1.ofnm = ascfile;
    o__1.orl = 0;
    o__1.osta = 0;
    o__1.oacc = 0;
    o__1.ofm = 0;
    o__1.oblnk = 0;
    f_open(&o__1);
    s_wsle(&io___40);
    do_lio(&c__9, &c__1, "  Next, enter the name of your binary file:", 43L);
    e_wsle();
    s_rsfe(&io___41);
    do_fio(&c__1, binfile, 15L);
    e_rsfe();
    o__1.oerr = 0;
    o__1.ounit = 2;
    o__1.ofnmlen = 15;
    o__1.ofnm = binfile;
    o__1.orl = 16384;
    o__1.osta = 0;
    o__1.oacc = "direct";
    o__1.ofm = "unformatted";
    o__1.oblnk = 0;
    f_open(&o__1);
    s_wsle(&io___43);
    do_lio(&c__9, &c__1, "   Please wait...........", 25L);
    e_wsle();
    jk = 0;
    for (i = 1; i <= 700; ++i) {
	s_rsfe(&io___46);
	do_fio(&c__4096, (char *)&m[0], (ftnlen)sizeof(integer));
	e_rsfe();
	++jk;
/* L2: */
	io___48.cirec = jk;
	s_wdue(&io___48);
	do_uio(&c__4096, (char *)&m[0], (ftnlen)sizeof(integer));
	e_wdue();
    }
    s_wsfe(&io___49);
    do_fio(&c__1, binfile, 15L);
    e_wsfe();
/* L5: */
    cl__1.cerr = 0;
    cl__1.cunit = 2;
    cl__1.csta = 0;
    f_clos(&cl__1);
} /* MAIN__ */

