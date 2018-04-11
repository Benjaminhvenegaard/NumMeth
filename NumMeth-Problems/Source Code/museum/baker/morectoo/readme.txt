More C Tools for Scientists and Engineers Source Disk 
	Disk contents
Copyright 1988 Louis Baker

Limits of Liability and Disclaimer of Warranty

     The author and publisher have exercised care in preparing this 
book and the programs contained in it. They make no representation, 
however, that the programs are error-free or suitable for every application 
to which a reader may attempt to apply them. The author and publisher 
make no warranty of any kind, expressed or implied, including the 
warranties of merchantability or fitness for a particular purpose, 
with regard to these programs or the documentation or theory contained 
in this book, all of whcih are provided "as is."  The author and publisher 
shall not be liable for damages in an amount greater than the purchase 
price of this book, or in any event for incidental or consequential  damages 
in connection with, or arising out of, the furnishing, performance, 
or use of these programs or the associated descriptions or discussions.  
     Readers should test any program on their own systems and compare 
results with those presented in this book.  They should then construct 
their own test programs to verify that they fully understand the requisite 
calling conventions and data formats for each of the programs.  Then 
they should test the specific application thoroughly.     

					-------------------------

Remarks

     The programs contained in this package were developed for the 
TURBO C system; many were tested on other compilers.  Please read 
the text for a discussion of conversion to other systems. 

     The statistical package, STATH.C, has been upgraded. The
error function erf(), which was valid for x >= 0, is now valid
for negative x, using erf(-x)=-erf(x).  Functions for the sign test
critical value, P and Q of Abramowitz and Stegun (P= cump(), Q= 1-P )
are now included along with the bivariate Normal distribution L and
the derivatives of the normal probability density Z(n)[x].

     A constant in u32() and two in u16() in RANDOM.C were corrected.

     An improved COMPLEX.H is included.

     The source code for the programs contained herein are available 
on an IBM-PC compatible double-sided diskette by mail order. Send 
$14.95 (New Mexico residents add applicable tax, 5.25%) for source 
code in C to:
          
          Dagonet Software
          2904 La Veta Dr. N.E.
          Albuquerque NM 87110


If you run into any problems, the first thing to look for is a
problem with the library functions.  Are they really the same?
Are any missing?
