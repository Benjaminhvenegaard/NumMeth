/*#include "libc.h"
#include "math.h"
*/

#include "ftoc.h"

/*
routines for linear systems processing via LU factorization
from Handbook of C tools for Scientists and engineers by L. Baker

PERFORM LU FACTORIZATION

lufact(a,coln,n,pivot,info)
	Factor an n by n matrix contained in type float array a 
	which is dimensioned a[m][coln].  pivot is an integer array
	of size n which will contain pivoting information to be used
	by later routines.  info is pointer to an integer which returns 
	0 if all is well, otherwise the row in which problems occured.

DEPENDENCIES:
requires header file ftoc.h and lus.c routines

*/

lufact (a,coln,n,pivot,info)
float *a;
int coln,n,*pivot,*info;
{
    int i,j,k,l,kp1,nm1,last;
    float t;
    *info=0;
    nm1=n-1;
    if (nm1>=1)
    {/*nontrivial pblm*/
        DOFOR(k,nm1)
        {
            kp1=k+1;
            /*partial pivoting ROW exchanges-search over column*/
            /* in FORTRAN, the increment would be 1 not n in ismax call*/
            pivot [k]=l=isamax((n-k),&(a INDEX(k,k)),coln)+k;
            if (a INDEX(l,k)!=0.)
            {/*nonsingular pivot found*/
                if(l!=k)
                {/*interchange needed*/
                    t=a INDEX(l,k);
                    a INDEX(l,k)=a INDEX(k,k);
                    a INDEX(k,k)=t;
                }
                t=-1./a INDEX(k,k);/*scale row*/
                sscal(nm1-k,t,&(a INDEX(k+1,k)),coln);
                DOBYYY(j,kp1,n)
                {
                    t=a INDEX(l,j);
                    if(l!=k)
                    {
                        a INDEX(l,j)=a INDEX(k,j);
                        a INDEX(k,j)=t;
                    }
                    saxpy(nm1-k,t,&(a INDEX(k+1,k)),coln,&(a INDEX(k+1,j)),coln);
                }
            }
                else /*pivot singular*/
                { *info=k;}
            }/*main loop over k*/
        }
        pivot [nm1]=nm1;
        if (a INDEX(nm1,nm1) ==0.0)*info=nm1;
        return;
    }
