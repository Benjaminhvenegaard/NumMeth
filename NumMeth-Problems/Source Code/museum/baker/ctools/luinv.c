/*#include "libc.h"
#include "math.h"
*/

#include "ftoc.h"

/*
routines for linear systems processing via LU factorization
from Handbook of C tools for Scientists and engineers by L. Baker

PURPOSE: INVERT A MATRIX GIVEN LU FACTORIZATION OF SAME

invm(a,coln,n,pivot,work)
	a contains LU factors of a matrix. a[m][coln], n is size of
	the actual matrix to be inverted.  pivot has pivot information.
	work is a type float work array of size n.

DEPENDENCIES:
	requires header ftoc.h and routines in LUS.C

*/

invm(a,coln,n,pivot,work)
int coln,n,*pivot;
float *a,*work;
{
    float t,ten;
    int i,j,k,l,kb,kp1,nm1;
    nm1=n-1;
/* no det calc.*/
/* inverse u*/
    DOFOR(k,n)
    {
        a INDEX(k,k)=t=1./ a INDEX(k,k);
        t= -t;
        sscal(k,t,&(a INDEX(0,k)),coln);
        kp1=k+1;
        if (nm1>=kp1)
        {
            DOBYYY(j,kp1,n)
            {
                t=a INDEX(k,j);
                a INDEX(k,j)=0.0;
                saxpy(k+1,t,&(a INDEX(0,k)),coln,&(a INDEX(0,j)), coln);
            }
        }

    }
/*inv(u)*inv(l)*/
    if (nm1>=1)
    {
        DOFOR(kb,nm1)
        {
            k=nm1-kb-1;
            kp1=k+1;
            DOBYYY(i,kp1,n)
            {
                work [i]=a INDEX(i,k);
                a INDEX(i,k)=0.0;
            }
            DOBYYY(j,kp1,n)
            {
                t=work [j];
                saxpy(n,t,&(a INDEX(0,j)),coln,&(a INDEX(0,k)),coln);
            }
            l=pivot [k];
            if(l!=k) sswap(n,&(a INDEX(0,k)),coln,&(a INDEX(0,l)),coln);
        }
    }
    return;
}
