/*#include "libc.h"
#include "math.h"
*/

#include "ftoc.h"

/*
routines for linear systems processing via LU factorization
from Handbook of C tools for Scientists and engineers by L. Baker

PURPOSE:

SOLVE LINEAR SYSTEM GIVEN FACTORIZATION OF A MATRIX (in A)

CONTENTS:
backsub(a,coln,n,pivot,b) 
	solves system  ax=b. assumes a contains LU factors, pivot
	pivoting information. matrix stored as a[m][coln] with the
	system to be solved nxn. Answer vector returned in b.

backt(a,coln,n,pivot,b)  As above, but will return the solution
	to a[T]x=b where a[T] is the transpose of matrix a.   This is
	useful in inverse iteration for eigenvalue determination if the
	matrix a is non-symmetric, and in determining matrix condition
	number.

DEPENDENCIES:
requires header file ftoc.h and routines in lus.c 


*/


backsub(a,coln,n,pivot,b)
int coln,n,*pivot;
float *a,*b;
{
    float t;
    int k,l,nm1;
    nm1=n-1;

    /* solve ly=b first*/
    DOFOR(k,nm1)
    {
        l=pivot[k];
        t=b[l];
        if(l!=k)
        {
            b [l]=b [k];
            b [k]= t;
        }
        saxpy( nm1-k,t, &(a INDEX(k+1,k)),coln,&(b[k+1]),1);
    }

    /* solve Ux=y*/
    DOFOR(l,n)
    {
        k=nm1-l;
        b [k]= b [k]/ a INDEX(k,k);
        t=-b [k];
        saxpy(k,t,&(a INDEX(0,k)),coln,b,1);
    }

    return;
}

backt(a,coln,n,pivot,b)
int coln,n,*pivot;
float *a,*b;
{/* like backsub, except solves A(T)x=b */
    float t,sdot();
    int k,l,nm1;
    nm1=n-1;

    /* solve u(T)y=b first*/
    DOFOR(k,n)
    {
        t=sdot(k,&(a INDEX(0,k)),coln,b,1);
printf(" k= %d t=%f\n",k,t);
        b[k]=(b[k]-t)/a INDEX(k,k);	
    }

    /* solve l(T)x=y*/
    if(nm1<1)return;
    for(k=nm1-1;k>=0;k--)
    {
        b[k]+=sdot(nm1-k,&(a INDEX(k+1,k)),coln,&(b[k+1]),1);
        l=pivot[k];
        if(l!=k)
        {
            t=b[l];
            b[l]=b[k];
            b[k]=t;
        }
    }

    return;
}
