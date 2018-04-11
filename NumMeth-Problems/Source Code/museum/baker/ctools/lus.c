/*#include "libc.h"
#include "math.h"
*/

#include "ftoc.h"

/*
routines for linear systems processing via LU factorization
from Handbook of C tools for Scientists and engineers by L. Baker

SUPPORT ROUTINES FOR LU FACTORIZATION

isamax(n,sx,incx)  finds the location of the element
		of greatest absolute value in a vector sx of length n.
		Each incx-th element is examined ( hence, if sx is a
		2-d matrix, may be used to find largest elements in each 
		row or column, depending upon whether incx is 1 or n. 
		

saxpy(n,sa,sx,incx,sy,incy)	
		performs an elementary row operation sy= sy+sa sx where sx 
		and sy	are vectors and sa is a scalar.  Used to subtract 
		a scaled row sx from the sy row.  incx,incy are as in isamax.
		Vectors of length n.

sdot(n,sx,incx,sy,incy)	
		takes the dot product of 2 vectors sx and sy, of length n.

sswap(n,sx,incx,sy,incy)   
		exchanges two vectors sx and sy.  Used for row exchanges
		which occur during pivoting operation.

sscal(n,sa,sx,incx)   scale a vector  sx= sa sx where a is a 
		scalar and sx a vector.

sasum(n,sx,invx) 	function type float which returns the
		sum of the absolute values of the elements of a vector

the above are all based upon BLAS  routines 

printm(a,coln,rown,col,row)
	prints a 2-dimensional matrix a in readable form.
	The actual data of the form  a(row,col) is stored 
	within a matrix dimensioned  a[rown][coln].

*/

int isamax(n,sx,incx)
int n,incx; float *sx;
{int maxi,ix,i;
    float temp,smax;
/*returns 1 less than corresponding FORTRAN version*/
    if (n<=0)return -1;
    if(n==1)return 0;
/* ix=0*/
    maxi=0;
    smax=abs(sx[0]);
    ix=incx;/*ix=ix+incx=incx*/
    DFOR(i,2,n)
    { temp=abs(sx[ix]);
        if (temp>smax)
        {smax=temp;
            maxi=i;
/* return ith element as max,NOT subscript a[ix] ix=i*incx*/
        }
        ix+=incx;
    }
    return maxi;
}

saxpy (n,sa,sx,incx,sy,incy)
int n,incx,incy;
float sa,*sx,*sy;
{/*sy=sa*sx+sy*/
    int i,iy,ix;
    if(n<=0)return;
    if(sa==0.)return;

    iy=ix=0;
    if(incx<0) ix=incx*(1-n);
    if(incy<0) iy=incy*(1-n);
    DOFOR(i,n)
    {
        sy[iy]=sy[iy]+sa*sx[ix];
        iy+=incy;
        ix+=incx;
    }
    return;
}

float sdot(n,sx,incx,sy,incy)
int n,incx,incy;
float *sx,*sy;
{float stemp;
    int i,ix,iy;
    if(n<=0)return(0.);
    ix=iy=0;
    stemp=0.0;
    if(incx<0) ix=incx*(1-n);
    if(incy<0) iy=incy*(1-n);
    DOFOR(i,n)
    {
        stemp+=sy[iy]*sx[ix];
        iy+=incy;
        ix+=incx;
    }
    return stemp;
}

sswap(n,sx,incx,sy,incy)
int n,incx,incy;
float *sx,*sy;
{
    int ix,iy,i;
    float t;
    if(n<=0)return;
    ix=iy=0;
    if(incx<0) ix=incx*(1-n);
    if(incy<0) iy=incy*(1-n);

    DOFOR(i,n)
    {
        t=sx [ix];
        sx [ix]= sy [iy];
        sy [iy]=t;
        ix+=incx;
        iy+=incy;
    }
    return;
}

sscal(n,sa,sx,incx)
int n,incx; float sa,*sx;
{/*scale vector*/
    int i,nincx;

    if (n<=0) return;
    nincx=incx*n;
    DOV(i,nincx,incx)
    sx[i]=sx[i]*sa;
    return;
}

float sasum(n,sx,incx)
float *sx;
int incx,n;
{/* ssum abs values*/
    int i,nincx;
    double stemp;
    stemp=0.0;
    nincx=n*incx;
    if (n<=0)return 0.0;
    DOV(i,n,nincx) stemp=stemp+abs(sx[i]);
    return (stemp);
}


printm(a,coln,rown,col,row) int rown,row,col,coln; float a[];
{
    int i,j,btm,top,count;
    printf("\n");
    btm=top=0;
    while(btm<col)
    {
        top=min(col,(btm+8));
        printf(" printing matrix columns %d to %d\n",btm,(top-1));
        DOFOR(j,row)
        {
            for(i=btm;i<top;i++)
            {
                printf(" %e",a INDEX(j,i));
            }
            printf("\n");
        }
        btm+=8;
    }
    return;
}
