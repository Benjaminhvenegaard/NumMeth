/* matrix & vector processing routines
(from "C Tools for Scientists and Engineers" by L. Baker)

CONTENTS:
	vdif(a,b,dif,n)			
		vector difference of two vectors a and b
	swaprow(a,from,to,coln)	swap rows from and to of matrix a
	swapcol(a,from,to,coln) swap columns  coln=# of columns

The following two routines are similar to mv()and mvt()
	in VECTOR.C  Here, we process the first n columns of a
	matrix of coln columns.  Previously, it was assumed n=coln. Note
	that there is one additional argument, the last.

	mvc						multiply vector by matrix
	mvct					multipy vector by transpose of matrix
	mcopy(a,aa,coln,nrow,ncol)	
		copy the upper left-hand portion of one matrix, a,to another (aa).
		nrow rows and ncol columns are copied.It is assumed that 
		both a and aa are  dimensioned to have coln columns.

DEPENDENCIES:
ftoc.h
*/

#include <ftoc.h>

vdif(a,b,dif,n) int n; float a[],b[],dif[];
{
int i;
DOFOR(i,n) dif[i]=a[i]-b[i];
return;
}

swaprow(a,from,to,coln) int from,to,coln; float a[];
{
int i;
float x;
DOFOR(i,coln)
	{
	x= a INDEX(from,i);
	a INDEX(from,i)= a INDEX(to,i);
	a INDEX(to,i)=x;
	}
return;
}

swapcol(a,from,to,coln) int from,to,coln; float a[];
{/*matrix assumed to be square=>number of rows=coln*/
int i;
float x;
DOFOR(i,coln)
	{
	x= a INDEX(i,from);
	a INDEX(i,from)= a INDEX(i,to);
	a INDEX(i,to)=x;
	}
return;
}

mvc(a,x,y,n,m,coln) int n,coln,m;
float x[],y[],a[];
/* y=ax a(m,n) */
{
int i,j,k;
float sum;
DOFOR(i,m)
	{
	sum=0.;
	DOFOR(j,n) sum+= x[j]*a INDEX(i,j);
	y[i]=sum;
	}
return;
}

mvtc(a,x,y,n,m,coln) int coln,n,m;
float a[],x[],y[];
{
/* y= a(T)x  a(n,m)*/
float sum;
int i,j;
DOFOR(i,m)
	{
	sum=0.;
	DOFOR(j,n)sum+= x[j]*a INDEX(j,i);
	y[i]=sum;
	}
return;
}

mcopy(a,aa,coln,nrow,ncol) float a[],aa[];
int coln,nrow,ncol;
/* copy upper left part of one matrix to another.
assumed matrices are float, and have same number of columns*/
{
int i,j;
DOFOR(i,nrow)
	{
	DOFOR(j,ncol) aa INDEX(i,j)=a INDEX(i,j);
	}
return;
}
