/* routines to solve block matrix problems

from More C tools for scientists and engineers by L. Baker

CONTENTS:

bs() 	solves a block-bidiagonal matrix
msub() 	subtracts two matrices to yeid a third
invert() uses the lufact() and invm() routines to invert a matrix.
		the original matrix is left unaltered. 
mm()	move a matrix
mmult()	multiply two matrice to form a third. 
mmultg 	generalized version of mmult. dimensioning of 
		matrix storage can differ from actual number of columns
mset	set a matrix to a value (usually used to zero a matrix)
vdif	difference two vectors (also in VECTOR.C)

test problem: a simple two-point boundary value problem with 
second-order differencing.  The harmonic oscillator equation is
solved.


DEPENDENCIES:

invert() uses lufact() and invm() from Chapter 2,
	files LUF.C and LUINV.C to invert a matrix (block)

*/


/*#include "libc.h"
#include "math.h"
*/
#define DOFOR(i,to) for(i=0;i<to;i++)
#define INDEX(i,j) [j+(i)*coln]
#define IND(i,j,k) [j+(i)*k]
/* next is for element matrices such as q,r,bt,bt2,bb2,bb*/
#define IE(i,j)    [j+(i)*n]
/* next is for the main matrices a and b note that 
"natural" C ordering is NOT used  
	it would have been:  [k+(j)*n+n*n*i]
this would have spread the blocks all over the place unless
the first subscript were the block index, with j and k the within block
indices.
*/ 
#define IM(i,j,k)  [ j+(i)*n+(k)*nsq] 
/* to locate submatrix, IM(0,0,k) reduces to: */
#define IN(k) [(k)*nsq]


int bs(b,a,bt,bt2,bb2,bb,c,q,r,pivot,n,np)
int n,np,pivot[];
float a[],b[],c[],bt[],bt2[],bb2[],bb[],q[],r[];
/* assumed matrix problem in form:
	b1 a1 0  ...               c1
	0  b2 a2 0

	.............           =

	.....         bnp anp
	bt bt2 ...    bb2 bb        cb

***************************CAVEAT:*******************
backsweep is simplified by the assumption that cb is c(np+1)
q,r must have dimensions of at least nsq=n*n
*/
{
float *cb,*cp;
float *work;
int nsq,coln,i,index,j,k,info,np1,invert();
np1=np-1;
nsq=n*n;
work=q;
cb=&(c[(np)*n]);
coln=n;

DOFOR(i,np)
	{
	info=invert(&(b IN(i)),pivot,work,n);
if(info!=0)printf(" bomb i=%d\n",i);
	if(info!=0)return(1);
	mmult(bt,&(b IN(i)),r,n,n,n,n);
	mmult(r,&(a IN(i)),q,n,n,n,n);
	msub(bt2,q,bt,n,n,coln);
	mv(r,&(c[i*n]),q,n,n);
	vdif(cb,q,cb,n);
	if(i<(np-3))
		{
		mset(bt2,0.,n,n,coln);
		}
	else
	if(i==(np-3))
		{
		mm(bb2,bt2,n,n,coln);
		}
	else
	if(i==(np-2))
		{
		mm(bb,bt2,n,n,coln);
		}
	}
info=invert(bt,pivot,work,n);
if(info!=0)return(2);
mv(bt,cb,r,n,n);
vcopy(r,cb,n);
/* if cb were not c[np*n] then here would include:
	mv(&(a IN(np1)),cb,r,n,n);
	msub(r,&(c IN(np1),r,n,n,coln);
	mv(&(b IN(np1),r,&(c IM(0,0,np-1),n,n);
AND
	next loop ranges from j=np-2 not np-1 
*/
for(j=np-1;j>=0;j--)
	{
	k=j+1;
	cp=&(c[j*n]) ;
	mv(&(a IN(j)),&(c[k*n]),r,n,n);
	vdif(cp,r,q,n);
	mv(&(b IN(j)) ,q,cp,n,n);
	}
return(0);
}

msub(a,b,c,m,n,coln)
int coln;
float a[],b[],c[];
{
int i,j;
DOFOR(i,n)
	{
	 DOFOR(j,n)c INDEX(i,j) = a INDEX(i,j) - b INDEX(i,j);
	}
return;
}

/*------------------- invert matrix m----------------*/

int invert(m,pivot,work,n)
float m[],work[];
int pivot[],n;
{int info;
lufact(m,n,n,pivot,&info);
if(info!=0)return(info);
invm(m,n,n,pivot,work);
return(0);
}

/*------------ multiply two matrices----------------------*/

mmult(a,b,c,m,n,o,coln) int n,m,o,coln;
float a[],b[],c[];
/* c=a*b matrices actually a(m,n) b(n,o) c(m,o) with coln columns*/
{
int i,j,k;
double sum;
DOFOR(i,m)
	{
	DOFOR(j,o)
		{
		sum=0.;
		DOFOR(k,n)sum+= a INDEX(i,k)* b INDEX(k,j);
		c INDEX(i,j)=sum;
		}
	}
return;
}

/*------------------multiply two matrices-generalized
	the dimensions of the actual matrices do not have to
	be the same as the storage limits, i.e. the actual number
	of columns could be less than the array dimension-----------*/

mmultg(a,b,c,m,n,o,cola,colb,colc) int o,n,m,cola,colb,colc;
float a[],b[],c[];
/* c=a*b matrices actually a(m,n) b(n,o) c(m,o) */
{
int i,j,k;
double sum;
DOFOR(i,m)
	{
	DOFOR(j,o)
		{
		sum=0.;
		DOFOR(k,n)sum+= a IND(i,k,colc)* b IND(k,j,colb);
		c IND(i,j,colc)=sum;
		}
	}
return;
}

/*-------- difference of two vectors--------------------------------*/

vdif(a,b,c,n) int n;
float a[],b[],c[];
{
int i;
DOFOR(i,n)c[i]=a[i]-b[i];
return;
}

/* ------------ set a matrix to value s ----------------------*/

mset(f,s,n,m,coln) int n,m,coln;
float f[],s;
{
int i,j;
DOFOR(i,n)
	{
	DOFOR(j,m) f INDEX(i,j)=s;
	}
return;
}

/* ------------- copy a matrix --------------------*/

mm(from,to,n,m,coln) int n,m,coln;
float to[],from[];
/* matrix move*/
{
int i,j;
DOFOR(i,n)
	{
	DOFOR(j,m) to INDEX(i,j)= from INDEX(i,j);
	}
return;
}

