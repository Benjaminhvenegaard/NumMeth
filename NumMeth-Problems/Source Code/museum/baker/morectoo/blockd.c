/* test driver for routines to solve block matrix problems

test problem: a simple two-point boundary value problem with 
second-order differencing.  The harmonic oscillator equation is
solved.

from More C tools for scientists and engineers by L. Baker

CONTENTS:
	main()

DEPENDENCIES:
	none
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

/*------------------ test driver------------------------*/

main(argc,argv) int argc;char **argv;
{
int pivot[4],info,i,j,k,np,n,nsq,bs();
double x,dx,dxi,v;
double sin(),cos();
float bc;
float bb[4],bb2[4],bt[4],bt2[4],c[24],a[44],b[44],*cb,q[4],r[4];
x=2.*3.1415926535;
np=10;
dx=x/np;
dxi=1./dx;

n=2;
nsq=n*n;
cb=&(c[np*n]);
/*setup problem*/
DOFOR(i,np)
	{
	mset(&(a IN(i)),0.,n,n);
	mset(&(b IN(i)),0.,n,n);
	/* equation 0: y'=z */
	a IM(0,0,i)=dxi;
	b IM(0,0,i)=-dxi;
	a IM(0,1,i)=-.5;
	b IM(0,1,i)=-.5;
	c [ i*n ]=0.;
	/*equation 1: z'=-y*/	
	a IM(1,1,i)=dxi;
	b IM(1,1,i)=-dxi;
	a IM(1,0,i)=.5;
	b IM(1,0,i)=.5;
	c [ i*n +1 ]=0.;
	}
/* boundary conditions*/
mset(bb,0.,n,n,n);
mset(bt,0.,n,n,n);
mset(bb2,0.,n,n,n);
mset(bt2,0.,n,n,n);
/* y=1 at bottom*/
bb[0]=1.;
cb[0]=1.;
/*periodic b.c. z(top)=z(btm) if bb[3]=-1. */
/* z=0. at top if bb[3]=0.*/
bt[3]=1.;
/*printf(" enter coef in coef*z(top)+z(btm)=0. 0,-1 probably\n");
scanf("%f", &bc);*/
bc=0.;
printf(" echoing coef=%f\n",bc);
bb[3]=bc;
cb[1]=0.;
info=bs(b,a,bt,bt2,bb2,bb,c,q,r,pivot,n,np);
/*answer*/
if(info!=0)
	{printf(" info=%d,trouble\n",info);
	exit(0);
	}
printf(" z   z'   \n");
DOFOR(i,np+1)
	{
	j=i*n;
	v= i*dx;
	printf(" %f %f %f %f\n",c[j],cos(v),c[j+1], -sin(v));
	}
exit(0);
}
