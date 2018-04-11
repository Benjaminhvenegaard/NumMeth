/* perform robust least-squares regession using SVD
note- uses double-precision storage- DO NOT mix with routines
from vector.c or matrix.c, as these assume vectors to be of
type float.
(from "C Tools for Scientists and Engineers" by L. Baker)

CONTENTS:
lstsq() perform lst-sq. solution Az=y 
		A dimensioned for coln columns 
		  neq equations (rows)
		  col columns in use (variables)
		  other variables working storage

type double routines similar to those in vector.c and matrix.c:
pvd()
setrow()
vdifd()
mvd()
mvtd()

DEPENDENCIES:
svd() in svd.c required.
*/
/*
#include "libc.h"
#include "math.h"
*/
#define INDEX(i,j) [j+(i)*coln]
#define DOFOR(i,to) for(i=0;i<to;i++)
#define min(a,b) ((a)<(b)?(a):(b))

lstsq(a,y,coln,neq,col,u,s,v,z,resid,work) 
int neq,col,coln;
double a[],y[];
double u[],v[],work[],s[],z[],resid[];
{
int matu,matv,ierr,i;
matu=1;
matv=1;

svd(coln,neq,col,a,s,matu,u,matv,v,&ierr,work);
printf(" ierr=%d\n singular values=\n",ierr);
pvd(s,col);
printmd(u,coln,neq,col,neq);
printmd(v,coln,neq,col,col);

/* work d=U[t]*y*/
mvtd(u,y,work,col,neq,coln);

DOFOR(i,col)
	{
	if( s[i]>0.) work[i]/=s[i];
	}
mvd(v,work,z,col,col,coln);	
printf(" answer is:\n");
pvd(z,col);
printf(" residual errors for each eqn:\n");
mvd(a,z,work,col,neq,coln);
vdifd(work,y,work,neq);
pvd(work,neq);return;
}

setrow(a,coln,row,x) int row,coln; double x,a[];
{
a INDEX(row,0)= 1.;
a INDEX(row,1)= x;
a INDEX(row,2)= x*x;
return;
}

main(argc,argv) int argc; char **argv;
{
/* test pblm lst sq.*/
static double y[10]={1.1,1.3,2.0,2.1,2.7,2.8,3.4,3.6,4.1,4.0};
int i,j,k,coln,rown,ierr;
double x,a[30],z[3],s[10],resid[10],work[30],u[30],v[30];
 
static double aa[5][3]={1.,6.,11.,2.,7.,12.,3.,8.,13.,4.,9.,14.,5.,10.,15.};

static double aaa[3][3]={0.,.812999,-.582265,0.,-.097757 ,-.995210,0.,0.,0.};
printf(" test matrix (GOLUB-REINSCH BUG-NOT IN C TOOLS BOOK-SEE README FILE):");
printmd(aaa,3,3,3,3);
svd(3,3,3,aaa,s,1,u,1,v,&ierr,resid);
printf(" ierr=%d\n" ,ierr);
DOFOR(ierr,3)printf(" sing value=%f\n",s[ierr]);
printf("u matrix:");
printmd(u,3,3,3,3);
printf("v matrix:");
printmd(v,3,3,3,3);
svd(3,5,3,aa,s,1,u,1,v,&ierr,resid);
printf(" ierr=%d\n" ,ierr);
DOFOR(ierr,3)printf(" sing value=%f\n",s[ierr]);
printf("u matrix:");
printmd(u,3,5,3,5);
printf("v matrix:");
printmd(v,3,3,3,3);

svd(3,5,3,aa,s,-1,u,1,v,&ierr,resid);
printf(" ierr=%d\n" ,ierr);
DOFOR(ierr,3)printf(" sing value=%f:\n",s[ierr]);
printf("u matrix:");
printmd(u,5,5,5,5);
printf("v matrix:");
printmd(v,3,3,3,3);


coln=3;
rown=10;
DOFOR(i,5)
	{
	x= (i-2);
	DOFOR(j,2)
		{
		k= j+2*i;
		setrow(a,coln,k,x);
		}
	}
printf("coef. matrix is :\n");
printmd(a,coln,rown,coln,rown);
printf(" rhs is\n");
pvd(y,rown);
lstsq(a,y,coln,rown, coln,u,s,v,z,resid,work );
exit(0);
}

pvd(v,n) int n;double v[];
{
int btm,top,i;
btm=0;
top=0;
while (btm<n)
	{
	top=min(btm+6,n);
	printf(" printing vector from %d to %d\n",btm,(top-1));
	for(i=btm;i<top;i++)printf(" %e",v[i]);
	printf("\n");
	btm+=6;
	}
return;
}

mvd(a,x,y,n,m,coln) int n,coln,m;
double x[],y[],a[];
/* y=ax a(m,n) */
{
int i,j;
double sum;
DOFOR(i,m)
	{
	sum=0.;
	DOFOR(j,n) sum+= x[j]*a INDEX(i,j);
	y[i]=sum;
	}
return;
}
mvtd(a,x,y,n,m,coln) int n,coln,m;
double x[],y[],a[];
/* y=a[t]x a(m,n) */
{
int i,j;
double sum;
DOFOR(i,n)
	{
	sum=0.;
	DOFOR(j,m) sum+= x[j]*a INDEX(j,i);
	y[i]=sum;
	}
return;
}


vdifd(x,y,z,n) int n; double x[],y[],z[];
/*z=x-y*/
{
int i;
DOFOR(i,n)z[i]=x[i]-y[i];
return;
}


