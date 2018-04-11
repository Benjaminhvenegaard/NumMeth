/* conjugate gradient solver for general matrices

from More C tools for Scientists and Engineers by L. Baker


cg() returns integer count of iterations used. see text for details

main() test driver.  tests solver on both symmetric positive definite
		and asymmetric system.  As system is or rank 2, exact answer
		should be obtained after two iterations, neglecting roundoff.

DEPENDENCIES:
	requires programs in vector.c

*/


#include <ftoc.h>
#define tol 1.e-10

int cg(a,x,y,r,p,ar,ap,n,m,xold)
int n,m;
float a[],x[],y[],r[],p[],ar[],ap[],xold[];
{
float gamma,rnew,alpha,beta,rlen,change;
double dot();
int i,j,k;
vset(x,0.,n);
vcopy(x,xold,n);
/*vcopy(y,r,n);*/
mvt(a,y,r,n,n);/* set r to a[t]*y, not y*/
beta=0.;
vcopy(r,p,n);
pv(x,n);
pv(r,n);
pv(p,n);
k=n<<1;printf(" max iterations allowed %d\n",k);
DOFOR(i,k)
	{
	/* ith iteration*/
	/* for symm case, was mv(a,p,ap,n,n) only*/
		mv(a,p,ar,n,n);
		pv(ar,n);
		mvt(a,ar,ap,n,n);/*new*/
	alpha=dot(p,ap,n);
	rlen=dot(r,r,n);
	if(alpha==0. || rlen==0.) return(i);
	alpha=rlen/alpha;

	/*update*/
	vv(x,x,p,alpha,n);
	gamma=-alpha;
	vv(r,r,ap,gamma,n);
	rnew=dot(r,r,n);
	beta=rnew/rlen;
	if(beta==0.)return(-i);
	vv(p,r,p,beta,n);
	change=0.;
	for(j=0;j<n;j++)change+=(x[j]-xold[j])*(x[j]-xold[j]);
	printf(" change in x %e\n",change);
	if(change<tol)return i;
	vcopy(x,xold,n);
	pv(p,n);
	pv(x,n);
	}
return(n);
}

main(argc,argv) int argc; char **argv;
{
float a[4],p[2],x[2],r[2],y[2],ar[2],ap[2],work[4];
int n,m,cg(),iter,isym,k;
float d,x1,x2;
DOFOR(isym,2)
{/* isym=0 symmetric case =1 asymmetric*/
n=m=2;
y[0]=0.;y[1]=1.;
a[0]=1.;a[3]=4.;a[2]=.5-isym;a[1]=.5;
/*guess*/
x[0]=1.;x[1]=1.;
d=1./(a[0]*a[3]-a[1]*a[2]);
k=n;
x1= d*(y[0]*a[3]-y[1]*a[1]);
x2= d*(y[1]*a[0]-y[0]*a[2]);
printf(" x1= %f x2=%f\n",x1,x2);
iter=cg(a,x,y,r,p,ar,ap,n,m,work);
printf(" iterations=%d\n",iter);
pv(x,n);
}
exit(0);
}


