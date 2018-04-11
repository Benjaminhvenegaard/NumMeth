/* conjugate gradient solver for symmetric positive definite
systems

from Handbook of C tools for Scientists and Engineers by L. Baker


int cgs() returns integer count of iterations used.

main() is test driver for spd system.  As system is of second order,
		two iterations should produce "exact" answer (neglecting
		roundoff).

DEPENDENCIES:
	requires routines in vector.c
*/

#include <ftoc.h>

int cgs(a,x,y,r,p,ar,ap,n,m,xold)
int n,m;
float a[],x[],y[],r[],p[],ar[],ap[],xold[];
{
    float gamma,rnew,alpha,beta,rlen;
    float resid;
    double dot();
    int i,k; int j;
    vset(x,0.,n);
    vcopy(y,r,n);
    vcopy (x,xold,n);
    beta=0.;
    vcopy(r,p,n);
    pv(x,n);
    pv(r,n);
    pv(p,n);
    k=n<<1;
    DOFOR(i,k)
    {
        /* ith iteration*/
        mv(a,p,ap,n,n);
/*	pv(ap,n);*/
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
/*	pv(p,n);   */

	for(j=0,resid=0.;j<n;j++)
		{resid+=(x[j]-xold[j])*(x[j]-xold[j]);}
printf(" at %d iteration x=%e\n",i,resid);
	if( resid<1.e-5)break;
	vcopy(x,xold,n);
/*	pv(x,n);*/
    }
    return(n);
}

main(argc,argv)
int argc; char **argv;
{
    float a[20],p[20],x[20],r[20],y[20],ar[20],ap[20],work[800];
    int n,m,cgs(),iter,isym,k;
    float d,x1,x2;
    isym=0;
    n=m=2;
    y[0]=0.;y[1]=1.;
    a[0]=1.;a[3]=4.;a[2]=.5-isym;a[1]=.5;
    x[0]=1.;x[1]=1.;
    d=1./(a[0]*a[3]-a[1]*a[2]);
    k=n;
    x1= d*(y[0]*a[3]-y[1]*a[1]);
    x2= d*(y[1]*a[0]-y[0]*a[2]);
    printf(" x1= %f x2=%f\n",x1,x2);
    iter=cgs(a,x,y,r,p,ar,ap,n,m,work);
    printf(" iterations=%d\n",iter);
    pv(x,n);
    exit(0);
}

