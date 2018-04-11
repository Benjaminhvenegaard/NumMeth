/* package to implement complex arithmetic in C
	                    muller's root solver for
	                    	multiple complex roots 

from Handbook of C tools for scientists and engineers by L. Baker

*/
#include "complex.h"
/*
#include "libc.h"
#include "math.h"
*/
struct complex 	ci,c1,c0,o,o2,ir;

int iterp;/* global to return count used*/

/*
#define min(a,b) (((a)<(b))? (a): (b))
*/
#define max(a,b) (((a)>(b))? (a): (b))
#define abs(x) ((x)?  (x):-(x))

double argmt(y,x)
double x,y;
{/* returns answer between 0 and twopi, as needed in
complex principal argument*/
/* caveat- Aztec C returns 0,not + or -halfpi if x=0., y nonzero*/
double ans,ratio,twopi=6.283185307,pi=3.14159265358979,halfpi=1.570796327;
double atan(),undef=0.0;/* change if desired*/

if (x==0.){if(y>0.) return(halfpi);
	   if(y<0.) return(pi+halfpi);
	   return(undef);
	  };
/* if -pi to pi: return (halfpi*sign(y));*/
ratio=(y/x);ans=atan(ratio);
/*atan returns answer between -halfpi and halfpi*/
/* now move to correct quadrant  between -pi and pi*/
if (ratio>0.){/* ratio, ans>0. */
	      if (x>0.) return(ans);/* quadrant I*/
		/* else x<0.,y<0. quadrant III*/
		return (pi+ans);
              };
/* else ratio,ans<0.*/
if(x>0.)return(twopi+ans);/*quadrant IV*/
/*else x<0.,y>0., quadrant II*/
return(pi+ans);

/* if answer bwtn -pi and pi desired: if ans<=pi accept ans unchanged
	else   ans-twopi
   this will affect quadrant III,IV only. change:
   	III: pi+ans to ans-pi
   	 IV: twopi+ans to ans
*/
}


/*------------- complex logarithm function ---------------*/

clog(x,ans) struct complex *x,*ans;
{
double r,argmt(),sqrt(),log(),angle;
r= sqrt( CNORM(*x) );
angle=argmt(x->y,x->x);
ans->x=log(r);
ans->y=angle;
return;
}

/* ----------------- complex square root --------------------*/

csqrt(z,ans) struct complex *z,*ans;
{
double x,y,r,sqrt(),argmt(),angle;
r=  sqrt(sqrt( CNORM(*z) ) );
angle=.5*argmt(z->y,z->x);
polarxy(r,angle,&x,&y);
ans->x=x;
ans->y=y;
return;
}

/* ------- convert from polar to rectangular coordinates---- */

polarxy(r,angle,x,y) double r,angle,*x,*y;
{double sin(),cos();
*x=r*cos(angle);
*y=r*sin(angle);
return;
}

main (argc,argv) int argc; char **argv;
{
double ep1,ep2;
struct complex rts[12],delta;
int i, maxit=20,kn,n,fnreal, fn();
kn=0;n=3;fnreal=0;
ep1=1.e-8;
ep2=1.e-8;
delta.x=.5;delta.y=0.;
for(i=0;i<n;i++){
		rts[i].x=0.;
		rts[i].y=0.;
		};
printf(" in main ");printc( &(rts[0]));printc(rts);printf("\n");
muller(kn,n,rts,maxit,ep1,ep2,fn,fnreal,&delta);
for(i=0;i<n;i++){printf(" root=");
		printc(&(rts[i]));
		printf("\n");
		};


exit(0);
}

/*----------- test function to find zeros (roots) of --------*/

fn(x,ans) struct complex *x,*ans;
{
/* x^3-x-1. */
struct complex cdum,cdu1,cdu2;
/*printf(" in fn ");printc(x);*/
CLET(cdu1,*x);
CMULT(cdu2,cdu1,cdu1);
CMULT(cdum,cdu1,cdu2);
CSUB(cdum,cdum,cdu1);cdum.x=cdum.x-1.;
CLET(*ans,cdum);
/*printc(ans);printf("\n");*/
return;
}


printc(x) struct complex *x;
{
printf("%f %f",x->x,x->y);
return;
}

/*------------------ muller's method
	based on FORTRAN program of Conte and DeBoor-----------*/

muller(kn,n,rts,maxit,ep1,ep2,fn,fnreal,delt)
int fnreal,n,kn,maxit;
double ep1,ep2; int (*fn)();
struct complex *delt,rts[];
{
double sqrt(),eps1,eps2,real;
int i,j,kount,ibeg,iend,zerodiv,setfr();
struct complex rt,h,delfpr,frtdef,lambda,delta,
cdum,cdu1,delf,dfprlm,num,den,g,sqr,frt,frtprv,*root;

struct complex *kptr,kludge;
/* tolerences*/
eps1= max(ep1,1.e-12);
eps2= max(ep2,1.e-20);
/* skip any previously found roots*/
ibeg=kn;
iend=kn+n;
CASSN(delta,delt);

/*printf(" muller ");printc(delt);printc(&delta);
printf("\n");
for(i=0;i<n;i++)
	{printf(" roots=");
	root= &(rts[i]);printc(root);printf("rootptr %u",root);
	printf(" \n");
	};
*/
for(i=0;i<iend;i++)
	{
	kount=3;
	/*initial guesses: guess, guess+ and -delta*/
	start: CLET(h,delta);
	root=&(rts[i]);
	/*printf(" rootptr %u",root);printc(root);*/
	CADD(rt, *root,h);
	/*printc(&rt);printf("\n");*/
	setfr(&delfpr,fn,&rt,i,rts,eps2,&zerodiv,&delta);
	if(zerodiv)goto start;
	CSUB(rt,rt,h);
	CSUB(rt,rt,h);
	setfr(&frtprv,fn,&rt,i,rts,eps2,&zerodiv,&delta);
	if(zerodiv)goto start;
	CADD(rt,rt,h);
	setfr(&frtdef,fn,&rt,i,rts,eps2,&zerodiv,&delta);
	if(zerodiv)goto start;
	lambda.x=-.5;lambda.y=0.;

	while(1)
	{
	/* next estimate for root*/
	CSUB(delf,frtdef,frtprv);
	CMULT(dfprlm,delfpr,lambda);
	CLET(cdum,lambda);cdum.x=cdum.x+1.;
	CTREAL(cdum,cdum,(-2.));
	CMULT(num,frtdef,cdum);
	CTREAL(cdum,lambda,2.);
	cdum.x=cdum.x+1.;
	CMULT(cdu1,cdum,delf);
	CMULT(cdum,lambda,dfprlm);
	CSUB(g,cdu1,cdum);
	CSUB(cdum,delf,dfprlm);
	CTREAL(cdum,cdum,2.);
	CMULT(cdu1,cdum,num);
	CMULT(cdum,cdu1,lambda);
	CMULT(sqr,g,g);
	CADD(sqr,sqr,cdum);
	if( fnreal && sqr.x<0.) {sqr.x=0.;sqr.y=0.;};
	csqrt(&sqr,&sqr);
	CADD(den,sqr,g);
	real= g.x*sqr.x+g.y*sqr.y;
	if (real<0.)
		{CSUB(den,g,sqr);};
	if( cabs(den)==0.) {den.x=1.;den.y=0.;};
	CDIV(lambda,num,den);
	CLET(frtprv,frtdef);
	CLET(delfpr,delf);
	CMULT(cdum,h,lambda);
	CLET(h,cdum);
	CADD(rt,rt,h);
	if(kount>maxit) goto fini;
	while(1)
		{
		kount++;
		setfr(&frtdef,fn,&rt,i,rts,eps2,&zerodiv,&delta);
		if(zerodiv)goto start;
		if( cabs(h)<eps1*cabs(rt) || cabs(frtdef)<eps2) goto fini;
		/*unconverged to desired tolerance*/
		if( cabs(frtdef)<10.*cabs(frtprv)) break; /* compute next estimate*/
		/* diverged cut step size */
		printf(" cutting stepsize\n");
		CTREAL(h,h,.5);
		CTREAL(lambda,lambda,.5);
		CSUB(rt,rt,h);
		};/* infinite while loop-unconverged*/
	};/* infinite while loop- next estimate for root*/
	fini:;root=&(rts[i]);
	/*printf(" befor fini ");printc(&(rts[i]));printc(&(rts[i+1]));
	printf("\n");printf(" %u\n",root);*/
	rts[i].x=rt.x; rts[i].y=rt.y;
}/* loop over desired roots*/
return;
}

setfr(frtdef,fn,rt,i,rts,eps2,zerodiv,delta) int *zerodiv,i;double eps2;
struct complex *rt,*frtdef,rts[],*delta;int (*fn)();
{int j;
double sqrt();
struct complex *root,den,cdum;
*zerodiv=0;
/*printf(" setfr ");printc(rt);printf("\n");*/
(*fn)(rt,frtdef);
/*printf(" in setfr ");printc(rt);printc(frtdef);printf("\n");*/
if(i>0)
	{/*deflate for found roots*/
	for (j=0;j<i;j++)
		{root=&(rts[j]);
		CSUB(den,*rt,*root);
		/*printf(" deflating for root");
		printc( root);printf("\n");*/
		if(cabs(den)<eps2){CTREAL(cdum,*delta,.1);
				    root=&(rts[i]);
				    CADD(cdum,cdum,*root);
				    CLET(*root,cdum);
				    *zerodiv=1;
printf(" zero denom. in setfr\n");
				    return;
				   }
		 CDIV(cdum,*frtdef,den);
		 CLET(*frtdef,cdum);
		}
	};/*i>0*/
/*printf(" return from setfr\n");*/
return;
}

/*----------- complex exponential function--------------*/

cexp( x,ans) struct complex *x,*ans;
{
double cos(),sin(),y,z;
struct complex c1,c2;
y = exp ( x->x);
c2.x= cos (x->y);
c2.y= sin (x->y);
CTREAL(c1,c2,y);
/*printf(" cexp ");printc(&c1);printc(x);printf("\n");*/
CLET(*ans,c1);
return;
}
