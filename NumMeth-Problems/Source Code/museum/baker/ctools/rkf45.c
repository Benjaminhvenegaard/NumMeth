/*
Runge-Kutta-Fehlberg integrator for
	system of Ordinary Differential Equations
	(Initial Value Problems) non-stiff to mildly stiff
	general order; RKF45 i.e. RK4(5) parameters supplied
(from "C Tools for Scientists and Engineers" by L. Baker)


CONTENTS:

rkf()	
	Runge-Kutta-Fehlberg integrator. General order.
	(see text for qualifications of this statement)

feval()	
	evaluates steps
error()	
	evaluates error term 

sign()	similar to FORTRAN routine for sign transfer
isign()  ditto


DEPENDENCIES:
ftoc.h header file
*/
#include "ftoc.h"
/*#include "math.h"*/
#define iabs(x) (((x)>=0)? (x):-(x))
/* C ordering but with k*/
#define INDEC(i,j,k) [j+(i)*(k)]
/* ------  global variables to remember eps, u26,etc between calls of rkf*/
float u26,eps,crit,criti;
double order,iorder;

/* The constant order is the only scheme dependent parameter
in RKF; other parameters are globals used by error and feval
the functions to evalute the error terms and the intermediate
derivatives.  order should be set to m=n+1, e.g. 5 for RKF45*/

int norder=5;

/* subroutine rkf for general Runge-Kutta-Fehlberg integrations*/

rkf(neq,y,t,tout,relerr,abserr,iflag,work,iwork,f)
int iwork[],neq,*iflag; int (*f)();
float *t,*tout,y[],abserr,relerr,work[];
/* NB that relerr and abserr are float variables,t and tout pointers
		to float variables.
*/
/* iwork must be dimensioned at least 5, work at least 3+7*neq for rkf45*/
/* generally work must be 3+neq*(2+norder) norder=5 for rkf45*/
{
int base,*nfe,*kop,*init,*kflag,*jflag,hfaild,output,k,maxnfe=3000;
int i,mflag,gflag,coln;
float *h,*yp,*ss,s,*ff,*savre,*savae,a,dt,ee
,eeoet,esttol,et,hmin,remin=1.e-12,rer,scale,tol,epsp1,ypk;
/* FORTRAN code had two unused variables: ae and toln */
float sign(),error();
double pow();
/*assign to work-this will enable different ODE's to be solved
simultaneously by passing in different work, iwork vectors*/

coln=neq;
/* set pointers to work areas*/
nfe= iwork;
base=1;
kop= &(iwork[base++]);
init=&(iwork[base++]);
kflag=&(iwork[base++]);
jflag=&(iwork[base++]);
base=neq*(1+norder);
yp=work;
ff=work;
ss=&(work[base]);
base+=neq;
h=&(work[base++]);
savre=&(work[base++]);
savae=&(work[base]);
/* valid input? */
mflag= iabs(*iflag);

if(neq<1 || relerr<0. || abserr<0.
|| mflag==0 || mflag>8 ||((*t==*tout) && (*kflag!=3)))
		{*iflag=8;
		return;}
gflag=0;
if (mflag==1)
	{/*first call set precision limit*/
	eps=1.;
	while(1)
		{
		eps=eps*.5;
		epsp1=eps+1.;
		if(epsp1<= 1.)break;
		}
	u26=26.*eps;
	/*printf(" u26,eps %e %e\n",u26,eps); 8087: eps=5.96e-8*/
/* now set the limits to stepsize change-these depend upon
the parameter order, and are larger for larger orders */
	order=(double) norder;
	iorder= 1./(order);
	crit =pow(9., order);
	criti=pow( (.9/order),order);/* make sure 5 here is order*/
	/* crit= 59059., criti=1.889568e-4 for rkf45*/
	}
else
	{

		  if ( *iflag==3 || (mflag==2 &&(*init==0 || *kflag==3)) )
		  	{gflag=1;goto next;}
		  if(*iflag==4||(*kflag==4&&mflag==2))
		  	{*nfe=0;if(mflag!=2)gflag=1;goto next;};
		  if((*kflag==5 && abserr==0.)||
		  (*kflag==6 && relerr<*savre && abserr<*savae))
		  		{/* unrecoverable serious pblm*/
		  		printf(" iflag=5,6,7,8 not properly handled by user\n");
				*iflag=9;return;
				}
	};
next:
if(gflag){*iflag=*jflag;if(*kflag==3)mflag=iabs(*iflag);}

*jflag=*iflag;
*kflag=0;
*savre=relerr;
*savae=abserr;
rer=2.*eps+remin;
if(relerr<rer)
	{
	relerr=rer;
	*iflag=*kflag=3;
	return;
	};
gflag=0;
dt=*tout-*t;
if(mflag==1)
	{/* initialize*/
	*init=0;
	*kop=0;
	a=*t;
	gflag=1;
	(*f)(a,y,yp);
	*nfe=1;
	if(*t==*tout){*iflag=2;return;}
	};/* mflag==1*/
if(*init==0||gflag)
	{
	*init=1;
	*h=abs(dt);
	/*toln=0.; in FORTRAN version but not used*/
	DOFOR(k,neq)
		{
		tol=relerr*abs(y[k])+abserr;
		if(tol> 0.)
			{
			/*toln=tol; in FORTRAN but not used*/
			ypk=abs(yp[k]);
			if( ypk* pow((double)*h,order)>tol) *h=pow((tol/ypk),iorder);
			}/*if tol */
		}	/*do*/	
	if(tol<=0.)*h=0.;
	ypk=max(abs(dt),abs(*t)) ;
	*h= max(*h, u26*ypk);
	*jflag= isign(2,*iflag);	
	}/* if int ==0*/

*h=sign(*h,dt);
/* too many output requests?*/
if ( abs(*h)>=2.*abs(dt)) (*kop)++;
if(*kop==100){*kop=0;*iflag=7;return;}

if(abs(dt)<= u26*abs(*t))
	{
	DOFOR(k,neq) y[k]+=dt*yp[k];
	a=*tout;
	(*f)(a,y,yp);
	(*nfe)++;
	*t=*tout;
	*iflag=2;
	return;
	}
output=0;
scale=2./relerr;
/*ae=scale*abserr; not used- in FORTRAN*/
while(1) /* infinite loop over steps*/
{
hfaild=0;
hmin=u26*abs(*t);
dt=*tout-*t;
if( abs(dt)<2.*abs(*h))
	{
	if(abs(dt)<=abs(*h)){output=1;*h=dt;}
	else *h=.5*dt;
	}

if(*nfe>maxnfe)
	{
	*iflag=*kflag=4;return;
	};
step:
feval(f,neq,y,*t,*h,ff,ss);
DOFOR(i,neq) ff INDEX(1,i)=ss[i];
(*nfe)+=norder;
/* error ok?*/
eeoet=0.;
DOFOR(k,neq)
	{
	et=abs(y[k])+abs(ff INDEX(1,k) );
	if(et<=0.){*iflag=5;return;}
	ee= abs(error(k,ff,coln));
	eeoet=max(eeoet,ee/et);
	};
esttol=abs(*h)*eeoet*scale;
if(esttol>1.)
	{/*step failed*/
	hfaild=1;
	output=0;
	s=.1;
	if(esttol<crit  )s=.9/pow((double)esttol,iorder);
	*h=s* *h;
	if(abs(*h)>hmin)goto step;
	*iflag=*kflag=6;
	return;
	};
/*successful step*/
(*t)+=*h;
DOFOR(k,neq) y[k]=ff INDEX(1,k);
a=*t;
(*f)(a,y,yp);
(*nfe)++;
s=5.;
/* power is .2= 1/5 for 4th-5th order scheme
crit= 9**(1/power) criti=(.9/5)**(1/power) */
if(esttol> criti ) s=.9/pow((double)esttol,iorder);
if(hfaild)s=min(1.,s);
*h=sign(max(hmin,s*abs(*h)),*h);
if(output) {*t=*tout;*iflag=2;return;};
if(*iflag<=0){*iflag=-2;return;}
};/* end while loop- loop back for next step*/

}

/* coefficients for RKF45 4-5th order scheme*/

  double e[6]={0.0027777777,0.0, -0.0299415,-0.0291998,0.02
  											, 0.036363636};
  double a[6] = { 0. , .25 , .375 , .9230769 , 1. , .5 } ;
  double c[6]={.1157407,.0,.5489278,.5353313,-.2,0.};
  double b[6][5]={0.,0.,0.,0.,0.,.25,0.,0.,0.,0.,
   .09375,.28125,0.,0.,0.,.8793809,-3.2771961,3.3208921,0.,0.,
    2.0324074,-8.,7.1734892,-.2058966,0.,
   -.29629629629,2.,-1.3816764,.4529727,-.275};
/* note that the zeros in b are merely placeholders to simplify indexing
-not used in calculations*/


float error(k,ff,coln)
int k,coln; float *ff;
{
  int i;
  double sum;
sum=0.;
DOFOR(i,(norder+1))sum+= e[i]* ff INDEX(i,k);
return(sum);
}

/* compute terms in RKF steps */

feval(f,neq,y,t,h,ff,s)
int neq; float y[],t,h,ff[],s[];
int (*f)();
{
int i,j,m;
double x;
DFOR(j,2,(norder+1))
	{
	DOFOR(i,neq)
		{x=0.;
		DOFOR(m,j)
			x+=b[j][m]*  ff INDEC(m,i,neq);
		s[i]= x*h+ y[i];
		}
	x=t+a[j]*h;
	(*f)(x,s,&(ff INDEC(j,0,neq)) );
	}
DOFOR(i,neq)
	{
	x=0.;/* note that c[5]=0. */
	DOFOR(j,norder)x+= c[j]* ff INDEC(j,i,neq);
	s[i]=h*x+ y[i];
	}
return;
}

isign (to,from) int to,from;
{
int sign;
if (from>=0)sign=1;
if (from<0)sign=-1;
return( abs(to)*sign);
}

float sign (to,from) float to,from;
{
float sin;
if (from>=0)sin=1.;
if (from<0)sin=-1.;
return( abs(to)*sin);
}


