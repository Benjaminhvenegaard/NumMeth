/*
stiff IVP-ODE solver

from Handbook of c tools for scientists and engineers by L. Baker

based on method Brayton,Gustavson,Hachtel as implemented by
Zimmerman second order method only

DEPENDENCIES:

uses the linear system solver from Chapter 3
(selections from header fotc.h has been included for convenience)
*/
/*
#include "libc.h"
#include "math.h"
*/
/* in-line functions for use with 2D arrays: */

/* row major order as in C  indices run 0..n-1 as in C*/
#define INDEX(i,j)  [j+(i)*coln]
#define INDEC(i,j,col) [j+col*(i)]
/*various loop constructors */
#define DOFOR(i,to) for(i=0;i<to;i++)
#define DFOR(i,from,to) for(i=from-1;i<to;i++)
#define DOBY(i,from,to,by) for(i=from-1;i<to;i+=by)
#define DOBYY(i,from,to,by) for(i=from;i<to;i+=by)
#define DOBYYY(i,from,to) for(i=from;i<to;i++)
#define DOV(i,to,by) for(i=0;i<to;i+=by)
/* row major order as in C  indices run 1..n */
/*#define INDEX1(i,j)  [j-1+(i-1)*n]
*/
/* column major order, as in fortran: */
#define INDEXC(i,j) [i-1+(j-1)*rown]

/* usage: if a(20,30) is matrix, then
a(i,j) in C will be a INDEX(i,j) if n=30. */

/* to index vectors starting with 1 */
#define VECTOR(i) [i-1]

#define min(a,b) (((a)<(b))? (a): (b))
#define max(a,b) (((a)>(b))? (a): (b))
#define abs(x)  ((x)>=0.? (x): -(x))
main (argc,argv) int argc; char **argv;
{
float exp(),a,b,time,x[8],t[4],delt,deltmn,deltmx,u,v,emax; 
int f();
static float deltax=1.e-6,deltbx=.01,acc=.001;
float deltst,work[100];
int i,j,pivot[3],ibug,coln=4,iter;
x INDEX(0,3)=x INDEX(1,3)=1.;time=0.;iter=1;
ibug=0;
printf(" enter ibug>0 for debug info from stiff2\n");
scanf("%d",&ibug);
printf(" ibug=%d\n",ibug);
t[0]=t[1]=t[2]=t[3]=0.0;
printf(" enter delt,deltmn,deltmx,deltst,emax\n");
scanf("%f %f %f %f %f",&delt,&deltmn,&deltmx,&deltst,&emax);
printf(" delt %f min %f max %f st %femax %f\n"
,delt,deltmn,deltmx,deltst,emax);
for (i=0;i<200;i++)
{
t[3]=time;
/*printf(" calling stiff2 %f %f %f %f\n",delt,deltmn,deltmx,emax);*/
stiff2(t,x,&iter,2,&delt,&deltmn,&deltst,&deltmx,&emax,&ibug,f
,deltax,deltbx,acc,pivot,work);
time=t[3];
if (ibug>0)printf(" WARNING- ibug=%d\n",ibug);
if (ibug==5)ibug=0;/* reset */
if ((i % 10)==0    || (i<5) ){
	a=exp(-time);b=exp(-1000.*time);
	u= 4.*a-3.*b;
	v=-2.*a+3.*b;
printf(" %f %f %f %f %f %f\n",
time,u,x INDEX(0,3),v,x INDEX(1,3),delt);
	};
};
exit(0);
}

f(x,t,xdot,neq) int neq; float t,x[],xdot[];
{float u,v;int coln=4;
u= x INDEX(0,3);
v= x INDEX(1,3);
xdot[0]= 998.*u+1998.*v;
xdot[1]=-999.*u-1999.*v;
return;
}


stiff2(t,x,iter,neq,delt,deltmn,deltst,deltmax,emax,ibug,f
,deltax,deltbx,acc,pivot,work)
/* pivot must be dimensioned at least neq work max(3,neq)**2+3neq
see text for details of other input parameters
note that t is a vector of length 4 with t[3] the current time
and x is a matrix of the dependent variables x[i][3] is the
ith dependent variable at the current time,both on entry and exit

*/
int pivot[],neq,*ibug,*iter; int (*f)();
float t[],x[],*delt,*deltmn,*deltst,*deltmax,*emax,deltax,deltbx,acc,work[];
{
int base,nlist=1000,norder=2,ierr1=0,newton=20,iredmx=10,ierr=0,isflag=0;
int coln,nn,ired,i,j,k,npow,iflag,info;
/* will modify so that a,xdot,xdot1,xp pivot point to sections of
	a single work array passed in*/
float *a,alpha[3],sum,gamma[3],*xdot,*xdot1,*xp,pown(),delx,erm;
float w1,detold,detnew,w2,w3,w4,ww5,ddx;
float dum; float xnorm;int lufact(),backsub();
coln=3;a=work;base=max(3,neq);base=base*base;
xdot= &(work[base]);base+=neq;
xdot1= &(work[base]);base+=neq;
xp= &(work[base]);
/*shift arrays*/
if (*delt<=0.){
		*delt=*deltmn;
		printf(" bad delt input to stiff2, reset to %f\n",*delt);
               };
if(*emax<=0.)
	{
	printf(" bad emax=%f\n",*emax);
	return;
	}
for (i=0;i<neq;i++)
	{
	x INDEC(i,0,4)= x INDEC(i,1,4);
	x INDEC(i,1,4)= x INDEC(i,2,4);
	x INDEC(i,2,4)= x INDEC(i,3,4);
	};
t[0]=t[1];t[1]=t[2];t[2]=t[3];

if( (*iter-norder)<=1)
	{/* start by Euler steps at (small) delst */
	*delt=*deltst;
	t[3]=t[2]+ *delt;

	(*f)(x,t[3],xdot,neq);
	for (i=0;i<neq;i++) x INDEC(i,3,4)= x INDEC(i,2,4)+*delt*xdot[i];
	(*iter)++;	
/*detold just for diagnostics printed by stiff2*/
	detold=*delt;
	return;
	}

if((*iter-norder)>=2)
	{
redo:	for(ired=0;ired<iredmx;ired++)
		{
		if((*ibug)>0){printf("ired %d delt %f ibug %d\n"
				,ired,*delt,*ibug);};
		t[3]=t[2]+*delt;
		/* predictor*/
		nn=norder+1;
		coln=3;
		for (k=0;k<nn;k++)
			{
			gamma[k]=0.;
			if(!k)gamma[k]=1.;
			dum=((t[3]-t[2-k]) / (*delt) ) ;
/* pulled out of loop-dum is independent of i*/
			for(i=0;i<nn;i++)
				{
				a INDEX(i,k)=pown(dum,i);
				};
			}/* for k*/;
			lufact(a,coln,nn,pivot,&info);
			if(info){printf(" bad matrix 1in stiff2\n");
					  *ibug=1; return;};

			   backsub(a,coln,nn,pivot,gamma);
			for(k=0;k<neq;k++)
				{
				sum=0.;
				for(i=0;i<nn;i++)
					{
					sum+=gamma[i]*x INDEC(k,2-i,4);
					}	
				xp [k]=sum;
				x INDEC(k,3,4)=xp[k];
				} /* for k */
			/* prepare for iteration*/
			nn=norder+1;
			for (k=0;k<nn;k++)
				{
				alpha[k]=0.;
				if(k==1)alpha[k]=1.;
		dum=((t[3]-t[3-k]) / (*delt) );
		/*similarly moved out of loop*/
			for (i=0;i<nn;i++)
					{
				a INDEX(i,k)=pown(dum,i);
				};
			}/* for k*/;

				lufact(a,coln,nn,pivot,&info);
   			if(info){printf(" bad matrix 2in stiff2\n");
				  *ibug=2;return;};

				backsub(a,coln,nn,pivot,alpha);
				 coln=neq;

			/* newton raphson iteration*/
			for (j=0;j<newton;j++)
				{/*zerom(a,neq,neq);not needed*/
				(*f)(x,t[3],xdot,neq);
				for (k=0;k<neq;k++)
					{
					w4= x INDEC(k,3,4);
					ddx=deltbx*w4;
					x INDEC(k,3,4)=w4-ddx;
					(*f)(x,t[3],xdot1,neq);
					x INDEC(k,3,4)=w4;
					ww5=0.;
					if (ddx!=0.)ww5=*delt/(ddx*alpha[0]);
					for(i=0;i<neq;i++)
						{
						w2=0.0;
						if(i==k)w2=1.;
						w3=ww5*(xdot[i]-xdot1[i]);
						w4=w2+w3;
						/* a= 1+Jacobian */
						/* no need to zero a, or add to itself*/
     					a INDEX(i,k)=w4;
						}
					}/*end k*/
				/* rhs vector calc*/
				for (k=0;k<neq;k++)	
					{
					sum=0.;
					for (i=1;i<nn;i++)
					sum+=alpha[i]* x INDEC(k,3-i,4);
					xdot[k]=-x INDEC(k,3,4)
					 -(sum+*delt*xdot[k])/alpha[0];
					}/*end k*/
				lufact(a,coln,neq,pivot,&info);
			if(info){printf(" bad matrix 3 in stiff2\n");
				*ibug=4;return;};

		         	backsub(a,coln,neq,pivot,xdot);
				/*advance state vector*/
				delx=0.;
				xnorm=0.;
				ddx=0.;
				iflag=0;
			for (i=0;i<neq;i++)
					{
					ddx=abs(xdot[i]);
					xnorm=abs(x INDEC(i,3,4))+deltax;
					w1=ddx/xnorm;
					if(w1>acc)iflag=1;
					x INDEC(i,3,4)+=xdot[i];
					if(w1>delx)delx=w1;
					}/*i*/
				if((*ibug)>0)
				{printf(" j %d, delx %f acc %f ibug %d\n"
				,j,delx,acc,*ibug);};
				if(iflag!=1)goto convg;
				/* else continue iteration*/
				}/* for j newton raphson*/
		/* no convergence in NR- cut delt and repeat or exit*/
		if( *delt<= *deltmn) { isflag=1;
				if((*ibug)>0)printf("dt<dtmin at" ,t[3]);
							}
		*delt=*delt*.25;
		if(isflag>=1)goto goodbye;
		*delt= max(*delt,*deltmn);
	};/* end iteration ired */
		/* warn about outerloop nonconvergence*/
		printf(" outer loop stiff2 nonconvrg\n");
convg:
		erm=0.;
		for (i=0;i<neq;i++)
			{
			ddx= abs( xp[i]-x INDEC(i,3,4));
			xnorm= abs( x INDEC(i,3,4) ) +deltax;
			w1=ddx/(xnorm*(t[3]-t[2-norder]));
			if(w1>erm)erm=w1;
			}
/* erm is   now  (x[predictor]-x[corrector])/{t-t}/x-norm:
compared to Henrici local trunc. error, need to mult by dt
and xnorm- this is a relative local trunc error if we mult by
dt*/
	erm= erm* (*delt);
		if (erm>= .25* *emax && erm<= *emax*4.) goto goodbye;
		if (erm < *emax*.25) {*delt=2.* *delt;/* was 4.*/
  				*delt= min(*delt,*deltmax);
				/* had set iflag in chandler-not exit*/
					goto goodbye;};
		if (erm> *emax*4.)
			{if( *delt <= *deltmn)goto goodbye;
			*delt=*delt*.5;
			}
		*delt= max(*delt,*deltmn);
		*delt= min(*delt,*deltmax);
	if((*ibug)>0)printf(" redoing in stiff2 error= %f %f\n",erm,*emax);
	 	goto redo;
goodbye:
		w1= *deltmax;
		if(erm>0.)w1=*delt* *emax/erm;
		detnew= min(w1, (2.* *delt) );
		*delt=detnew;
		*delt= max(*delt,*deltmn);
		*delt= min(*delt,*deltmax);
(*iter)++;	}/* end of if  iter-norder>=2 (90 in FTN)*/
detold=*delt;
return;
}	


float pown(x,n) int n; double x;
{float ans,power;
if (!n) return (1.);
if(n==1)return (x);
power=x*x;
/* as only need up to n=2, will not do binary decomp of n*/
if(n==2)return(power);
if(n==3)return(power*x);
printf(" pown n=%d>3\n",n);
return (1.);/* should never get here*/
}
zerom(a,n,m) int n,m; double a[];
{int i,k;
k=n*m;
for (i=0;i<k;i++) a[i]=0.0;
return;
}
