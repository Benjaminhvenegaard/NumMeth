/* test driver for

	RKF45 Runga-Kutta-Fehlberg integrator for
	system of Ordinary Differential Equations
	(Initial Value Problems)
	non-stiff to mildly stiff
(from "C Tools for Scientists and Engineers" by L. Baker)


DEPENDENCIES:
ftoc.h header file required
/*
#include "libc.h"
#include "math.h"
*/
#include "ftoc.h"

int problem; float ksq;

main (argc,argv) int argc; char **argv;
{
double sin(),cos(),exp(),sqrt();
float a,b,time,x[8],tout,t,u,v;
int f();float abserr,relerr,work[200],dt;
int i,j,iwork[25],ibug,neq,iter;
x [0]=x[1]=1.;t=0.;
ibug=1;neq=2;
for (i=0;i<1000;i++)
abserr=.001;relerr=.01;
printf(" enter dt");scanf("%f",&dt);
printf(" enter 0 for harmonic osc 1 for stiff\n");
scanf("%d",&problem);
if(!problem)
	{printf(" enter square of spring constant\n");
	scanf("%f",&ksq);
	x[1]=0.;
	a=sqrt((double)ksq);
	}
printf(" Time x1(Calculated) x1(Exact) x2(Calculated) X2(Exact)\n");
for(i=0;i<100;i++)
  {
  tout=t+dt;neq=2;
  rkf(neq,x,&t,&tout,relerr,abserr,&ibug,work,iwork,f);
  if(ibug>2){printf(" ibug=%d\n",ibug);exit(0);};
  time=t;
  t=tout;/*bumpt t*/
  if ((i % 10)==0    || (i<5) )
	{
	if(problem)
		{
		a=exp(-time);b=exp(-1000.*time);
		u= 4.*a-3.*b;
		v=-2.*a+3.*b;
		}
	else
		{
		b=time*a;
		u=cos((double)b);
		v=-a*sin((double)b);
        	}
	printf(" %f %f %f %f %f\n",time,x[0],u,x[1],v);
	};
  };
exit(0);
}

/*------------  right-hand sides of ODE'S--------------------*/

f(t,x,xdot) float t,x[],xdot[];
{float u,v;
u= x[0];
v= x[1];
if(problem==0)
	{
	xdot[0]=v;
	xdot[1]=-ksq*u;
	}
else
	{
	xdot[0]= 998.*u+1998.*v;
	xdot[1]=-999.*u-1999.*v;
	}
return;
}

