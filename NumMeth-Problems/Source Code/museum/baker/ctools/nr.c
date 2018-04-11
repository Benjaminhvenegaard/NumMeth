/* Newton Raphson iteration solution of system of nonlinear
equations.
	Modified for greater robustness near points of inflection.

TEST PROBLEM:
solves the Saha equations for the ionization equilibrium of
SiO2.

from Handbook of C tools for scientists and engineers by L. Baker

DEPENDENCIES:
uses linear system solver from Chapter 3
uses pv() function in vector.c
*/
/*
#include "libc.h"
#include "math.h"
*/
#define abs(x)  ((x)>=0? (x): -(x))

/* in-line functions for use with 2D arrays: */

/* row major order as in C  indices run 0..n-1 as in C*/
#define INDEX(i,j)  [j+(i)*coln]

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


#define min(a,b) (((a)<(b))? (a): (b))
#define max(a,b) (((a)>(b))? (a): (b))

float pop[10],fns,fno,fscale,dmult,dadd,tf,conv,conv2,rcon;

#define ne 8
#define neq 9

main(argc,argv) int argc;char **argv;
{
float rho,t,fne;
int i,j;
printf(" enter density (g/cc) ");
scanf(" %f",&rho);
printf(" rho=%f\n enter conv,conv2,rcon",rho);
scanf("%e %e %e",&conv,&conv2,&rcon);
printf("echo %e %e %e\n",conv,conv2,rcon);
pop[8]=0.;
fno=rho/(1.6e-24*(16.+.5*28.));
fns=.5*fno;
for(i=1;i<9;i++)
{t=i;
fne=saha(fns,fno,t);
printf(" t=%f rho=%f\n",t,rho);
pv(pop,neq);
}
exit(0);
}

/* u[]i[] i=1 silicon 2=oxygen are the statistical weights
of the ionic ground states
	pi are the ionization potentials in eV
	*/
float u[12][2]={1.,5.,2.,4.,1.,1.,2.,3.,1.,1.,4.,
2.,5.,1.,4.,2.,1.,0.,3.,0.,1.,0.,3.,0.};
float pi[12][2]={8.149,13.614,16.34,35.146,33.46,54.934,
45.13,77.394,166.7,113.873,205.11,138.080,
246.41,739.114,303.87,871.12,351.83,0.,401.3,0.,476.0,0.,523.2,0.};

double sahac(iatom,istate,t)
float t;
int iatom,istate;
{
double sa; double exp();

sa= 6.e21*u[istate+1][iatom]/u[istate][iatom]
	*exp(-pi[istate][iatom]/t);
return (fscale/sa);
}
double eqn(i,f) float f[]; int i;
{
double x;
/* the equations to be solved- Saha unless noted*/
switch (i)
	{
	case 0: x=f[ne]*f[1]-f[0]/sahac(0,0,tf);
		break;
	case 1: x=f[ne]*f[2]-f[1]/sahac(0,1,tf);
		break;
	case 2: x=f[ne]*f[3]-f[2]/sahac(0,2,tf);
		break;
/* conservation of silicon nuclei: */
	case 3: x=f[0]+f[1]+f[2]+f[3]-fns/fscale;
		break;
	case 4: x=f[ne]*f[5]-f[4]/sahac(1,0,tf);
		break;
	case 5: x=f[ne]*f[6]-f[5]/sahac(1,1,tf);
		break;
	case 6: x=f[ne]*f[7]-f[6]/sahac(1,2,tf);
		break;
/* conservation of oxygen nuclei:*/
	case 7: x=f[4]+f[5]+f[6]+f[7]-fno/fscale;
		break;
/* charge neutrality:*/
	case 8: x=f[1]+2.*f[2]+3.*f[3]
		 +f[5]+2.*f[6]+3.*f[7] -f[ne];
		break;
	
	}
return (x);
}

/* numerical differentiation for forming Jacobian matrix:*/
double dr(ieq,j,f) int ieq,j;
float f[];
{
double hold,dx,dn;
hold=eqn(ieq,f);
dx=f[j]*dmult+dadd;
f[j]+=dx;
dn=eqn(ieq,f);
f[j]-=dx;
return( (dn-hold)/dx);
}

#define itmax 20
#define ihmax 10
#define fignore 1.e-5

float b[9],a[9][9];



saha(fns,fno,t) float fns,fno,t;
{
int iter,coln,info,pivot[9],i,j,ic,ih;
double sqrt(),eqn(),dr();
float resid,newresid,fm,arg,abarg,relarg,rworst,aworst,fnsi,fnoi;
float popmin,popmax;
tf=t;
fscale=1.e20;
fnsi=fns/fscale;
fnoi=fno/fscale;
popmin=0.;
popmax=10.*(fnsi+fnoi);

printf("f=%e %e scaled %e %e\n ",fno,fns,fnoi,fnsi);
dmult=.001;
dadd=.01*fnoi;
/* initial guess*/
if (pop[neq-1]==0.)
	{
	arg=sahac(0,0,t);
	if (arg>1.e-4)
		fm=(sqrt(1.+2.*arg)-1.)/arg;
	else
		fm=1.;

	pop[0]=fns*(1.-fm);	
	pop[1]=fns*fm;
	pop[ne]=pop[1];
	pop[3]=.0001*pop[1];
	pop[2]=0.;
	arg=sahac(1,0,t);
	if (arg>1.e-4)
		fm=(sqrt(1.+2.*arg)-1.)/arg;
	else
		fm=1.;

	pop[4]=fno*(1.-fm);	
	pop[5]=fno*fm;
	pop[neq-1]+=pop[5];/*electron number density*/
	pop[6]=.0001*pop[5];
	pop[7]=0.;
	};
DOFOR(i,neq)printf(" guess pop=%e\n",pop[i]);

coln=neq;/* number of equations*/
/*scaling*/
DOFOR(i,neq) pop[i]=pop[i]/fscale;

/* newton-raphson iteration*/
DOFOR(iter,itmax)
{

/*rhs residual vector-
also calculate sq. error */
resid=0.;
DOFOR(i,neq)
	{
	arg=-eqn(i,pop);
	resid+=arg*arg;
	b[i]=arg;		
	DOFOR(j,neq)
		{/* JACOBIAN*/
		a[i][j]=dr(i,j,pop);
		};
	};
printf(" square residual sum=%e\n",resid);
if (resid<rcon) 
		{
		ic=-1;
		goto converged;
		}
lufact(a,coln,neq,pivot,&info);

DOFOR(i,j)printf("resid=%e\n",b[i]);
if( info!=0)
	{
	printf(" bad Jacobian, index=%d\n",info);
	goto salvage;
	}
backsub(a,coln,neq,pivot,b);
ic=-1;
rworst=0.;
aworst=0.;
DOFOR(i,neq)
	{
	arg=abs(b[i]);
	abarg=abs(pop[i]);
	relarg= (arg/(abarg+fignore));
	if(relarg<conv || arg<conv2)continue;
	ic=i;
	rworst= max(rworst,relarg);
	aworst= max(aworst,arg);
	};
converged:
if(ic<0)
	{/*converged*/
	DOFOR(i,neq)pop[i]*=fscale;
	return;
	};

printf(" no converge i %d,f[i]=%e,b[i]=%e\n rworst=%e,aworst=%e\n"
,ic,pop[ic],b[ic],rworst,aworst);
/* no good*/
salvage:
DOFOR(j,neq)printf(" f=%e,df=%e\n",pop[j],b[j]);
	/*tentatively update solution */
	DOFOR(i,neq)
		{
		pop[i]+=b[i];
		pop[i]=max(pop[i],popmin);
		pop[i]=min(pop[i],popmax);
		}
DOFOR(ih,ihmax)
	{
	/* have things gotten better? */
	newresid=0.;
	DOFOR(i,neq)
		{
		arg=-eqn(i,pop);
		newresid+=arg*arg;
		};
	if(newresid<resid)break;/* exit ih loop*/
	/*things got worse*/
	printf(" new residual=%e worse\n",newresid);
	/* cancel  half of the previous update*/
	/* this will insure convergence near inflection points
	-itd assumes we are going in the right direction with b[]
	corrections, merely too far*/
	/* example- if pop were initially 0, b 1, then 
			first update(above) pop=1
			after loop below(once) b=.5 pop=1-.5=.5
			                 twice b=.25 pop=.25
			                 thrice b=.125 pop=.125
			                 etc.
 	*/ 
	DOFOR(i,neq)
		{
		b[i]*=.5;
		pop[i]-=b[i];
		}
	}/* ih loop*/;
/*continue NR iteration*/
}/* iter loop*/
	printf(" no convergence NR\n");
exit(0);
}
