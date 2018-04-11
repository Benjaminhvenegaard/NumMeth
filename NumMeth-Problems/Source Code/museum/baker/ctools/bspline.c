/* B-spline package
	(based on DeBoor's  FORTRAN  package)

test problem: fit sin ( pi*x) for 0<=x<=1
with constraints that the function has zero derivatives at the endpoints
the test problem uses the linear system solution routines of Chapter 2
to solve for the coefficients  of the fit.
the fundamental B-spline routines do not use the linear system solver
themselves 

from Handbook of C tools for scientists and engineers by L. Baker

DEPENDENCIES:
	linear systems solver from Chapter 3
*/


/*#include "libc.h"
#include "math.h"
*/
#define DOFOR(i,to) for(i=0;i<to;i++)
#define jmax 20
#define INDEC(i,j,col) [j+(i)*col]
#define min(a,b) ((a)<(b)? (a):(b))
#define max(a,b) ((a)>(b)? (a):(b))

/* determines which interval (between knots) the location x is

	uses binary search

*/

interv(t,nbk,x,left,mflag) int nbk,*left,*mflag;
float x,t[];
{
int i,n,top,btm;
float below,above;
n=nbk-1;/*index of last value in t[]*/
if(x<t[0])
	{*mflag=-1;*left=0;}
else
if(x>=t[n])
	{*mflag=1;*left=n;}
else
	{
	*mflag=0;
	/*search*/
	/* binary search*/
	btm=0;top=n; 
	while(1){
		i=(top+btm)>>1;
		below=t[i];above=t[i+1];
/*printf(" top,btm,i %d %d %d\n",top,btm,i);*/
		if( below<=x)
			{
			if(x<above) 
				{
				*left=i;
				return;
				}
			/*x>=above*/
			btm=i;
			}
		else{		/* x<below*/
		    top=i;
		    }
		}
	}
return ;
}


/* evaluates b-spline basis functions */

bsplvb(t,jhigh,index,x,left,biatx)
int jhigh,index,left;
float t[],x,biatx[];
{static int j=0; int jp1,i;
static float deltal[jmax],deltar[jmax],saved,term;
/* index=0 start from scratch else j+1 to jout generated*/
/*printf(" bsplvb jhigh %d index %d left %d j %d\n", jhigh,index,left,j);*/
if(!index)
	{
	j=0;	
	biatx[0]=1.;
	if(jhigh<2)return;
	}
for( ; j<jhigh-1;j++)
	{
	jp1=j+1;
	deltar[j]=t[left+j+1]-x;
	deltal[j]=x-t[left-j];
/*printf(" x %f t %f %f %f %f\n",x,t[left+j+1],t[left-j],deltal[j],deltar[j]);
*/
	saved=0.;
	DOFOR(i,j+1)
		{
		term=biatx[i]/(deltar[i]+deltal[j-i]);
		biatx[i]=saved+term*deltar[i];
/*printf(" term=%e saved=%e ans=%e\n",term,saved,biatx[i]);*/
		saved=deltal[j-i]*term;
/*printf(" j=%d,i=%d in b\n",j,i);*/
		}
	biatx[jp1]=saved;
/*printf(" b[%d]=%e\n",jp1,saved);*/
	}
/*printf(" leaving b\n");*/
return;
}


/* calculates values and derivatives of a function
represented by a B-spline 
uses bsplvb()
*/

bsplvd(t,k,x,left,a,biatx,nderiv) int k,left,nderiv;
float t[],x,a[],biatx[];
{
double fk,factor,sum;
int i,il,j,jlow,jpmid,kp1mm,ldummy,m,kp1,mhigh,ideriv;
kp1=k+1;
/*printf(" bsplvd k %d,left %d,nderiv %d\n",k,left,nderiv);*/
mhigh= min(nderiv,k);mhigh=max(mhigh,1);
bsplvb(t,kp1-mhigh,0,x,left,a);
/*deBoor uses column ordering of FORTRAN, sends biatx not a*/
if(mhigh==1)
	{
	DOFOR(j,k) biatx INDEC(j,0,k)=a[j];
/*	DOFOR(i,k)printf(" a[%d]=%e\n",i,a[i]);*/
	return;
	}
ideriv=mhigh;
for(m=2;m<=mhigh;m++)
	{
	jpmid=0;
	for(j=ideriv;j<=k;j++)
		{
		biatx INDEC(j-1,ideriv-1,k)=a[jpmid];
		jpmid++;
		}
	ideriv--;
	bsplvb(t,kp1-ideriv,1,x,left,a);
	}
DOFOR(j,k) biatx INDEC(j,0,k)=a[j];
/*DOFOR(i,k)printf(" a[%d]=%e\n",i,a[i]);*/

jlow=0;
DOFOR(i,k)
	{
	for(j=jlow;j<k;j++)a INDEC(j,i,k)=0.;
	jlow=i;
	a INDEC( i,i,k)=1.;
	}

for(m=2;m<=mhigh;m++)
	{
	kp1mm=kp1-m;
	fk=kp1mm;	
	il=left;
	i=k-1;
	DOFOR(ldummy,kp1mm)
		{
		factor=fk/(t[il+kp1mm]-t[il]);

		DOFOR(j,i+1) 
			{
			a INDEC(i,j,k)=
			  factor*(a INDEC(i,j,k)-a INDEC(i-1,j,k));
			}
		il--;i--;
		}
	DOFOR(i,k)
		{
		sum=0.;
		jlow= max(i,m-1);
		for(j=jlow;j<k;j++) 
		    sum+=a INDEC(j,i,k) * biatx INDEC(j,m-1,k);
		biatx INDEC(i,m-1,k)=sum;
/*printf(" biatx[%d][%d]=%f\n",i,(m-1),sum);*/
		}
	}
/*printm(a,4,4,4,4);
printf(" leaving d\n");*/
return;
}

/* -------------------- test driver -------------------*/

main (argc,argv) int argc; char **argv;
{
/* don amos' example 2a- fit spline and check it out */
float xi[11],z,y,x,yi[11],t[17],cm[169],r[13],a[13],vnikx[16];
float work[16],bvalue();/*dimensioned kxk*/
/* CAVEAT  some differences between DeBoor's book (1978) and
	previous articles.  For example, bsplvd has an extra
	argument, the work array, in the later book.
*/ 
int pivot[11];
double sin();
int mult[11],interv(),ilft,i,nm1,m,imk,nbk,n,nb,kp1,nm;
int j,it,km1,k,n,jshift,nderiv,mflag,info;
k=4;n=11;/* cubic spline order k=4, 11 points*/

/*test problem*/
DOFOR(i,n)
	{
	xi[i]= x=i*.1; 
	yi[i]=y=sin(x*3.141592653589);
	}

/* the rest of the routine is fairly general*/
nm1=n-1;
mult[0]=k;
for(i=1;i<nm1;i++) mult[i]=1;
mult[nm1]=k;
j=0;
DOFOR(i,n)
	{
	DOFOR(m, (mult[i]) )
		{
		t[j++]= xi[i];
		}
	}
nb=j-k;
DOFOR(i,nb)
	{
	DOFOR(j,nb) cm INDEC(i,j,nb)=0.;
	}
nbk=nb+k;
km1=k-1;
nderiv=3;/*second deriv b.c. */
ilft=k-1;
printf(" n=%d nb=%d nbk=%d\n",n,nb,nbk);
DOFOR(i,nbk)printf(" t=%f\n",t[i]);
DOFOR(i,n)printf(" mult=%d\n",mult[i]);

bsplvd(t,k,xi[0],ilft,work,vnikx,nderiv);
DOFOR(j,k) cm INDEC(0,j,nb)=vnikx INDEC(j,2,k);
r[0]=0.;
/* rest of equations*/
nderiv=1;
DOFOR(i,nm1)
	{
	x=xi[i];
	interv(t,nbk,x,&ilft,&mflag);
/*printf(" ilft %d mflag=%d x %e\n",ilft,mflag,x);*/
	jshift=ilft-km1+1;
	if(jshift+k >nb )jshift=nb-k+1;
	bsplvd(t,k,x,ilft,work,vnikx,1);
	DOFOR(j,k)
		{
		it=jshift-1+j;
		cm INDEC(i+1,it,nb)= vnikx INDEC(j,0,k);
/*if (!j)printf(" first it=%d\n",it);*/
		r[i+1]=yi[i];
		}
	}
/*left limits of last two eqns*/
nderiv=3;/*second derivative b.c.*/
jshift=nb-km1;
bsplvd(t,k,xi[nm1],ilft,work,vnikx,nderiv);
DOFOR(j,k)
	{
	it=jshift+j-1;
/*if(!j)printf(" first it=%d\n",it);*/
	cm INDEC(nb-2,it,nb)= vnikx INDEC(j,0,k);
	cm INDEC(nb-1,it,nb)= vnikx INDEC(j,2,k);
	}
r[nb-2]=yi[nm1];
r[nb-1]=0.;/*second deriv.=0*/

/* lufact backsub solve system cm x= r*/	
printm(cm,nb,nb,nb,nb);
lufact(cm,nb,nb,pivot,&info);
if(info!=0)printf(" info=%d\n",info);
DOFOR(i,nb)printf("data r=%e\n",r[i]);
backsub(cm,nb,nb,pivot,r);
DOFOR(i,nb)printf("solution=%e\n",r[i]);
DOFOR(i,20)
	{
	x=i*.05;
	y=bvalue(t,r,nb,k,x,0);
	z=bvalue(t,r,nb,k,x,1)/3.14159265358979;/* deriv of sin=cos*/
	printf(" x=%f sin=%f exact=%f cos=%f\n",x,y,sin(x*3.14159265),z);
	}

}


/* bvalue returns the jderiv-th derivative of a function
represented by a bspline, at any point x within the
range of the fitted function.

uses interv()
*/


float bvalue(t,bcoef,n,k,x,jderiv) int n,k,jderiv;
float t[],bcoef[],x;
{
int i,ilo,nmi,imk,j,jc,jcmin,jcmax,jj,kmj,km1,mflag,nm1;
float fkmj,aj[jmax],dl[jmax],dr[jmax];
if(jderiv>=k)return (0.);
interv(t,n+k,x,&i,&mflag);
if(mflag)return(0.);
km1=k-1;
if(km1<=0)return(bcoef[0]);
jcmin=0;
imk=i-k;
/*printf(" i %d,k %d,imk %d\n",i,k,imk);*/
if(imk<-1)
	{
	jcmin=-1-imk;
	DOFOR(j,i+1)dl[j]=x-t[i-j];
	for(j=i;j<km1;j++)
		{
		aj [km1-j-1]=0.;
		dl[j]=dl[i];
/*printf(" j%d i%d aj[%d]\n",j,i,(km1-j-1));*/
		}
	}
else
	{
	DOFOR(j,km1) dl[j]=x-t[i-j];
	}
jcmax=k;
nmi=n-i;
if(nmi<1)
	{
	jcmax=k+nmi;
	DOFOR(j,jcmax)dr[j]=t[i+j+1]-x;
	for(j=jcmax-1;j<km1;j++)
		{
		aj[j+1]=0.;
		dr[j]=dr[jcmax];
		}
	}
else
	{
	DOFOR(j,km1) dr[j]=t[i+j+1]-x;
	}
/*printf(" jcmin %d max %d\n",jcmin,jcmax);
DOFOR(j,km1)printf(" dr=%f dl=%f\n",dr[j],dl[j]);*/
for(j=jcmin;j<=jcmax;j++) aj[j]=bcoef[j+imk+1];
if(jderiv!=0)
	{
	DOFOR(j,jderiv)
		{
		kmj=km1-j;
		ilo=kmj-1;
		fkmj=kmj;
		DOFOR(jj,kmj)
			{
/*if(ilo<0)printf(" warn1 ilo=%d\n",ilo);*/
			aj[jj]=fkmj*((aj[jj+1]-aj[jj])/(dl[ilo]+dr[jj]));
			ilo--;
			}
		}
	}
if(jderiv!=km1)
	{
	for(j=jderiv+1;j<=km1;j++)
		{
		kmj=km1-j;
		ilo=kmj;

		DOFOR(jj,kmj+1)
			{
/*if(ilo<0)printf(" warn2 ilo=%d\n",ilo);*/
			aj[jj]=(aj[jj+1]*dl[ilo]+aj[jj]*dr[jj])
			  /(dl[ilo]+dr[jj]);
			ilo--;
			}
		}
return(aj[0]);
	}
return(0.);/*DeSmet*/
}
