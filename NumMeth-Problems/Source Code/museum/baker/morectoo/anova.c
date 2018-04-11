/* analysis of variance ANOVA package

from More C tools for Scientists and Engineers by L. Baker

DEPENDENCIES: none
*/

/*#include "libc.h"
#include "math.h"
*/

#define DOFOR(i,to) for(i=0;i<to;i++)
#define INDEX(i,j) [j+(i)*coln]
#define failsafe 1000

anova(x,n,levels,t,i,ti,tlimit,q,a,b)
double x[],t[],q[],a[],b[];int n,levels[],i[],ti[],tlimit[];
{
/* CACM 330*/
int k1,k2,j,factor,coln,j1,indx(),kount;
kount=0;
DOFOR(factor,n)
	{
	coln=levels[factor];
	orthog(q,coln);
	DOFOR(j,n) i[j]=0;
	loop1:	
		DOFOR(i[factor],coln)
			a[i[factor]]=x[indx(i,levels,n)];
	DOFOR(k1,coln)
		{
		b[k1]=0.;
		DOFOR(k2,coln)b[k1]+=a[k2]*q INDEX(k2,k1);
		}		

	DOFOR(i[factor],coln)	
		x[indx(i,levels,n)]=b[i[factor]];

	DOFOR(j,n)
		{
		if(j!=factor)
			{
			i[j]++;
			if (i[j]<=levels[j]-1) goto loop1;
			else
				i[j]=0; 
			}
		}

	}
DOFOR(j,n)
	{
	ti[j]=0;
	tlimit[j]=2;
	}
loop2:
	DOFOR(j,n) i[j]=ti[j];
k1=indx(ti,tlimit,n);
t[k1]=0.;
loop3:
	k2=indx(i,levels,n);
x[k2]*=x[k2];
t[k1]+=x[k2];
DOFOR(j,n)
	{
	if(ti[j]!=0)
		{
		i[j]++;
		if(i[j]<=levels[j]-1) goto loop3;
		else
			i[j]=1;
		}
	}	
DOFOR(j,n)
	{
	ti[j]++;
	if(ti[j]<=1)goto loop2;
	else
		ti[j]=0;
	}
return;
}

int indx(subscript,limit,n)
int subscript[],limit[],n;
{
int j,temp;
temp=0;
for(j=n-1;j>=0;j--)
	{
	temp=limit[j]*temp+subscript[j];/*ftn-1*/
	}
return(temp);/*ftn+1*/
}

orthog(q,coln) int coln; double q[];
{
double rsize,fj,fj1,sqrt();
int i,j;
rsize=coln;
DOFOR(i,coln) q INDEX(i,0) = 1./sqrt(rsize);
for(j=1;j<coln;j++)
	{fj=(double)j;fj1=fj+1.0;
	DOFOR(i,j)
		q INDEX(i,j)=-1./sqrt( fj*fj1  );
	q INDEX(j,j)= sqrt( fj/fj1 );
	for(i=j+1;i<coln;i++) q INDEX(i,j)=0.;
	}
return;
}


panova(t,n) int n;
double t[];
{
int i,j,k,l,k1,j1;
printf("\n analysis of variance %d factors\n",n);
printf(" global ss=%f\n",t[0]);
k=1;
DOFOR(j,n)
	{
	printf(" ss for factor %d is %f (index %d)\n",j,t[k],k);
	k=k<<1;
	}
j1=1;
DOFOR(j,n)
	{k1=1<<(j+1);
	for(k=j+1;k<n;k++)
		{
		l= k1+j1;
		printf(" ss for interaction %d %d is %f %d\n",j,k,t[l],l);
		k1=k1<<1;
		}
	j1<<1;
	}

return;
}

main(argc,argv) int argc;char **argv;
{
static double x[9]={64.,61.,58.,61.,71.,61.,52.,64.,66.};
static double y[12]=
	{4.6,4.9,4.4,6.2,6.3,5.9,5.0,5.4,5.4,6.6,6.8,6.3};
static double z[30]=
	{5.01,4.61,5.22,4.93,5.37,4.74,4.41,4.98,4.26,4.80,
	4.99,4.55,4.87,4.19,4.77,5.64,5.02,4.89,5.51,5.17,
	5.07,4.93,4.81,5.19,5.48,5.90,5.27,5.65,4.96,5.39};
double q[36],a[36],b[36],t[36],sst,ss2,ss3,ms1,ms2;
int levels[6],ti[36],i[36],tlimit[36];
int n,j,k,l,j1,k1;
/*ex 13.7 p. 576ff  simple 2 way anova*/
n=2;
levels[0]= 3;
levels[1]= 4;
anova(y,n,levels,t,i,ti,tlimit,q,a,b);
panova(t,n);

printf("another problem\n");
/* ex13.5 p.565 two factor anova*/
n=2;
DOFOR(j,n)levels[j]=3;
anova(x,n,levels,t,i,ti,tlimit,q,a,b);
panova(t,n);
printf(" yet another problem\n");
/* example 14.1 p.624 nested sample TYPE II MODEL */
n=2;
levels[0]=5;
levels[1]=6;
sst=0.0;
l=1; DOFOR(k,n) l*=levels[k];/* count of observations*/
printf(" %d observations \n",l);
DOFOR(k,l) sst+= z[k]*z[k];
anova(z,n,levels,t,i,ti,tlimit,q,a,b);
panova(t,n);
ss2= sst-t[0];
ss3=ss2-t[2]; 
ms1= t[2]/(levels[1]-1);
ms2= ss3/(levels[1]*(levels[0]-1));
printf(" sst=%f, ss2=%f ss3=%f\n ms1=%f ms2=%f\n",sst,ss2,ss3,ms1,ms2);
exit(0);
}



