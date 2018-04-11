/* vector processing routines
often simplified versions of BLAS routines for vectors
with contiguous storage

(from "C Tools for  Scientists and Engineers" by L. Baker)

CONTENTS:

dot(a,b,n)
	returns double value of dot product of two vectors a,b
	of n elements
pv(v,n)
	prints vector of n elements
mv(a,x,y,m,n)	
	y=ax where a is m x n matrix
normv(v,n)		
	normalize a vector (scale its length to one)
sqnor(v,n)		
	square of the length of vector v of n elements 
mvt(a,x,y,m,n)	
	like mv except y=a^x a^ =transpose of a
resid(r,a,x,n)	
	r=ax-x, a matrix all other vectors.
vs(v,s,n)		
	scale vector by multiplying each element by s
vset(v,s,n)		
	set vector v to scalar value s for each element
vcopy(x,y,n)	
	y=x, vectors 
vv(a,b,c,s,n)	
	a=b+c*s, s scalar, a,b,c,vectors 

DEPENDENCIES:
NONE
*/

#include "ftoc.h"

double dot(a,b,n) int n; float a[],b[];
{
int i;
double sum;
if(n<=0)return(0.);
sum=0.;
DOFOR(i,n)sum+=a[i]*b[i];
return(sum);
}

pv(v,n) int n;float v[];
{
int btm,top,i,ncol=4;
btm=0;
top=0;
while (btm<n)
	{
	top=min(btm+ncol,n);
	printf(" printing vector from %d to %d\n",btm,(top-1));
	for(i=btm;i<top;i++)printf(" %e",v[i]);
	printf("\n");
	btm+=ncol;
	}
return;
}

mv(a,x,y,m,coln) int coln,m;
float x[],y[],a[];
/* y=ax a(m,n) m rows n columns [row of length n]*/
{
int i,j,k;
float sum;
DOFOR(i,m)
	{
	sum=0.;
	DOFOR(j,coln) sum+= x[j]*a INDEX(i,j);
	y[i]=sum;
	}
return;
}

normv(v,n) int n; float v[];
{
double x,sqrt(),sqnor();
x=sqnor(v,n);
if(x!=0.)x=1./sqrt(x);
vs(v,x,n);
return;
}

double sqnor(x,n) float x[]; int n;
{
int i;
double ans;
ans=0.;
if(n<=0)return(ans);
DOFOR(i,n)ans+= x[i]*x[i];
return(ans);
}

mvt(a,x,y,m,coln) int coln,m;
float a[],x[],y[];
{
/* y= a^x  a(m,n) a m rows n columns [n elements/row]
a^ n rows m columns*/
float sum;
int i,j;
DOFOR(i,coln)
	{
	sum=0.;
	DOFOR(j,m)sum+= x[j]*a INDEX(j,i);
	y[i]=sum;
	}
return;
}

resid(a,x,y,r,n) int n;
float a[],x[],y[],r[];
{
int i;
mv(a,x,r,n,n);
DOFOR(i,n)r[i]-=y[i];
return;
}

vs(v,s,n) int n; float v[],s;
{
int i;
DOFOR(i,n)v[i]*=s;
return;
}

vset(x,s,n) int n; float s,x[];
{
int i;
DOFOR(i,n)x[i]=s;
return;
}


vcopy(x,y,n) int n; float x[],y[];
{
int i;
DOFOR(i,n)y[i]=x[i];
return;
}

vv(a,b,c,s,n) int n; float a[],b[],c[],s;
{
int i;
DOFOR(i,n) a[i]=b[i]+s*c[i];
return;
}
