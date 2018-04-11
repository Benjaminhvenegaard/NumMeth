/*  Gaussian Quadrature routine

from More C tools for Scientists and Engineers by L. Baker

includes function power() for computing arbitrary integral power of
a floating point variable x

*/



/*#include "libc.h"*/
#define DOFOR(i,to) for(i=0;i<to;i++)

int expn;

main(argc,argv) int argc;char **argv;
{
double gq3(),power();
int n;

DOFOR(n,7)
	{
	expn=n;
	printf(" integral 0-1 x to %d = %f \n",n, gq3(0.,1.,power) );
	}
exit(0);
}

double gq3(left,right,f)
double left,right,(*f)();
{
double a,b,y;

a=.5*(left+right);
b=right-left;
y=.3872983*b;
y=.2777777777*( (*f)(a+y) +(*f)(a-y) );
y=b*(y+ .4444444444*(*f)(a));
return(y);
}

double zprwr;/* making it global reduces the recursive call overhead*/

double power(x)
double x;
{double powr();
/* returns x**expn */
zprwr=x;
if (expn<0)
	{expn=-expn;
	 return(1./powr(expn));
	 }
return (powr(expn));
}

double powr(n)
int n;
{
double y;
if (n==0) return(1.);
if(n==1)return(zprwr);
	y=powr(n>>1);
	y=y*y;
if( n&1 ) 
	/* n is odd*/
	return( zprwr*y);
else
	return(y);
return(0.);/* keep DeSmet happy*/
}
