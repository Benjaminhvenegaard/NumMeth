#include <math.h>

void eialpha(float x, int n, float alpha[])
{
	int k;
	float a,b,c;

	c=1.0/x;
	a=exp(-x);
	b=alpha[0]=a*c;
	for (k=1; k<=n; k++) alpha[k]=b=(a+k*b)*c;
}
