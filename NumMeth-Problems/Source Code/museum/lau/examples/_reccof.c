#include <stdio.h>

float a(float x)
{
	return sqrt(1.0-x*x);
}

void main ()
{
	void reccof(int, int, float *, float (*)(float), float [],
					float [], float [], int);
	float x,b[3],c[3],l[3];

	reccof(2,200,&x,a,b,c,l,1);
	printf("Delivers: %7.3f %7.3f\n",c[1],c[2]);
}

