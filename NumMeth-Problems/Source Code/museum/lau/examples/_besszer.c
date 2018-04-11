#include <stdio.h>
void main ()
{
	void besszeros(float, int, float [], int);
	int n,d;
	float a,z[3];

	a=3.14;
	n=d=2;
	besszeros(a,n,z,d);
	printf("The first two zeros of the Bessel function Y of "
			"the order 3.14:\n  %e   %e\n",z[1],z[2]);
}

