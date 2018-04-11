#include <stdio.h>

float ai(float i)
{
	return 1.0/(i*i);
}

void main ()
{
	float sumposseries(float (*)(float), int, float, int, int, int);

	printf("SUMPOSSERIES delivers:  %e\n",
			sumposseries(ai,100,1.0e-6,8,100,10));
}

