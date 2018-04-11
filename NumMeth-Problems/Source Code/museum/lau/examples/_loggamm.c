#include <stdio.h>
void main ()
{
	float loggamma(float);
	int i;
	static float x[5]={0.25, 1.5, 12.0, 15.0, 80.0};
	printf("LOGGAMMA delivers:\n");
	for (i=0; i<=4; i++)
		printf(" %6.2f   %+e\n",x[i],loggamma(x[i]));
}

