#include <stdio.h>
void main ()
{
	float gamma(float);
	int i;
	static float x[4]={-8.5, 0.25, 1.5, 22.0};
	printf("GAMMA delivers:\n");
	for (i=0; i<=3; i++)
		printf(" %6.2f   %+e\n",x[i],gamma(x[i]));
}

