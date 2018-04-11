#include <stdio.h>
void main ()
{
	void bessk01(float, float *, float *);
	int j;
	float x,k0,k1;

	printf("BESSK01 delivers:\n");
	x=0.5;
	for (j=1; j<=3; j++) {
		bessk01(x,&k0,&k1);
		printf(" %4.1f   %e   %e\n",x,k0,k1);
		x += 1.0;
	}
}

