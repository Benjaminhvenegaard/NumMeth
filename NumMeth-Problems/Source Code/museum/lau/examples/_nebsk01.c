#include <stdio.h>
void main ()
{
	void nonexpbessk01(float, float *, float *);
	int j;
	float x,k0,k1;

	printf("NONEXPBESSK01 delivers:\n");
	x=0.5;
	for (j=1; j<=5; j++) {
		nonexpbessk01(x,&k0,&k1);
		printf(" %4.1f   %e   %e\n",x,k0,k1);
		x += 0.5;
	}
}

