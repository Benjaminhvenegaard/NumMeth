#include <math.h>
#include <stdio.h>
void main ()
{
	void spherbessk(float, int, float []);
	void nonexpspherbessk(float, int, float []);
	int n;
	float x,expx,k1[4],k2[4];

	printf("SPHERBESSK and NONEXPSPHERBESSK deliver:\n");
	x=2.0;
	expx=exp(-x);
	n=3;
	spherbessk(x,n,k1);
	nonexpspherbessk(x,n,k2);
	for (n=0; n<=3; n++)
		printf("  %d   %e  %e\n",n,k1[n],k2[n]*expx);
}

