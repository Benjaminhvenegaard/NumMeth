#include <stdio.h>
void main ()
{
	float logoneplusx(float);
	int i;
	float x;

	printf("LOGONEPLUSX delivers:\n");
	x=1.0e-1;
	for (i=1; i<=2; i++) {
		printf("  %e\n",logoneplusx(2.0*exp(x/2.0)*sinh(x/2.0)));
		x *= 1.0e-9;
	}
}

