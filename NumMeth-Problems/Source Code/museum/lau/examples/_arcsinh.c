#include <stdio.h>
void main ()
{
	float arcsinh(float);
	printf("ARCSINH delivers:\n");
	printf("  %e\n",arcsinh(sinh(0.01)));
	printf("  %e\n",arcsinh(sinh(0.05)));
	printf("  %e\n",sinh(arcsinh(0.05)));
	printf("  %e\n",sinh(arcsinh(0.01)));
}

