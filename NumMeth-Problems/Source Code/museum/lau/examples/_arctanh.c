#include <stdio.h>
void main ()
{
	float arctanh(float);
	printf("ARCTANH delivers:\n");
	printf("  %e\n",arctanh(tanh(0.01)));
	printf("  %e\n",arctanh(tanh(0.05)));
	printf("  %e\n",tanh(arctanh(0.05)));
	printf("  %e\n",tanh(arctanh(0.01)));
}

