#include <stdio.h>

float a(int i)
{
	return (pow(-1,i)/((i+1)*(i+1)));
}

void main ()
{
	float euler(float (*)(int), float, int);

	printf("Delivers:  %13.6e\n",euler(a,1.0e-6,100));
}

