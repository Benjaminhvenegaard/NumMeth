#include <stdio.h>
void main ()
{
	void eialpha(float, int, float []);
	int k;
	float a[6];

	printf("EIALPHA delivers:\n");
	eialpha(0.25,5,a);
	for (k=0; k<=5; k++) {
		printf(" %2d   %e\n",k,a[k]);
	}
}

