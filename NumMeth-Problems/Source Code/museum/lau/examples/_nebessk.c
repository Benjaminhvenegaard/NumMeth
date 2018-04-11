#include <stdio.h>
void main ()
{
	void nonexpbessk(float, int, float []);
	int j;
	float x,k[3];

	printf("NONEXPBESSK delivers:\n");
	x=0.5;
	for (j=1; j<=4; j++) {
		nonexpbessk(x,2,k);
		printf(" %4.1f   %e   %e   %e\n",x,k[0],k[1],k[2]);
		x += 0.5;
	}
}

