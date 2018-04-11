#include <stdio.h>
void main ()
{
	void bessy(float, int, float []);
	float y[3];

	bessy(1.0,2,y);
	printf("BESSY delivers:\n  %e   %e   %e\n",y[0],y[1],y[2]);
}

