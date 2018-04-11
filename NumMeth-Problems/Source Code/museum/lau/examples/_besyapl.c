#include <stdio.h>
void main ()
{
	void bessyaplusn(float, float, int, float []);
	float yan[3];

	bessyaplusn(0.0,1.0,2,yan);
	printf("BESSYAPLUSN delivers:\n  %e   %e   %e\n",
			yan[0],yan[1],yan[2]);
}

