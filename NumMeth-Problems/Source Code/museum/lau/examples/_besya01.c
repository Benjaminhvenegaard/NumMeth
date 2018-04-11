#include <stdio.h>
void main ()
{
	void bessya01(float, float, float *, float *);
	float p,q;

	bessya01(0.0,1.0,&p,&q);
	printf("BESSYA01 delivers:  %e   %e\n",p,q);
}

