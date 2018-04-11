#include <stdio.h>
void main ()
{
	void besska01(float, float, float *, float *);
	float p,q;

	besska01(0.0,1.0,&p,&q);
	printf("BESSKA01 delivers:  %e  %e\n",p,q);
}

