#include <stdio.h>
void main ()
{
	void comsqrt(float, float, float *, float *);
	float r,i;

	comsqrt(-3.0,4.0,&r,&i);
	printf("The square root of -3+4*i is %-4.2f+%-4.2f*i",r,i);
}

