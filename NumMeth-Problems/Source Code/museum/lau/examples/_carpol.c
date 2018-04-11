#include <stdio.h>
void main ()
{
	void carpol(float, float, float *, float *, float *);
	float r,c,s;

	carpol(0.3,0.4,&r,&c,&s);
	printf("The polar coordinates of 0.3+0.4*i are \n"
			" modulus:  %-4.2f\n cosine of argument:  %-4.2f\n"
			" sine of argument:  %-4.2f",r,c,s);
}

