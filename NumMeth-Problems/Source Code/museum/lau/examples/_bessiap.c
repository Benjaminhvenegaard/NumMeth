#include <stdio.h>
void main ()
{
	void bessiaplusn(float, float, int, float []);
	float a,x,ia[3];

	a=0.25;
	x=2.0;
	bessiaplusn(a,x,2,ia);
	printf("BESSIAPLUSN delivers:\n A = %4.2f    X = %4.2f\n"
			" %e   %e   %e\n",a,x,ia[0],ia[1],ia[2]);
}

