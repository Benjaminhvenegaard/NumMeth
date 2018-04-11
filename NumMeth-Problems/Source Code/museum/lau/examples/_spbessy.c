#include <stdio.h>
void main ()
{
	void spherbessy(float, int, float []);
	int n;
	float x,y[3];

	x=1.57079632679489;
	n=2;
	spherbessy(x,n,y);
	printf("SPHERBESSY delivers:\n X = %e    N = %d\n"
			" %e  %e  %e\n",x,n,y[0],y[1],y[2]);
}

