#include <math.h>
#include <stdio.h>

float fxy(float x, float y)
{
	return -y;
}

void main ()
{
	void rk1(float *, float, float, float *, float,
				float (*)(float, float), float [], float [], int);
	int first;
	float x,y,d[5],e[3];

	e[1]=e[2]=1.0e-4;
	first=1;
	rk1(&x,0.0,1.0,&y,1.0,fxy,e,d,first);
	printf("RK1 delivers:\n  x = %e\n  y = %e      yexact =  %e",
				x,y,exp(-x));
}

