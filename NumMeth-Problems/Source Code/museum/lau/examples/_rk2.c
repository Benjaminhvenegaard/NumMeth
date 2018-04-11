#define float double

#include <math.h>
#include <stdio.h>

float fxyz(float x, float y, float z)
{
	return 10.0*(1.0-y*y)*z-y;
}

void main ()
{
	void rk2(float *, float, float, float *, float, float *, float,
			float (*)(float, float, float), float [], float [], int);
	int i,fi;
	float x,y,z,e[5],d[6],
			b[4]={9.32386578, 18.86305405, 28.40224162, 37.94142918};

	e[1]=e[2]=e[3]=e[4]=e[5]=1.0e-8;
	printf("RK2 delivers :\n");
	for (i=0; i<=3; i++) {
		fi=(b[i] < 10.0);
		rk2(&x,0.0,b[i],&y,2.0,&z,0.0,fxyz,e,d,fi);
		printf("  X = %+e  Y = %+e    DY/DX = %+e\n",x,y,z);
	}
}

