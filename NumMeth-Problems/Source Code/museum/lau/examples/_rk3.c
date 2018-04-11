#include <math.h>
#include <stdio.h>

float fxy(float x, float y)
{
	return x*y;
}

void main ()
{
	void rk3(float *, float, float, float *, float, float *, float,
				float (*)(float, float), float [], float [], int);
	int n,i,fi;
	float x,y,z,e[6],d[6],x3,s,term,
			b[4]={0.25, 0.50, 0.75, 1.00};

	e[1]=e[3]=1.0e-6;
	e[2]=e[4]=1.0e-6;
	printf("RK3 delivers :\n");
	for (i=0; i<=3; i++) {
		fi=(b[i] < 0.3);
		rk3(&x,0.0,b[i],&y,0.0,&z,1.0,fxy,e,d,fi);
		x3=x*x*x;
		term=x;
		s=0.0;
		n=3;
		do {
			s += term;
			term=term*x3/n/(n+1);
			n += 3;
		} while (fabs(term) > 1.0e-14);
		printf("Y-YEXACT= %+e    X= %4.2f    Y= %8.6f\n",y-s,x,y);
	}
}

