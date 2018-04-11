#include <math.h>
#include <stdio.h>

void f(int m, float a[], float *sigma)
{
	float a1,a2;

	a1=a[1];
	a2=a[2];
	a[1]=(a1+0.99)*(a2-1.0)+0.99;
	a[2]=1000.0*((1.0+a1)*(1.0-a2)-1.0);
}

void jacobian(int m, float **j, float y[], float *sigma)
{
	j[1][1]=y[2]-1.0;
	j[1][2]=0.99+y[1];
	j[2][1]=1000.0*(1.0-y[2]);
	j[2][2] = -1000.0*(1.0+y[1]);
	*sigma=fabs(j[2][2]+j[1][1]-sqrt((j[2][2]-j[1][1])*
					(j[2][2]-j[1][1])+4.0*j[2][1]*j[1][2]))/2.0;
}

void out(float x, float xe, int m, float y[],
			float sigma, float **j, float info[])
{
	if (x == 50.0)
		printf("%3.0f %3.0f %3.0f %3.0f %3.0f %3.0f  %5.1e  %e   %e\n",
				info[1],info[2],info[3],info[4],info[5],info[6],info[9],
				y[1],y[2]);
}

void main ()
{
	float **allocate_real_matrix(int, int, int, int);
	void free_real_matrix(float **, int, int, int);
	void liniger1vs(float *, float, int, float [], float *,
				void (*)(int, float[], float *), float **,
				void (*)(int, float **, float [], float *),
				int, float, float, float, float,	float [],
				void (*)(float, float, int, float [], float,
							float **, float []));
	int i,itmax;
	float x,sigma,reta,y[3],**j,info[10];

	j=allocate_real_matrix(1,2,1,2);
	printf("The results with LINIGER1VS are:\n\n");
	reta=1.0;
	for (i=1; i<=3; i++) {
		reta *= 1.0e-2;
		x=y[2]=0.0;
		y[1]=1.0;
		liniger1vs(&x,50.0,2,y,&sigma,f,j,jacobian,10,0.1,50.0,
					reta,reta,info,out);
	}
	printf("\n");
	reta = -1.0;
	for (i=1; i<=3; i++) {
		reta *= 1.0e-2;
		x=y[2]=0.0;
		y[1]=1.0;
		liniger1vs(&x,50.0,2,y,&sigma,f,j,jacobian,10,0.1,1.0,
					reta,reta,info,out);
	}
	free_real_matrix(j,1,2,1);
}

