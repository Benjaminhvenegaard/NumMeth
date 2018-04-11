#include <float.h>

void alllagzer(int n, float alfa, float zer[])
{
	float *allocate_real_vector(int, int);
	void free_real_vector(float *, int);
	void allzerortpol(int, float [], float [], float [], float []);
	int i;
	float *a,*b,em[6];

	a=allocate_real_vector(0,n);
	b=allocate_real_vector(0,n);
	b[0]=0.0;
	a[n-1]=n+n+alfa-1.0;
	for (i=1; i<=n-1; i++) {
		a[i-1]=i+i+alfa-1.0;
		b[i]=i*(i+alfa);
	}
	em[0]=FLT_MIN;
	em[2]=FLT_EPSILON;
	em[4]=6*n;
	allzerortpol(n,a,b,zer,em);
	free_real_vector(a,0);
	free_real_vector(b,0);
}

