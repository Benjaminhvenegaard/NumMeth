#include <math.h>

void selzerortpol(int n, int n1, int n2, float b[], float c[],
						float zer[], float em[])
{
	float *allocate_real_vector(int, int);
	void free_real_vector(float *, int);
	void valsymtri(float [], float [], int, int, int,
						float [], float []);
	int i;
	float nrm,*d;

	d=allocate_real_vector(1,n);
	nrm=fabs(b[0]);
	for (i=n-2; i>=1; i--)
		if (c[i]+fabs(b[i]) > nrm) nrm=c[i]+fabs(b[i]);
	if (n > 1)
		nrm = (nrm+1 >= c[n-1]+fabs(b[n-1])) ? nrm+1.0 :
					(c[n-1]+fabs(b[n-1]));
	em[1]=nrm;
	for (i=n; i>=1; i--) d[i]=b[i-1];
	valsymtri(d,c,n,n1,n2,zer,em);
	em[5]=em[3];
	free_real_vector(d,1);
}
