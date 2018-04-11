#include <math.h>

void allzerortpol(int n, float b[], float c[], float zer[],
						float em[])
{
	float *allocate_real_vector(int, int);
	void free_real_vector(float *, int);
	int qrivalsymtri(float [], float [], int, float []);
	void dupvec(int, int, int, float [], float []);
	int i;
	float nrm,*bb;

	bb=allocate_real_vector(1,n);
	nrm=fabs(b[0]);
	for (i=1; i<=n-2; i++)
		if (c[i]+fabs(b[i]) > nrm) nrm=c[i]+fabs(b[i]);
	if (n > 1)
		nrm = (nrm+1 >= c[n-1]+fabs(b[n-1])) ? nrm+1.0 :
					(c[n-1]+fabs(b[n-1]));
	em[1]=nrm;
	for (i=n; i>=1; i--) zer[i]=b[i-1];
	dupvec(1,n-1,0,bb,c);
	qrivalsymtri(zer,bb,n,em);
	free_real_vector(bb,1);
}
