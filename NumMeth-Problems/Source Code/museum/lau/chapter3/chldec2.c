#include <math.h>

void chldec2(float **a, int n, float aux[])
{
	float tammat(int, int, int, int, float **, float **);
	int k,j;
	float r,epsnorm;

	r=0.0;
	for (k=1; k<=n; k++)
		if (a[k][k] > r) r=a[k][k];
	epsnorm=aux[2]*r;
	for (k=1; k<=n; k++) {
		r=a[k][k]-tammat(1,k-1,k,k,a,a);
		if (r <= epsnorm) {
			aux[3]=k-1;
			return;
		}
		a[k][k]=r=sqrt(r);
		for (j=k+1; j<=n; j++)
			a[k][j]=(a[k][j]-tammat(1,k-1,j,k,a,a))/r;
	}
	aux[3]=n;
}
