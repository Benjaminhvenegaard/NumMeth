#include <math.h>

void chldec1(float a[], int n, float aux[])
{
	float vecvec(int, int, int, float [], float []);
	int j,k,kk,kj,low,up;
	float r,epsnorm;

	r=0.0;
	kk=0;
	for (k=1; k<=n; k++) {
		kk += k;
		if (a[kk] > r) r=a[kk];
	}
	epsnorm=aux[2]*r;
	kk=0;
	for (k=1; k<=n; k++) {
		kk += k;
		low=kk-k+1;
		up=kk-1;
		r=a[kk]-vecvec(low,up,0,a,a);
		if (r <= epsnorm) {
			aux[3]=k-1;
			return;
		}
		a[kk]=r=sqrt(r);
		kj=kk+k;
		for (j=k+1; j<=n; j++) {
			a[kj]=(a[kj]-vecvec(low,up,kj-kk,a,a))/r;
			kj +=j;
		}
	}
	aux[3]=n;
}
