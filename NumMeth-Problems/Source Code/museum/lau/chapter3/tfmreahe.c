#include <math.h>

void tfmreahes(float **a, int n, float em[], int index[])
{
	float *allocate_real_vector(int, int);
	void free_real_vector(float *, int);
	float matvec(int, int, int, float **, float []);
	float matmat(int, int, int, int, float **, float **);
	void ichcol(int, int, int, int, float **);
	void ichrow(int, int, int, int, float **);
	int i,j,j1,k,l;
	float s,t,machtol,macheps,norm,*b;

	b=allocate_real_vector(0,n-1);
	macheps=em[0];
	norm=0.0;
	for (i=1; i<=n; i++) {
		s=0.0;
		for (j=1; j<=n; j++) s += fabs(a[i][j]);
		if (s > norm) norm=s;
	}
	em[1]=norm;
	machtol=norm*macheps;
	index[1]=0;
	for (j=2; j<=n; j++) {
		j1=j-1;
		l=0;
		s=machtol;
		for (k=j+1; k<=n; k++) {
			t=fabs(a[k][j1]);
			if (t > s) {
				l=k;
				s=t;
			}
		}
		if (l != 0) {
			if (fabs(a[j][j1]) < s) {
				ichrow(1,n,j,l,a);
				ichcol(1,n,j,l,a);
			} else
				l=j;
			t=a[j][j1];
			for (k=j+1; k<=n; k++) a[k][j1] /=t;
		} else
			for (k=j+1; k<=n; k++) a[k][j1]=0.0;
		for (i=1; i<=n; i++)
			b[i-1] = a[i][j] +=
				((l == 0) ? 0.0 : matmat(j+1,n,i,j1,a,a)) -
				matvec(1,(j1 < i-2) ? j1 : i-2,i,a,b);
		index[j]=l;
	}
	free_real_vector(b,0);
}
