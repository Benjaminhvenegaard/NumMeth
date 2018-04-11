#include <math.h>

float onenrminv(float **a, int n)
{
	float *allocate_real_vector(int, int);
	void free_real_vector(float *, int);
	float matvec(int, int, int, float **, float []);
	int i,j;
	float norm,max,aid,*y;

	y=allocate_real_vector(1,n);
	norm=0.0;
	for (j=1; j<=n; j++) {
		for (i=1; i<=n; i++)
			y[i]=(i < j) ? 0 :
					((i == j) ? 1.0/a[i][i] : -matvec(j,i-1,i,a,y)/a[i][i]);
		max=0.0;
		for (i=n; i>=1; i--) {
			aid = y[i] -= matvec(i+1,n,i,a,y);
			max += fabs(aid);
		}
		if (norm < max) norm=max;
	}
	free_real_vector(y,1);
	return (norm);
}
