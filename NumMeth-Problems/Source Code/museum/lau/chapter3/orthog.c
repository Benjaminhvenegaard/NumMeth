#include <math.h>

void orthog(int n, int lc, int uc, float **x)
{
	int *allocate_integer_vector(int, int);
	void free_integer_vector(int *, int);
	float tammat(int, int, int, int, float **, float **);
	void elmcol(int, int, int, int, float **, float **, float);
	int i,j,k;
	float normx;

	for (j=lc; j<=uc; j++) {
		normx=sqrt(tammat(1,n,j,j,x,x));
		for (i=1; i<=n; i++) x[i][j] /=normx;
		for (k=j+1; k<=uc; k++) elmcol(1,n,k,j,x,x,-tammat(1,n,k,j,x,x));
	}
}

