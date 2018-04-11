#include <math.h>

void itisol(float **a, float **lu, int n, float aux[],
				int ri[], int ci[], float b[])
{
	float *allocate_real_vector(int, int);
	void free_real_vector(float *, int);
	void solelm(float **, int, int [], int [], float []);
	void inivec(int, int, float [], float);
	void dupvec(int, int, int, float [], float []);
	int i,j,iter,maxiter;
	float maxerx,erx,nrmres,nrmsol,r,rr,*res,*sol;
	double dtemp;

	res=allocate_real_vector(1,n);
	sol=allocate_real_vector(1,n);
	maxerx=erx=aux[10];
	maxiter=aux[12];
	inivec(1,n,sol,0.0);
	dupvec(1,n,0,res,b);
	iter=1;
	do {
		solelm(lu,n,ri,ci,res);
		erx=nrmsol=nrmres=0.0;
		for (i=1; i<=n; i++) {
			r=res[i];
			erx += fabs(r);
			rr=sol[i]+r;
			sol[i]=rr;
			nrmsol += fabs(rr);
		}
		erx /= nrmsol;
		for (i=1; i<=n; i++) {
			dtemp = -(double)b[i];
			for (j=1; j<=n; j++)
				dtemp += (double)a[i][j]*(double)sol[j];
			r = -dtemp;
			res[i]=r;
			nrmres += fabs(r);
		}
		iter++;
	} while ((iter <= maxiter) && (maxerx < erx));
	dupvec(1,n,0,b,sol);
	aux[11]=erx;
	aux[13]=nrmres;
	free_real_vector(res,1);
	free_real_vector(sol,1);
}
