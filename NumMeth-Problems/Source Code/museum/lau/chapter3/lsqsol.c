void lsqsol(float **a, int n, int m, float aid[], int ci[], float b[])
{
	float matvec(int, int, int, float **, float []);
	float tamvec(int, int, int, float **, float []);
	void elmveccol(int, int, int, float [], float **, float);
	int k,cik;
	float w;

	for (k=1; k<=m; k++)
		elmveccol(k,n,k,b,a,tamvec(k,n,k,a,b)/(aid[k]*a[k][k]));
	for (k=m; k>=1; k--) b[k]=(b[k]-matvec(k+1,m,k,a,b))/aid[k];
	for (k=m; k>=1; k--) {
		cik=ci[k];
		if (cik != k) {
			w=b[k];
			b[k]=b[cik];
			b[cik]=w;
		}
	}
}
