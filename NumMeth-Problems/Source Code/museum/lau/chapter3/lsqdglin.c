void lsqdglinv(float **a, int m, float aid[], int ci[], float diag[])
{
	float vecvec(int, int, int, float [], float []);
	float tamvec(int, int, int, float **, float []);
	int j,k,cik;
	float w;

	for (k=1; k<=m; k++) {
		diag[k]=1.0/aid[k];
		for (j=k+1; j<=m; j++) diag[j] = -tamvec(k,j-1,j,a,diag)/aid[j];
		diag[k]=vecvec(k,m,0,diag,diag);
	}
	for (k=m; k>=1; k--) {
		cik=ci[k];
		if (cik != k) {
			w=diag[k];
			diag[k]=diag[cik];
			diag[cik]=w;
		}
	}
}
