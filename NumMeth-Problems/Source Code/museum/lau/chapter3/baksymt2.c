void baksymtri2(float **a, int n, int n1, int n2, float **vec)
{
	float tammat(int, int, int, int, float **, float **);
	void elmcol(int, int, int, int, float **, float **, float);
	int j,k;
	float w;

	for (j=2; j<=n; j++) {
		w=a[j][j];
		if (w < 0.0)
			for (k=n1; k<=n2; k++)
				elmcol(1,j-1,k,j,vec,a,tammat(1,j-1,j,k,a,vec)*w);
	}
}
