void tfmprevec(float **a, int n)
{
	float tammat(int, int, int, int, float **, float **);
	void elmcol(int, int, int, int, float **, float **, float);
	int i,j,j1,k;
	float ab;

	j1=1;
	for (j=2; j<=n; j++) {
		for (i=1; i<=j1-1; i++) a[i][j1]=0.0;
		for (i=j; i<=n; i++) a[i][j1]=0.0;
		a[j1][j1]=1.0;
		ab=a[j][j];
		if (ab < 0)
			for (k=1; k<=j1; k++)
				elmcol(1,j1,k,j,a,a,tammat(1,j1,j,k,a,a)*ab);
		j1=j;
	}
	for (i=n-1; i>=1; i--) a[i][n]=0.0;
	a[n][n]=1.0;
}
