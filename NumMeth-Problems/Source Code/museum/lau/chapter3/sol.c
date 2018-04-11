void sol(float **a, int n, int p[], float b[])
{
	float matvec(int, int, int, float **, float []);
	int k,pk;
	float r;

	for (k=1; k<=n; k++) {
		r=b[k];
		pk=p[k];
		b[k]=(b[pk]-matvec(1,k-1,k,a,b))/a[k][k];
		if (pk != k) b[pk]=r;
	}
	for (k=n; k>=1; k--) b[k] -= matvec(k+1,n,k,a,b);
}
