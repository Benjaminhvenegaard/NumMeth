void chlsol2(float **a, int n, float b[])
{
	float matvec(int, int, int, float **, float []);
	float tamvec(int, int, int, float **, float []);
	int i;

	for (i=1; i<=n; i++) b[i]=(b[i]-tamvec(1,i-1,i,a,b))/a[i][i];
	for (i=n; i>=1; i--) b[i]=(b[i]-matvec(i+1,n,i,a,b))/a[i][i];
}
