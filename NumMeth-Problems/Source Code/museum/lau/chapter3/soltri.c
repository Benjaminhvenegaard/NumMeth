void soltri(float sub[], float diag[], float super[], int n, float b[])
{
	int i;
	float r;

	r = b[1] /= diag[1];
	for (i=2; i<=n; i++) r=b[i]=(b[i]-sub[i-1]*r)/diag[i];
	for (i=n-1; i>=1; i--) r = b[i] -= super[i]*r;
}
