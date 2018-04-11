void derpol(int n, int k, float x, float a[])
{
	void norderpol(int, int, float, float []);
	int j;
	float fac;

	fac=1.0;
	norderpol(n,k,x,a);
	for (j=2; j<=k; j++) {
		fac *= j;
		a[j] *=fac;
	}
}
