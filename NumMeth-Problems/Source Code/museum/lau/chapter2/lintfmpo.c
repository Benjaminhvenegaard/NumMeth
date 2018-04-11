void lintfmpol(float p, float q, int n, float a[])
{
	void norderpol(int, int, float, float []);
	int k;
	float ppower;

	norderpol(n,n,q,a);
	ppower=p;
	for (k=1; k<=n; k++) {
		a[k] *= ppower;
		ppower *= p;
	}
}
