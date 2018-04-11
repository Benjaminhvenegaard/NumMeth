void grnnew(int n, float x[], float a[])
{
	int k,l;

	for (k=n-1; k>=0; k--)
		for (l=n-1; l>=n-1-k; l--) a[l] += a[l+1]*x[n-1-k];
}
