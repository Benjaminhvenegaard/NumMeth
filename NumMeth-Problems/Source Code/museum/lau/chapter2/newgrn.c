void newgrn(int n, float x[], float a[])
{
	void elmvec(int, int, int, float [], float [], float);
	int k;

	for (k=n-1; k>=0; k--)
		elmvec(k,n-1,1,a,a,-x[k]);
}
