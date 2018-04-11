void dupmat(int l, int u, int i, int j, float **a, float **b)
{
	int k;

	for (; l<=u; l++)
		for (k=i; k<=j; k++) a[l][k]=b[l][k];
}
