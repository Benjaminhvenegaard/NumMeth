void elmveccol(int l, int u, int i, float a[], float **b, float x)
{
	for (; l<=u; l++) a[l] += b[l][i]*x;
}
