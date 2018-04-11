void mulcol(int l, int u, int i, int j, float **a, float **b, float x)
{
	for (; l<=u; l++) a[l][i]=b[l][j]*x;
}
