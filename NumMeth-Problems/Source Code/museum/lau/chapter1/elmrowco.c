void elmrowcol(int l, int u, int i, int j, float **a, float **b, float x)
{
	for (; l<=u; l++) a[i][l] += b[l][j]*x;
}
