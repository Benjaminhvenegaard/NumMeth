void mulrow(int l, int u, int i, int j, float **a, float **b, float x)
{
	for (; l<=u; l++) a[i][l]=b[j][l]*x;
}
