void dupvecrow(int l, int u, int i, float a[], float **b)
{
	for (; l<=u; l++) a[l]=b[i][l];
}
