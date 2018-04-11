void duprowvec(int l, int u, int i, float **a, float b[])
{
	for (; l<=u; l++) a[i][l]=b[l];
}
