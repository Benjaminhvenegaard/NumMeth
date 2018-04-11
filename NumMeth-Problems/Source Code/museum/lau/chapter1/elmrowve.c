void elmrowvec(int l, int u, int i, float **a, float b[], float x)
{
	for (; l<=u; l++) a[i][l] += b[l]*x;
}
