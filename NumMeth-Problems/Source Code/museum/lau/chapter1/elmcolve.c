void elmcolvec(int l, int u, int i, float **a, float b[], float x)
{
	for (; l<=u; l++) a[l][i] += b[l]*x;
}
