void colcst(int l, int u, int j, float **a, float x)
{
	for (; l<=u; l++) a[l][j] *= x;
}
