void rowcst(int l, int u, int i, float **a, float x)
{
	for (; l<=u; l++) a[i][l] *= x;
}
