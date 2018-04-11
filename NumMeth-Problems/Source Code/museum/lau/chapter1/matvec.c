float matvec(int l, int u, int i, float **a, float b[])
{
	int k;
	float s;

	s=0.0;
	for (k=l; k<=u; k++) s += a[i][k]*b[k];
	return (s);
}
