float vecvec(int l, int u, int shift, float a[], float b[])
{
	int k;
	float s;

	s=0.0;
	for (k=l; k<=u; k++) s += a[k]*b[k+shift];
	return (s);
}
