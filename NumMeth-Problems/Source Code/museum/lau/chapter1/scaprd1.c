float scaprd1(int la, int sa, int lb, int sb, int n, float a[], float b[])
{
	int k;
	float s;

	s=0.0;
	for (k=1; k<=n; k++) {
		s += a[la]*b[lb];
		la += sa;
		lb += sb;
	}
	return (s);
}
