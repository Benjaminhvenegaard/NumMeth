float seqvec(int l, int u, int il, int shift, float a[], float b[])
{
	float s;

	s=0.0;
	for (; l<=u; l++) {
		s += a[il]*b[l+shift];
		il += l;
	}
	return (s);
}
