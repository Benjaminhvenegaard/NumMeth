void ichseqvec(int l, int u, int il, int shift, float a[])
{
	float r;

	for (; l<=u; l++) {
		r=a[il];
		a[il]=a[l+shift];
		a[l+shift]=r;
		il += l;
	}
}
