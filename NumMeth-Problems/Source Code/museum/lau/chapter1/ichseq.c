void ichseq(int l, int u, int il, int shift, float a[])
{
	float r;

	for (; l<=u; l++) {
		r=a[il];
		a[il]=a[il+shift];
		a[il+shift]=r;
		il += l;
	}
}
