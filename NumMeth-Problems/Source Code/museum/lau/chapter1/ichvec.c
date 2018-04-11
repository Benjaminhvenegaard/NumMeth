void ichvec(int l, int u, int shift, float a[])
{
	float r;

	for (; l<=u; l++) {
		r=a[l];
		a[l]=a[l+shift];
		a[l+shift]=r;
	}
}
