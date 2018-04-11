void mulvec(int l, int u, int shift, float a[], float b[], float x)
{
	for (; l<=u; l++) a[l]=b[l+shift]*x;
}
