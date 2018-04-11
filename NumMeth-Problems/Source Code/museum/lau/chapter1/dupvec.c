void dupvec(int l, int u, int shift, float a[], float b[])
{
	for (; l<=u; l++) a[l]=b[l+shift];
}
