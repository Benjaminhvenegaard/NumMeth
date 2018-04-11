float pol(int n, float x, float a[])
{
	float r;

	r=0.0;
	for (; n>=0; n--) r=r*x+a[n];
	return (r);
}
