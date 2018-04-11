float jfrac(int n, float a[], float b[])
{
	int i;
	float d;

	d=0.0;
	for (i=n; i>=1; i--) d=a[i]/(b[i]+d);
	return (d+b[0]);
}
