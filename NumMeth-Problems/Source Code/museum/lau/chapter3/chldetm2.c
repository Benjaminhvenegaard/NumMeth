float chldeterm2(float **a, int n)
{
	int k;
	float d;

	d=1.0;
	for (k=1; k<=n; k++) d *= a[k][k];
	return (d*d);
}
