float chldeterm1(float a[], int n)
{
	int k,kk;
	float d;

	d=1.0;
	kk=0;
	for (k=1; k<=n; k++) {
		kk += k;
		d *= a[kk];
	}
	return (d*d);
}
