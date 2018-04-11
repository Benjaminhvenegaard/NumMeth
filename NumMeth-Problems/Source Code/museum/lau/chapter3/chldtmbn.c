float chldetermbnd(float a[], int n, int w)
{
	int j,kk,w1;
	float p;

	w1=w+1;
	kk = -w;
	p=1.0;
	for (j=1; j<=n; j++) {
		kk += w1;
		p *= a[kk];
	}
	return (p*p);
}
