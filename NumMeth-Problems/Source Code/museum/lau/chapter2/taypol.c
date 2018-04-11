void taypol(int n, int k, float x, float a[])
{
	int i,j,nm1;
	float xj,aa,h;

	if (x != 0.0) {
		xj=1;
		for (j=1; j<=n; j++) {
			xj *= x;
			a[j] *= xj;
		}
		aa=a[n];
		nm1=n-1;
		for (j=0; j<=k; j++) {
			h=aa;
			for (i=nm1; i>=j; i--) h = a[i] += h;
		}
	} else {
		for (; k>=1; n--) a[k]=0;
	}
}
