void solbnd(float a[], int n, int lw, int rw, float m[],
				int p[], float b[])
{
	float vecvec(int, int, int, float [], float []);
	void elmvec(int, int, int, float [], float [], float);
	int f,i,k,kk,w,w1,w2,shift;
	float s;

	f=lw;
	shift = -lw;
	w1=lw-1;
	for (k=1; k<=n; k++) {
		if (f < n) f++;
		shift += w1;
		i=p[k];
		s=b[i];
		if (i != k) {
			b[i]=b[k];
			b[k]=s;
		}
		elmvec(k+1,f,shift,b,m,-s);
	}
	w1=lw+rw;
	w=w1+1;
	kk=(n+1)*w-w1;
	w2 = -1;
	shift=n*w1;
	for (k=n; k>=1; k--) {
		kk -= w;
		shift -= w1;
		if (w2 < w1) w2++;
		b[k]=(b[k]-vecvec(k+1,k+w2,shift,b,a))/a[kk];
	}
}
