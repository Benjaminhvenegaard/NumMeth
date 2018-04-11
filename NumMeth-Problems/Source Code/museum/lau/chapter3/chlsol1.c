void chlsol1(float a[], int n, float b[])
{
	float vecvec(int, int, int, float [], float []);
	float seqvec(int, int, int, int, float [], float []);
	int i,ii;

	ii=0;
	for (i=1; i<=n; i++) {
		ii += i;
		b[i]=(b[i]-vecvec(1,i-1,ii-i,b,a))/a[ii];
	}
	for (i=n; i>=1; i--) {
		b[i]=(b[i]-seqvec(i+1,n,ii+i,0,a,b))/a[ii];
		ii -= i;
	}
}
