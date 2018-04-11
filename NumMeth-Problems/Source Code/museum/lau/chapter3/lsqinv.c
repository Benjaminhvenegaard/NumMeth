void lsqinv(float **a, int m, float aid[], int c[])
{
	void chlinv2(float **, int);
	void ichcol(int, int, int, int, float **);
	void ichrow(int, int, int, int, float **);
	void ichrowcol(int, int, int, int, float **);
	int i,ci;
	float w;

	for (i=1; i<=m; i++) a[i][i]=aid[i];
	chlinv2(a,m);
	for (i=m; i>=1; i--) {
		ci=c[i];
		if (ci != i) {
			ichcol(1,i-1,i,ci,a);
			ichrow(i+1,ci-1,i,ci,a);
			ichrow(ci+1,m,i,ci,a);
			w=a[i][i];
			a[i][i]=a[ci][ci];
			a[ci][ci]=w;
		}
	}
}
