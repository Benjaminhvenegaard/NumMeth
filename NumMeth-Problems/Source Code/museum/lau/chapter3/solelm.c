void solelm(float **a, int n, int ri[], int ci[], float b[])
{
	void sol(float **, int, int [], float []);
	int r,cir;
	float w;

	sol(a,n,ri,b);
	for (r=n; r>=1; r--) {
		cir=ci[r];
		if (cir != r) {
			w=b[r];
			b[r]=b[cir];
			b[cir]=w;
		}
	}
}
