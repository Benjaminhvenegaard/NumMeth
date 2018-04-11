void sclcom(float **ar, float **ai, int n, int n1, int n2)
{
	void comcolcst(int, int, int, float **, float **, float, float);
	int i,j,k;
	float s,r,arij,aiij;

	for (j=n1; j<=n2; j++) {
		s=0.0;
		for (i=1; i<=n; i++) {
			arij=ar[i][j];
			aiij=ai[i][j];
			r=arij*arij+aiij*aiij;
			if (r > s) {
				s=r;
				k=i;
			}
		}
		if (s != 0.0) comcolcst(1,n,j,ar,ai,ar[k][j]/s,-ai[k][j]/s);
	}
}
