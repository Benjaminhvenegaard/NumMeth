void psdinvsvd(float **u, float val[], float **v, int m, int n,
					float em[])
{
	float *allocate_real_vector(int, int);
	void free_real_vector(float *, int);
	float matvec(int, int, int, float **, float []);
	int i,j;
	float min,vali,*x;

	x=allocate_real_vector(1,n);
	min=em[6];
	for (i=1; i<=n; i++)
		if (val[i] > min) {
			vali=1.0/val[i];
			for (j=1; j<=m; j++) u[j][i] *= vali;
		} else
			for (j=1; j<=m; j++) u[j][i]=0.0;
	for (i=1; i<=m; i++) {
		for (j=1; j<=n; j++) x[j]=u[i][j];
		for (j=1; j<=n; j++) u[i][j]=matvec(1,n,j,v,x);
	}
	free_real_vector(x,1);
}

