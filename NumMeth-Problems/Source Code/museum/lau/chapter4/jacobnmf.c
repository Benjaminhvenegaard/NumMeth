void jacobnmf(int n, int m, float x[], float f[], float **jac,
			float (*di)(int), void (*funct)(int, int, float[], float[]))
{
	float *allocate_real_vector(int, int);
	void free_real_vector(float *, int);
	int i,j;
	float step,aid,*f1;

	f1=allocate_real_vector(1,n);
	for (i=1; i<=m; i++) {
		step=(*di)(i);
		aid=x[i];
		x[i]=aid+step;
		step=1.0/step;
		(*funct)(n,m,x,f1);
		for (j=1; j<=n; j++) jac[j][i]=(f1[j]-f[j])*step;
		x[i]=aid;
	}
	free_real_vector(f1,1);
}
