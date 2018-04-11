void solsvdund(float **u, float val[], float **v, int m, int n,
					float x[], float em[])
{
	float *allocate_real_vector(int, int);
	void free_real_vector(float *, int);
	float matvec(int, int, int, float **, float []);
	float tamvec(int, int, int, float **, float []);
	int i;
	float min,*x1;

	x1=allocate_real_vector(1,n);
	min=em[6];
	for (i=1; i<=n; i++)
		x1[i] = (val[i] <= min) ? 0.0 : tamvec(1,n,i,v,x)/val[i];
	for (i=1; i<=m; i++)	x[i] = matvec(1,n,i,u,x1);
	free_real_vector(x1,1);
}
