void bakreahes1(float **a, int n, int index[], float v[])
{
	float *allocate_real_vector(int, int);
	void free_real_vector(float *, int);
	float matvec(int, int, int, float **, float []);
	int i,l;
	float w,*x;

	x=allocate_real_vector(1,n);
	for (i=2; i<=n; i++) x[i-1]=v[i];
	for (i=n; i>=2; i--) {
		v[i] += matvec(1,i-2,i,a,x);
		l=index[i];
		if (l > i) {
			w=v[i];
			v[i]=v[l];
			v[l]=w;
		}
	}
	free_real_vector(x,1);
}
