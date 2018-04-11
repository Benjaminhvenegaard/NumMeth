void inv(float **a, int n, int p[])
{
	float *allocate_real_vector(int, int);
	void free_real_vector(float *, int);
	float matmat(int, int, int, int, float **, float **);
	void ichcol(int, int, int, int, float **);
	void dupcolvec(int, int, int, float **, float []);
	int j,k,k1;
	float r,*v;

	v=allocate_real_vector(1,n);
	for (k=n; k>=1; k--) {
		k1=k+1;
		for (j=n; j>=k1; j--) {
			a[j][k1]=v[j];
			v[j] = -matmat(k1,n,k,j,a,a);
		}
		r=a[k][k];
		for (j=n; j>=k1; j--) {
			a[k][j]=v[j];
			v[j] = -matmat(k1,n,j,k,a,a)/r;
		}
		v[k]=(1.0-matmat(k1,n,k,k,a,a))/r;
	}
	dupcolvec(1,n,1,a,v);
	for (k=n-1; k>=1; k--) {
		k1=p[k];
		if (k1 != k) ichcol(1,n,k,k1,a);
	}
	free_real_vector(v,1);
}
