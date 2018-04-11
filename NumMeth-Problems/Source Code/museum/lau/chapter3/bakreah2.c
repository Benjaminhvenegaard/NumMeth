void bakreahes2(float **a, int n, int n1, int n2, int index[],
						float **vec)
{
	float *allocate_real_vector(int, int);
	void free_real_vector(float *, int);
	float tamvec(int, int, int, float **, float []);
	void ichrow(int, int, int, int, float **);
	int i,l,k;
	float *u;

	u=allocate_real_vector(1,n);
	for (i=n; i>=2; i--) {
		for (k=i-2; k>=1; k--) u[k+1]=a[i][k];
		for (k=n1; k<=n2; k++) vec[i][k] += tamvec(2,i-1,k,vec,u);
		l=index[i];
		if (l > i) ichrow(n1,n2,i,l,vec);
	}
	free_real_vector(u,1);
}
