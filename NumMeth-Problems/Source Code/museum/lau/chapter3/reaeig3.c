int reaeig3(float **a, int n, float em[], float val[], float **vec)
{
	int *allocate_integer_vector(int, int);
	float *allocate_real_vector(int, int);
	void free_integer_vector(int *, int);
	void free_real_vector(float *, int);
	void tfmreahes(float **, int, float [], int []);
	void bakreahes2(float **, int, int, int, int [], float **);
	void eqilbr(float **, int, float [], float [], int []);
	void baklbr(int, int, int, float [], int [], float **);
	void reascl(float **, int, int, int);
	int reaqri(float **, int, float [], float [], float **);
	int i,*ind,*ind0;
	float *d;

	ind=allocate_integer_vector(1,n);
	ind0=allocate_integer_vector(1,n);
	d=allocate_real_vector(1,n);
	eqilbr(a,n,em,d,ind0);
	tfmreahes(a,n,em,ind);
	i=reaqri(a,n,em,val,vec);
	if (i == 0) {
		bakreahes2(a,n,1,n,ind,vec);
		baklbr(n,1,n,d,ind0,vec);
		reascl(vec,n,1,n);
	}
	free_integer_vector(ind,1);
	free_integer_vector(ind0,1);
	free_real_vector(d,1);
	return i;
}
