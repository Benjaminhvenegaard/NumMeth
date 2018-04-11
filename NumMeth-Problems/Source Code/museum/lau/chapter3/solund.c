int solund(float **a, int m, int n, float x[], float em[])
{
	float *allocate_real_vector(int, int);
	float **allocate_real_matrix(int, int, int, int);
	void free_real_vector(float *, int);
	void free_real_matrix(float **, int, int, int);
	int qrisngvaldec(float **, int, int, float [], float **, float []);
	void solsvdund(float **, float [], float **, int, int,
						float [], float []);
	int i;
	float *val,**v;

	val=allocate_real_vector(1,n);
	v=allocate_real_matrix(1,n,1,n);
	i=qrisngvaldec(a,m,n,val,v,em);
	if (i == 0) solsvdund(a,val,v,m,n,x,em);
	free_real_vector(val,1);
	free_real_matrix(v,1,n,1);
	return i;
}

