int homsol(float **a, int m, int n, float **v, float em[])
{
	float *allocate_real_vector(int, int);
	void free_real_vector(float *, int);
	int qrisngvaldec(float **, int, int, float [], float **, float []);
	void homsolsvd(float **, float [], float **, int, int);
	int i;
	float *val;

	val=allocate_real_vector(1,n);
	i=qrisngvaldec(a,m,n,val,v,em);
	if (i == 0) homsolsvd(a,val,v,m,n);
	free_real_vector(val,1);
	return i;
}

