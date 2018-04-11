int qrisngval(float **a, int m, int n, float val[], float em[])
{
	float *allocate_real_vector(int, int);
	void free_real_vector(float *, int);
	void hshreabid(float **, int, int, float [], float [], float []);
	int qrisngvalbid(float [], float [], int, float []);
	int i;
	float *b;

	b=allocate_real_vector(1,n);
	hshreabid(a,m,n,val,b,em);
	i=qrisngvalbid(val,b,n,em);
	free_real_vector(b,1);
	return i;
}
