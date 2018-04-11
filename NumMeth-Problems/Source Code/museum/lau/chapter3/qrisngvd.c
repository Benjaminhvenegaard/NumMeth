int qrisngvaldec(float **a, int m, int n, float val[], float **v,
						float em[])
{
	float *allocate_real_vector(int, int);
	void free_real_vector(float *, int);
	void hshreabid(float **, int, int, float [], float [], float []);
	void psttfmmat(float **, int, float **, float []);
	void pretfmmat(float **, int, int, float []);
	int qrisngvaldecbid(float [], float [], int, int, float **,
							float **, float []);
	int i;
	float *b;

	b=allocate_real_vector(1,n);
	hshreabid(a,m,n,val,b,em);
	psttfmmat(a,n,v,b);
	pretfmmat(a,m,n,val);
	i=qrisngvaldecbid(val,b,m,n,a,v,em);
	free_real_vector(b,1);
	return i;
}
