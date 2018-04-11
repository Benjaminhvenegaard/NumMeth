int qrisym(float **a, int n, float val[], float em[])
{
	float *allocate_real_vector(int, int);
	void free_real_vector(float *, int);
	void tfmsymtri2(float **, int, float [], float [], float [],
						float []);
	void tfmprevec(float **, int);
	int qrisymtri(float **, int, float [], float [], float [], float []);
	int i;
	float *b,*bb;

	b=allocate_real_vector(1,n);
	bb=allocate_real_vector(1,n);
	tfmsymtri2(a,n,val,b,bb,em);
	tfmprevec(a,n);
	i=qrisymtri(a,n,val,b,bb,em);
	free_real_vector(b,1);
	free_real_vector(bb,1);
	return i;
}

