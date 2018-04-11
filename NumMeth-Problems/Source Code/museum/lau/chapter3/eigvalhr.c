void eigvalhrm(float **a, int n, int numval, float val[], float em[])
{
	float *allocate_real_vector(int, int);
	void free_real_vector(float *, int);
	void hshhrmtrival(float **, int, float [], float [], float []);
	void valsymtri(float [], float [], int, int, int,
						float [], float []);
	float *d,*bb;

	d=allocate_real_vector(1,n);
	bb=allocate_real_vector(1,n-1);
	hshhrmtrival(a,n,d,bb,em);
	valsymtri(d,bb,n,1,numval,val,em);
	free_real_vector(d,1);
	free_real_vector(bb,1);
}

