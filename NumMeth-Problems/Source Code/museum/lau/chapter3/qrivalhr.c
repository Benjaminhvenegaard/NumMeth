int qrivalhrm(float **a, int n, float val[], float em[])
{
	float *allocate_real_vector(int, int);
	void free_real_vector(float *, int);
	void hshhrmtrival(float **, int, float [], float [], float []);
	int qrivalsymtri(float [], float [], int, float []);
	int i;
	float *bb;

	bb=allocate_real_vector(1,n);
	hshhrmtrival(a,n,val,bb,em);
	bb[n]=0.0;
	i=qrivalsymtri(val,bb,n,em);
	free_real_vector(bb,1);
	return i;
}

