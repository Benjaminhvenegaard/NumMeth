int qrivalsym1(float a[], int n, float val[], float em[])
{
	float *allocate_real_vector(int, int);
	void free_real_vector(float *, int);
	void tfmsymtri1(float [], int, float [], float [], float [],
						float []);
	int qrivalsymtri(float [], float [], int, float []);
	int i;
	float *b,*bb;

	b=allocate_real_vector(1,n);
	bb=allocate_real_vector(1,n);
	tfmsymtri1(a,n,val,b,bb,em);
	i=qrivalsymtri(val,bb,n,em);
	free_real_vector(b,1);
	free_real_vector(bb,1);
	return i;
}

