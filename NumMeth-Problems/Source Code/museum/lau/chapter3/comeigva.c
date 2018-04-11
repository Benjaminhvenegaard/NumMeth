int comeigval(float **a, int n, float em[], float re[], float im[])
{
	int *allocate_integer_vector(int, int);
	float *allocate_real_vector(int, int);
	void free_integer_vector(int *, int);
	void free_real_vector(float *, int);
	void eqilbr(float **, int, float [], float [], int []);
	void tfmreahes(float **, int, float [], int []);
	int comvalqri(float **, int, float [], float [], float []);
	int i,*ind,*ind0;
	float *d;

	ind=allocate_integer_vector(1,n);
	ind0=allocate_integer_vector(1,n);
	d=allocate_real_vector(1,n);
	eqilbr(a,n,em,d,ind0);
	tfmreahes(a,n,em,ind);
	i=comvalqri(a,n,em,re,im);
	free_integer_vector(ind,1);
	free_integer_vector(ind0,1);
	free_real_vector(d,1);
	return i;
}
