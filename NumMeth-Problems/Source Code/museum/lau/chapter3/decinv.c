void decinv(float **a, int n, float aux[])
{
	int *allocate_integer_vector(int, int);
	void free_integer_vector(int *, int);
	void dec(float **, int, float [], int []);
	void inv(float **, int, int []);
	int *p;

	p=allocate_integer_vector(1,n);
	dec(a,n,aux,p);
	if (aux[3] == n) inv(a,n,p);
	free_integer_vector(p,1);
}
