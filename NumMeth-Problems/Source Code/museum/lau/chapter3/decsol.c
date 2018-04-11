void decsol(float **a, int n, float aux[], float b[])
{
	int *allocate_integer_vector(int, int);
	void free_integer_vector(int *, int);
	void sol(float **, int, int [], float []);
	void dec(float **, int, float [], int []);
	int *p;

	p=allocate_integer_vector(1,n);
	dec(a,n,aux,p);
	if (aux[3] == n) sol(a,n,p,b);
	free_integer_vector(p,1);
}
