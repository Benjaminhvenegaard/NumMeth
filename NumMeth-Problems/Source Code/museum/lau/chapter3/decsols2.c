void decsolsym2(float **a, int n, float b[], float tol, int aux[])
{
	int *allocate_integer_vector(int, int);
	float *allocate_real_vector(int, int);
	void free_integer_vector(int *, int);
	void free_real_vector(float *, int);
	void decsym2(float **, int, float, int [], int [], float []);
	void solsym2(float **, int, float [], int [], float []);
	int *p;
	float *detaux;

	p=allocate_integer_vector(1,n);
	detaux=allocate_real_vector(1,n);
	decsym2(a,n,tol,aux,p,detaux);
	if (aux[5] == 0) solsym2(a,n,b,p,detaux);
	free_integer_vector(p,1);
	free_real_vector(detaux,1);
}
