void gsssolerb(float **a, int n, float aux[], float b[])
{
	int *allocate_integer_vector(int, int);
	void free_integer_vector(int *, int);
	void solelm(float **, int, int [], int [], float []);
	void gsserb(float **, int, float [], int [], int []);
	int *ri,*ci;

	ri=allocate_integer_vector(1,n);
	ci=allocate_integer_vector(1,n);
	gsserb(a,n,aux,ri,ci);
	if (aux[3] == n) solelm(a,n,ri,ci,b);
	free_integer_vector(ri,1);
	free_integer_vector(ci,1);
}
