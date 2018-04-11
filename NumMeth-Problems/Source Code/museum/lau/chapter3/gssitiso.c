void gssitisol(float **a, int n, float aux[], float b[])
{
	int *allocate_integer_vector(int, int);
	float **allocate_real_matrix(int, int, int, int);
	void free_integer_vector(int *, int);
	void free_real_matrix(float **, int, int, int);
	void gsselm(float **, int, float [], int [], int []);
	void itisol(float **, float **, int, float [],
					int [], int [], float []);
	void dupmat(int, int, int, int, float **, float **);
	int *ri,*ci;
	float **aa;

	ri=allocate_integer_vector(1,n);
	ci=allocate_integer_vector(1,n);
	aa=allocate_real_matrix(1,n,1,n);

	dupmat(1,n,1,n,aa,a);
	gsselm(a,n,aux,ri,ci);
	if (aux[3] == n) itisol(aa,a,n,aux,ri,ci,b);

	free_integer_vector(ri,1);
	free_integer_vector(ci,1);
	free_real_matrix(aa,1,n,1);
}
