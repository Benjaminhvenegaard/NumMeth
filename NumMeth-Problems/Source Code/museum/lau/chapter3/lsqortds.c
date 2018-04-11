void lsqortdecsol(float **a, int n, int m, float aux[],
						float diag[], float b[])
{
	int *allocate_integer_vector(int, int);
	float *allocate_real_vector(int, int);
	void free_integer_vector(int *, int);
	void free_real_vector(float *, int);
	void lsqortdec(float **, int, int, float [], float [], int []);
	void lsqdglinv(float **, int, float [], int [], float []);
	void lsqsol(float **, int, int, float [], int [], float []);
	int *ci;
	float *aid;

	ci=allocate_integer_vector(1,m);
	aid=allocate_real_vector(1,m);
	lsqortdec(a,n,m,aux,aid,ci);
	if (aux[3] == m) {
		lsqdglinv(a,m,aid,ci,diag);
		lsqsol(a,n,m,aid,ci,b);
	}
	free_integer_vector(ci,1);
	free_real_vector(aid,1);
}
