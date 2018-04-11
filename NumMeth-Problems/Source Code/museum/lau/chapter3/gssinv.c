void gssinv(float **a, int n, float aux[])
{
	int *allocate_integer_vector(int, int);
	void free_integer_vector(int *, int);
	void gsselm(float **, int, float [], int [], int []);
	float inv1(float **, int, int [], int [], int);
	int *ri,*ci;

	ri=allocate_integer_vector(1,n);
	ci=allocate_integer_vector(1,n);
	gsselm(a,n,aux,ri,ci);
	if (aux[3] == n) aux[9]=inv1(a,n,ri,ci,1);
	free_integer_vector(ri,1);
	free_integer_vector(ci,1);
}
