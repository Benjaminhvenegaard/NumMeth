void comcolcst(int l, int u, int j,
					float **ar, float **ai, float xr, float xi)
{
	void commul(float, float, float, float, float *, float *);

	for (; l<=u; l++)
		commul(ar[l][j],ai[l][j],xr,xi,&ar[l][j],&ai[l][j]);
}
