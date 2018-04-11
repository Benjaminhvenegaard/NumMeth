void comrowcst(int l, int u, int i,
					float **ar, float **ai, float xr, float xi)
{
	void commul(float, float, float, float, float *, float *);

	for (; l<=u; l++)
		commul(ar[i][l],ai[i][l],xr,xi,&ar[i][l],&ai[i][l]);
}
