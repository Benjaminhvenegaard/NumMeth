void ichcol(int l, int u, int i, int j, float **a)
{
	float r;

	for (; l<=u; l++) {
		r=a[l][i];
		a[l][i]=a[l][j];
		a[l][j]=r;
	}
}
