void ichrowcol(int l, int u, int i, int j, float **a)
{
	float r;

	for (; l<=u; l++) {
		r=a[i][l];
		a[i][l]=a[l][j];
		a[l][j]=r;
	}
}
