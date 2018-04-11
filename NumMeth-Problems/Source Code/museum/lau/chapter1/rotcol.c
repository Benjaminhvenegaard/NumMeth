void rotcol(int l, int u, int i, int j, float **a, float c, float s)
{
	float x, y;

	for (; l<=u; l++) {
		x=a[l][i];
		y=a[l][j];
		a[l][i]=x*c+y*s;
		a[l][j]=y*c-x*s;
	}
}
