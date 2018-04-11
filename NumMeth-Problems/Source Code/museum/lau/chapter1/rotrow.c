void rotrow(int l, int u, int i, int j, float **a, float c, float s)
{
	float x, y;

	for (; l<=u; l++) {
		x=a[i][l];
		y=a[j][l];
		a[i][l]=x*c+y*s;
		a[j][l]=y*c-x*s;
	}
}
