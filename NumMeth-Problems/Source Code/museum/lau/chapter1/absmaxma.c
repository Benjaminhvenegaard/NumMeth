float absmaxmat(int lr, int ur, int lc, int uc, int *i, int *j, float **a)
{
	float infnrmcol(int, int, int, int *, float **);
	int ii;
	float r, max;

	max=0.0;
	*i=lr;
	*j=lc;
	for (; lc<=uc; lc++) {
		r=infnrmcol(lr,ur,lc,&ii,a);
		if (r > max) {
			max=r;
			*i=ii;
			*j=lc;
		}
	}
	return (max);
}
