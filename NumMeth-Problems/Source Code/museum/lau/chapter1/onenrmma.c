float onenrmmat(int lr, int ur, int lc, int uc, int *kc, float **a)
{
	float onenrmcol(int l, int u, int j, float **a);
	float r, max;

	max=0.0;
	*kc=lc;
	for (; lc<=uc; lc++) {
		r=onenrmcol(lr,ur,lc,a);
		if (r > max) {
			max=r;
			*kc=lc;
		}
	}
	return (max);
}
