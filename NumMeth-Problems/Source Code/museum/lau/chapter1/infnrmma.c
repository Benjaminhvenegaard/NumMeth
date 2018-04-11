float infnrmmat(int lr, int ur, int lc, int uc, int *kr, float **a)
{
	float onenrmrow(int, int, int, float **);
	float r, max;

	max=0.0;
	*kr=lr;
	for (; lr<=ur; lr++) {
		r=onenrmrow(lc,uc,lr,a);
		if (r > max) {
			max=r;
			*kr=lr;
		}
	}
	return (max);
}
