void fultamvec(int lr, int ur, int lc, int uc,
					float **a, float b[], float c[])
{
	float tamvec(int, int, int, float **, float []);

	for (; lc<=uc; lc++) c[lc]=tamvec(lr,ur,lc,a,b);
}
