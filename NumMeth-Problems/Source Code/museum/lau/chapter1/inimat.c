void inimat(int lr, int ur, int lc, int uc, float **a, float x)
{
	int j;

	for (; lr<=ur; lr++)
		for (j=lc; j<=uc; j++) a[lr][j]=x;
}
