void hshrowmat(int lr, int ur, int lc, int uc, int i,
					float x, float **u, float **a)
{
	float matmat(int, int, int, int, float **, float **);
	void elmcolrow(int, int, int, int, float **, float **, float);

	for (; lc<=uc; lc++) elmcolrow(lr,ur,lc,i,a,u,matmat(lr,ur,i,lc,u,a)*x);
}
