void hshcolmat(int lr, int ur, int lc, int uc, int i,
					float x, float **u, float **a)
{
	void elmcol(int, int, int, int, float **, float **, float);
	float tammat(int, int, int, int, float **, float **);

	for (; lc<=uc; lc++) elmcol(lr,ur,lc,i,a,u,tammat(lr,ur,lc,i,a,u)*x);
}
