void hshvecmat(int lr, int ur, int lc, int uc,
					float x, float u[], float **a)
{
	void elmcolvec(int, int, int, float **, float [], float);
	float tamvec(int, int, int, float **, float []);

	for (; lc<=uc; lc++) elmcolvec(lr,ur,lc,a,u,tamvec(lr,ur,lc,a,u)*x);
}
