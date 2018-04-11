void hshcoltam(int lr, int ur, int lc, int uc, int i,
					float x, float **u, float **a)
{
	float matmat(int, int, int, int, float **, float **);
	void elmrowcol(int, int, int, int, float **, float **, float);

	for (; lr<=ur; lr++) elmrowcol(lc,uc,lr,i,a,u,matmat(lc,uc,lr,i,a,u)*x);
}
