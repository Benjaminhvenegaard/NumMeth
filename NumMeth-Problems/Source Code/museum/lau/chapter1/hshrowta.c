void hshrowtam(int lr, int ur, int lc, int uc, int i,
					float x, float **u, float **a)
{
	float mattam(int, int, int, int, float **, float **);
	void elmrow(int, int, int, int, float **, float **, float);

	for (; lr<=ur; lr++) elmrow(lc,uc,lr,i,a,u,mattam(lc,uc,lr,i,a,u)*x);
}
