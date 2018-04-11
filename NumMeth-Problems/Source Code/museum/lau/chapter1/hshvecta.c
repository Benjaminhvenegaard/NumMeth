void hshvectam(int lr, int ur, int lc, int uc,
					float x, float u[], float **a)
{
	float matvec(int, int, int, float **, float []);
	void elmrowvec(int, int, int, float **, float [], float);

	for (; lr<=ur; lr++) elmrowvec(lc,uc,lr,a,u,matvec(lc,uc,lr,a,u)*x);
}
