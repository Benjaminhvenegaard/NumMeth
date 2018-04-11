void resvec(int lr, int ur, int lc, int uc,
				float **a, float b[], float c[], float x)
{
	float matvec(int, int, int, float **, float []);

	for (; lr<=ur; lr++) c[lr]=matvec(lc,uc,lr,a,b)+c[lr]*x;
}
