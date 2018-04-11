void fulmatvec(int lr, int ur, int lc, int uc,
					float **a, float b[], float c[])
{
	float matvec(int, int, int, float **, float []);

	for (; lr<=ur; lr++) c[lr]=matvec(lc,uc,lr,a,b);
}
