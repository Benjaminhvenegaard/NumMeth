void fulsymmatvec(int lr, int ur, int lc, int uc,
						float a[], float b[], float c[])
{
	float symmatvec(int, int, int, float [], float []);

	for (; lr<=ur; lr++) c[lr]=symmatvec(lc,uc,lr,a,b);
}
