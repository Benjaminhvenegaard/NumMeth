void chldecinv2(float **a, int n, float aux[])
{
	void chldec2(float **, int, float []);
	void chlinv2(float **, int);

	chldec2(a,n,aux);
	if (aux[3] == n) chlinv2(a,n);
}
