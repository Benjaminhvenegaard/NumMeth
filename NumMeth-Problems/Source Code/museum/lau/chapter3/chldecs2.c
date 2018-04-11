void chldecsol2(float **a, int n, float aux[], float b[])
{
	void chldec2(float **, int, float []);
	void chlsol2(float **, int, float []);

	chldec2(a,n,aux);
	if (aux[3] == n) chlsol2(a,n,b);
}
