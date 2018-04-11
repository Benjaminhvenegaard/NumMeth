void chldecsolbnd(float a[], int n, int w, float aux[], float b[])
{
	void chldecbnd(float [], int, int, float []);
	void chlsolbnd(float [], int, int, float []);

	chldecbnd(a,n,w,aux);
	if (aux[3] == n) chlsolbnd(a,n,w,b);
}
