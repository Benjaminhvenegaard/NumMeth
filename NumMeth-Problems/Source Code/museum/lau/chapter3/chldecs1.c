void chldecsol1(float a[], int n, float aux[], float b[])
{
	void chldec1(float [], int, float []);
	void chlsol1(float [], int, float []);

	chldec1(a,n,aux);
	if (aux[3] == n) chlsol1(a,n,b);
}
