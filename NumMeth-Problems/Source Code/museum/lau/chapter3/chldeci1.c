void chldecinv1(float a[], int n, float aux[])
{
	void chldec1(float [], int, float []);
	void chlinv1(float [], int);

	chldec1(a,n,aux);
	if (aux[3] == n) chlinv1(a,n);
}
