void gsserb(float **a, int n, float aux[], int ri[], int ci[])
{
	void gsselm(float **, int, float [], int [], int []);
	float onenrminv(float **, int);
	void erbelm(int, float [], float);

	gsselm(a,n,aux,ri,ci);
	if (aux[3] == n) erbelm(n,aux,onenrminv(a,n));
}
