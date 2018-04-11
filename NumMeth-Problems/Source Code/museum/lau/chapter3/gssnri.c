void gssnri(float **a, int n, float aux[], int ri[], int ci[])
{
	void gsselm(float **, int, float [], int [], int []);
	float onenrminv(float **, int);

	gsselm(a,n,aux,ri,ci);
	if (aux[3] == n) aux[9]=onenrminv(a,n);
}
