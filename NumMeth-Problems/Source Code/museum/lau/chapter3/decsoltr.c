void decsoltri(float sub[], float diag[], float super[], int n,
					float aux[], float b[])
{
	void dectri(float [], float [], float [],	int, float []);
	void soltri(float [], float [], float [], int, float []);

	dectri(sub,diag,super,n,aux);
	if (aux[3] == n) soltri(sub,diag,super,n,b);
}
