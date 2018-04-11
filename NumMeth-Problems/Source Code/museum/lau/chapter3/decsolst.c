void decsolsymtri(float diag[], float co[], int n,
						float aux[], float b[])
{
	void decsymtri(float [], float [], int, float []);
	void solsymtri(float [], float [], int, float []);

	decsymtri(diag,co,n,aux);
	if (aux[3] == n) solsymtri(diag,co,n,b);
}
