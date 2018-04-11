float determsym2(float detaux[], int n, int aux[])
{
	int i;
	float det;

	if (aux[5] > 0)
		det=0.0;
	else {
		det=1.0;
		for (i=1; i<=n; i++) det *= detaux[i];
	}
	return (det);
}
