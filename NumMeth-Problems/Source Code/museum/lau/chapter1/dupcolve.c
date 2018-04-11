void dupcolvec(int l, int u, int j, float **a, float b[])
{
	for (; l<=u; l++) a[l][j]=b[l];
}

