void rnk1upd(float h[], int n, float v[], float c)
{
	void elmvec(int, int, int, float [], float [], float);
	int j,k;

	k=0;
	j=1;
	do {
		k++;
		elmvec(j,j+k-1,1-j,h,v,v[k]*c);
		j += k;
	} while (k < n);
}
