void homsolsvd(float **u, float val[], float **v, int m, int n)
{
	void ichcol(int, int, int, int, float **);
	int i,j;
	float x;

	for (i=n; i>=2; i--)
		for (j=i-1; j>=1; j--)
			if (val[i] > val[j]) {
				x=val[i];
				val[i]=val[j];
				val[j]=x;
				ichcol(1,m,i,j,u);
				ichcol(1,n,i,j,v);
			}
}

