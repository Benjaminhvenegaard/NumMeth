void baklbrcom(int n, int n1, int n2, float d[], int inter[],
					float **vr, float **vi)
{
	void baklbr(int, int, int, float [], int [], float **);

	baklbr(n,n1,n2,d,inter,vr);
	baklbr(n,n1,n2,d,inter,vi);
}
