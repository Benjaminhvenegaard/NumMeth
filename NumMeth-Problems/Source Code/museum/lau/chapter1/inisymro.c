void inisymrow(int l, int u, int i, float a[], float x)
{
	int k;

	if (l <= i) {
		k=((i-1)*i)/2;
		l += k;
		k += (u<i) ? u : i;
		for (; l<=k; l++) a[l]=x;
		l=i+1;
	}
	if (u > i) {
		k=((l-1)*l)/2+i;
		do {
			a[k]=x;
			l++;
			k += l-1;
		} while (l <= u);
	}
}

