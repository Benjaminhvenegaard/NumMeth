void fleupd(float h[], int n, float v[], float w[], float c1, float c2)
{
	int i,j,k;
	float vk,wk;

	k=0;
	j=1;
	do {
		k++;
		vk = -w[k]*c1+v[k]*c2;
		wk=v[k]*c1;
		for (i=0; i<=k-1; i++) h[i+j] += v[i+1]*vk-w[i+1]*wk;
		j += k;
	} while (k < n);
}
