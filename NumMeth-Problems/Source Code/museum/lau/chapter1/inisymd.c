#include <math.h>

void inisymd(int lr, int ur, int shift, float a[], float x)
{
	shift=abs(shift);
	ur += shift+1;
	shift += lr;
	lr += ((shift-3)*shift)/2;
	lr += shift;
	while (shift < ur) {
		a[lr]=x;
		shift++;
		lr += shift;
	}
}
