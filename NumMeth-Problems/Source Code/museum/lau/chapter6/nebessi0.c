#include <math.h>

float nonexpbessi0(float x)
{
	if (x == 0.0) return 1.0;
	if (fabs(x) <= 15.0) {
		float bessi0(float);
		return exp(-fabs(x))*bessi0(x);
	} else {
		int i;
		float sqrtx,br,br1,br2,z,z2,numerator,denominator;
		static float ar1[4]={0.2439260769778, -0.115591978104435e3,
			0.784034249005088e4, -0.143464631313583e6};
		static float ar2[4]={1.0, -0.325197333369824e3,
			0.203128436100794e5, -0.361847779219653e6};
		x=fabs(x);
		sqrtx=sqrt(x);
		br1=br2=0.0;
		z=30.0/x-1.0;
		z2=z+z;
		for (i=0; i<=3; i++) {
			br=z2*br1-br2+ar1[i];
			br2=br1;
			br1=br;
		}
		numerator=z*br1-br2+0.346519833357379e6;
		br1=br2=0.0;
		for (i=0; i<=3; i++) {
			br=z2*br1-br2+ar2[i];
			br2=br1;
			br1=br;
		}
		denominator=z*br1-br2+0.865665274832055e6;
		return (numerator/denominator)/sqrtx;
	}
}
