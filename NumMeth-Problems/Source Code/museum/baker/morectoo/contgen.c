/* generates a test file PLOT.DAT for contour plots

from More C tools for Scientists and Engineers by L. Baker

DEPENDENCIES:

	NONE
*/
#include <stdio.h>

FILE *fout;

main(argc,argv)
int argc;char **argv;
{
int i,j,nx=25,ny=25;float x[51],y[51],z,u,v;
fout=fopen("PLOT.DAT","w");

for(i=0;i<=nx;i++) x[i]=i*.04;
for(i=0;i<=ny;i++) y[i]=i*.04;

fprintf(fout," %d %d \n",nx,ny);
for(i=0;i<nx;i++) fprintf(fout," %f\n",(x[i]));
for(i=0;i<ny;i++) fprintf(fout," %f\n",(y[i]));
for(i=0;i<nx;i++) 
	{
	for(j=0;j<ny;j++)
		{
			u=x[i];
			v=y[j];
			z= u*u+v*v;/* circular contours*/
			fprintf(fout," %f \n",z);
		} 
	}
fclose(fout);
}
