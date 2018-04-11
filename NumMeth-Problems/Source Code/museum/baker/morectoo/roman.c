/* print integer in Roman form

from More C Tools for Scientists and Engineers by L. Baker
*/

#define NULL '\000'

main()
{
int in;
char out[50];
while(1){
scanf("%d",&in);printf("echo %d\n",in);
roman(in,out);printf("roman: %s\n",out); 
alpha(in,out);printf("alpha: %s\n",out);
}
}

char wh[7]="IVXLCDM";
/* no D-overbar, M-overbar*/
roman( in,out)
int in; char out[];
{
int num,i,j,div,k,l;
char *point,chr;
num=in;
point=out;
*point=NULL;
div=1000;
if(num>3999)
	{printf(" warn %d too big\n",num);
	num=num%4000;
	}
for(k=3;k>=0;k--)
	{
	i=num/div;   num=num%div;
    switch (i)
    	{
    	case 0 : break;
    	case 5 : *point= wh[2*k+1];
    			point++;
    			break;
		case 9 : *point= wh[2*k];
				point++;
				*point=wh[2*k+2];
				point++;
				break;
		case 4 : *point=wh[2*k];point++;
				 *point=wh[2*k+1];point++;
				 break;
		default:
				if(i>5)
					{
					*point=wh[2*k+1];
 					i-=5;point++;
 					}
				for(l=0;l<i;l++){
 						*point=wh[2*k];point++;
 						}
    	}
	div=div/10;
	}
*point=NULL;
}

alpha(in,out)
int in;char *out;
{
char *pointer;
int kt,i,j,div,l;
pointer=out;
i=in;
if(i<27)	*(pointer++)= i+64;
else
	{
	j=i;div=1;
	for(kt=0;(j/=26)>0;kt++)div*=26;
	for(j=0;j<=kt;j++)
		{
		l=i/div;
		i%=div;
		*pointer= l+64;
		pointer++;
		div/=26;
		}
	}
*(pointer)=NULL;return;
}

