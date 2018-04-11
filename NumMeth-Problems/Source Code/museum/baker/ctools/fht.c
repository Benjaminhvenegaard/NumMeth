/*
Discrete Fast Hartley Transform

based upon BASIC code in 2nd revised edition of R. N. Bracewell's
"The Fourier Transform and its Applications" (McGraw-Hill,1986).

various modifications have been made to reduce storage required
(transform is done in place, unlike Bracewell) and reduce 
computation (e.g., cos and sin only computed for n4+1 values,
not n as in Bracewell).


from Handbook of C tools for Scientists and Engineers by L. Baker

*/
#define min(a,b) (((a)<(b))? (a): (b))
#define max(a,b) (((a)>(b))? (a): (b))
#define abs(x) ((x)?  (x):-(x))
#define DOFOR(i,to) for(i=0;i<to;i++)
#define INDEX(i,j)  [j]
/* above def is same as for 1-d array on index j only*/

main(argc,argv) int argc;char **argv;
{int i,n=16,l,log2();
double invn,exp();
int m[256];
double f[256],r[256], w[256],wi[256],x[256];
n=8;/* bracewell testcase*/
DOFOR(i,n){
	f[i]= i+1;
	};
DOFOR(i,n){printf(" input %e ",f[i]) ;printf("\n");}
fhtinit(w,wi,m,n,256);
fht(f,r,x,w,wi,m,n);
DOFOR(i,n){printf(" %e %e %e",r[i],x[i],f[i]) ;printf("\n");}
n=16;
DOFOR(i,n){
	f[i]= exp(-.25*(i));
	};
f[0]=.5;
DOFOR(i,n){printf(" input %e ",f[i]) ;printf("\n");}
fhtinit(w,wi,m,n,256);
fht(f,r,x,w,wi,m,n);
printf(" fft(re) fft(Im)  ht\n");
DOFOR(i,n){printf(" %e %e %e",r[i],x[i],f[i]) ;printf("\n");}
invn=16.;
printf(" scaled for comparison with fft output\n");
DOFOR(i,n){ r[i]*=invn;x[i]*=invn;
             printf(" %e %e\n",r[i],x[i]);
             }
exit(0);
}



int log2(n) int n;
{
int i;
i=-1;/* will return -1 if n<=0 */
while(1)
	{
	if(n==0)break;
	n=n>>1;
	i++;
	}
return(i);
}

#define twopi 6.283185307

int cols,logn,n2,n4,log1;

fhtinit(c,s,m,n,columns) int n,m[],columns;
	double c[],s[];
{
int i,log2(),l;
double cos(),sin(),con,angl;
cols=columns;
con=twopi/n;
m[0]=1; logn=log2(n);log1=logn-1;
n2=n>>1;n4=n2>>1;
DOFOR(i,logn)
	{
        m[i+1]=(m[i])<<1;
	}
;
angl=0.;
DOFOR(i,n4+1)
	{c[i]=cos(angl);s[i]=sin(angl);
	angl+=con;
	 	}
return;
}


fht(f,r,x,c,s,m,n) int n,m[];
double x[],r[],c[],s[],f[];
{
double temp,temp2,temp3,temp4,temp1,invn;
int t,u,i,j,k,l,l1,d,e,ss,so,s2,exponent,p;
invn=1./n;
/*reorder-(i.e.,permute)*/
if(logn>1)
{
j=-1;i=-1;
do	{
	do{
	i++;
	k=logn;
	do	{
                 k--;
                 j-=m[k];
		}while(j>=-1);
	j+=m[k+1];
	}while(i<=j);
	temp=f[i+1];
        f[i+1]= f[j+1];
        f[j+1]=temp;
	} while(i<n-3);
};
/* transform*/
/*2 element dht unrolled inner loop*/
for(i=0;i<=n-2;i+=2)
	{
	temp= f[i];
	temp2=f[i+1];
	f[i]=temp+temp2;
	f [i+1]= temp-temp2;
        }
l=1;
if(logn!=1)
{
l=2;
/*4 element dht unrolled inner loop*/
for(i=0;i<=n-4;i+=4)
	{
	temp= f[i];
	temp2=f [i+2];
	temp3=f [i+3];
	temp1=f [i+1];
	f [i]=temp+temp2;
	f [i+1]= temp1+temp3;
	f [i+2]=temp-temp2;
	f [i+3]=temp1-temp3;
        }
/*remaining loop*/
}
if(logn>2)
{
ss=4;
u=log1;
for(l=2;l<logn;l++)
	{
         s2=ss<<1;
         u--;
         so=m[u-1];
         for(k=0;k<n;k+=s2)
         	{
         	i=k;
         	d=i+ss;
			temp= f[i];
			temp2=f [d];
		

         	f [i]=temp+temp2;
         	f [d]=temp-temp2;
         	l1=d-1;
         	for (j=so;j<=n4;j+=so)
         		{
         		i++;
         		d=i+ss;
         		e=l1+ss;
         		temp= f[d]*c[j]+f[e]*s[j];
			temp2=f[d]*s[j]-f[e]*c[j];
			temp3=f[i];
			temp1=f[ l1];
			f[i]=temp3+temp;
			f[d]=temp3-temp;
          		f[l1]=temp1+temp2;
			f[e]=temp1-temp2;
                       	l1--;
                        }
                e=l1+ss;
         	}
         ss=s2;
	}
}
f[0]*=invn;
/*dft*/
r[0]=f[0]/*+ f INDEX(l,0)*/;
x[0]=0.;
for(i=1;i<=n2;i++)
	{
         temp2=f[n-i]*invn;
         temp= f[i]*invn;
         r[i]= (temp2+temp)*.5;
         x[i]= (temp2-temp)*.5;
		f[n-i]=temp2;
		f[i]= temp;
	}
/* reflect for other components of fft*/
for(i=n2+1;i<n;i++)
	{
	j=n-i;
	r[i]=r[j];
	x[i]=-x[j];
	}
return;
}

