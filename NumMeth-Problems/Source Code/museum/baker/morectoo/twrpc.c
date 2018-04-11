/* typewriter program
from More C Tools for Scientists and Engineers by L. Baker*/
/* select ONE OF THE FOLLOWING:*/
#define TURBOC 1
/*
#define DESMET
#define MICROC
#define CPM
*/

#ifdef TURBOC
#include <conio.h>
#include <dos.h>
#include <stdio.h>
#include <string.h>
#include <io.h>
#define write( stream, string, count) fwrite( string ,1, count , stream)
#define index( string, chr) strchr(string,chr)
#endif

#define NULL 0
#define EOF -1
#define O_RDWR 2
#define O_RDONLY 0
#define O_WRONLY 3


#define ESC 27
#define SPACE ' '
#define BACKSP '\10'
#define BELL 7
#define KONFIG 30
#define KONFM  KONFIG-1
/* OCTAL 10 = DECIMAL 8 */
#define isascii(x)  (x<127 && x>31)
/*#define put(x) bdos(6,x)*/
/* us unbuffered i/o terminal input, device output. buffered i/o files*/
int cnfig[KONFM],length;
int flagi=0,flagt=0,flags=0,flagho=0,flagin=0,flagot=0,coln;/*globals*/
int lmargin=1,rmargin=72,flaga=0;
#ifdef TURBOC
FILE *fi,*fo,*diskin,*diskout,*config;
#endif
#ifdef DESMET
int  *fi,*fo,*diskin,*diskout,*config;
#endif
char filei[40],fileo[40],*chrr;
char line[80],tline[80],*newl,*newlp,*newls,newlc[6];
int bufsize,n_rw;
/* flags
	i  1 if incremental
	s  1 if serial i/o
	ho 1 if add h.o. bit
	in 1 if file input
	ot 1 if file output  */


confg(){
int i,yesno;

config=fopen("CONFTW.DAT","r+");
if(config){printf(" type D for disk config., other for current\n");
		yesno=get();
	if(yesno==68||yesno||100)
	{for(i=0;i<KONFIG;i++) fscanf(config,"%d",&(cnfig[i]));};}
	else
	{ cnfig[0]=26;cnfig[1]=0;/*defaults-clear*/
	  cnfig[2]=ESC;cnfig[3]=61;cnfig[4]=32;cnfig[5]=32;/*cursor*/
	  cnfig[7]=10;cnfig[8]=13;cnfig[9]=0;/*parallel cr/lf*/
	 cnfig[10]=13;cnfig[11]=0;cnfig[12]=0;/* serial cr/lf*/
	cnfig[13]=ESC;cnfig[14]=91;cnfig[15]=0;cnfig[16]=0;
	cnfig[17]=ESC;cnfig[18]=93;cnfig[19]=0;cnfig[20]=0;
	 for(i=21;i<KONFIG;i++)cnfig[i]=0;
	}

printf(" do you want to (re)config terminal? Y or N\n");
	yesno=get();
if(yesno==89||yesno==121) confgt();
printf(" do you want to (re)config parallel printer? Y or N\n");
	yesno=get();
if(yesno==89||yesno==121) confgp(0);
printf(" do you want to (re)config serial printer? Y or N\n");
	yesno=get();
if(yesno==89||yesno==121) confgp(1);
printf(" do you want these changes made permanent?Y or N\n");
	yesno=get();
if(yesno!=89&&yesno!=121)return;
config=fopen("CONFTW.DAT","w+");/* this takes place of rewind*/
for(i=0;i<KONFIG;i++)fprintf(config,"%d ",cnfig[i]);
fprintf(config,"%d %d %d %d %d %d %d\n",
flagi,flagt,flags,flagho,flagin,flagot,flaga);
fclose(config);
clearh();
margin();
menu();
colno();
return;
}

main(argc,argv)
int argc;
char *argv[];
{
char *chr,ch;
int chri,chro,tab=8,pos,tabs=8,printer;
register int i;
/* screen codes- hex
	adm 3a conventions-default-
	7	beep
	C	cursor rt
	D	return
       17 	clear to end of scr
       18	clear to end of line(24 decimal)
       1E	home
       1A	clear and home (26 decimal)
esc	=       row+20H,col+20H position
esc	E	insert line
esc	R	delete line
esc	A	display lower case as lower case,i.e. cancel esc-G
esc	G	lower case as Greek  */
/* initialize*/
#ifdef DESMET
scr_setup();
#endif
	config=fopen("CONFTW.DAT","r+");if(config==0)confg();
		else
		   {for(i=0;i<KONFIG;i++)
			fscanf(config,"%d",&(cnfig[i]))	;
			fscanf(config,"%d %d %d %d %d %d %d",
			&flagi,&flagt,&flags,&flagho,&flagin,&flagot,&flaga);}
	fclose(config);

	fo=fopen("lst:","w");
	fi=open("con:",O_RDWR);
	strcpy(tline,
"1...5....0....5....0....5....0....5....0....5....0....5....0....5....0....5...."
);
	margin();
	newlp=newlc; newls=&(newlc[3]);
	newl=newlp;

	fo=fopen("LST:","w");/* default to LPT, CON:=CRT: */
	if(fo==NULL)fo=fopen("LPT:","w");
	if(fo==NULL) {printf(" trouble opening LST\n");}
	clearh();
	cursor(1,coln);
/*process  parameters */
	for(i=1;i<argc;i++){
		chrr=argv[i];
/*	ch=argv[i][1]; printf(" parameter=%s,%c%c\n",argv[i],ch,*chrr);*/
		if( *chrr=='-'){
		for(chrr++;*chrr!='\0';chrr++){
			switch(*chrr){
			case 'I':
				flagi=1;
				incr();
				break;
			case 'S':
				flags=1;
				fo=fopen("com1:","w");
				newl=newls;
				break;
			case 'P':
				;
				break;
			case 'C':
				confg();
				break;
			default:
				argc=0;
				printf(" illegal option %c\n",*chr);
				break;
			}/*END SWITCH*/
		}/*END FOR*/
	 } /* end if */
	}/*END WHILE*/
		for(i=0;i<3;i++) newlc[i]=cnfig[7+i];
		menu();
		colno();
/* configure for serial printer*/
	if(flags) newl=newls;
/* now process input*/
/* reserved control inputs:
	^] screendump, if implemented-never gets here
	^A add ho bit toggle- initially off
	^B accept filename to output to
	^F accept filename to obtain input from
	^E incremental mode toggle-initially off
	^D  ^Zexit code (warm boot)
	del,backspace= eliminate chr if not incremental mode
	^G clear line
	^I tab (default=8 spaces)
	^H BACKSPACE
	^J R.C.
	^T TOGGLE TAB AT COL. AND SET TAB MODE TO <COL.
	^P RESET DEFAULT TAB TO INPUT SPACES
	^O overstrike- caveat: can mess up coln.
	^S toggle serial/parallel
	^R set right margin- merely rings bell when equalled or exceeded
	^L set left margin
	^K reconfigure terminal/ports
*/

	cursor(3,1);
	while ( (chri=get()) != 04){/* ^D=04*/
	if(isascii(chri))putchar(chri);/* display on screen, if appropriate*/
	if(coln>=rmargin) putchar(BELL);
	switch (chri){
	case 1:
		flagho=   flagho ? 0 : 1 ;
		break;
	case 2: /*readin filename,open as fo,write to it*/
		flagot=   flagot ? 0 : 1 ;
		if(flagot){filen(fileo);/* unbuffered i/o bad for files*/
				diskout= fopen(fileo ,"w");
			if(diskout==NULL)printf(" open error,file=%s",fileo);
		          }
		 else {i=fclose(diskout);printf(" close %d",i);}
		break;
	case 6:/* readin filename, open as fi. on eof (^z or -1)
			return to stream input */
		flagin=   flagin ? 0 : 1 ;
		if(flagin)filen(filei);
		 diskin= fopen(filei,"r");
		if(diskin==NULL)printf(" open error");
		break;
	case 5:
		/*send incremental toggle*/
		/*tp1 on serial needs none*/
		flagi= flagi ? 0 : 1 ;
		incr();
		break;
	case 11:
		confg();
		break;

	case 10:
	case 13:
		/* flush buffer output linefeed, cr */
		if(!flagi)
		{ strncpy( &(line[coln-1]),newl,3);coln++;
	/*	 line[coln-1]=10;line[coln]=13;line[++coln]='\0';*/
			if(flagot){/* write to disk file*/
				cursor(16,1);printf("%s",line);
				fprintf(diskout,"%s",line);
			 	  }
			else
				{/* write to line printer*/
				writelb(fo,line,coln-1);

				};
		 }/* end non-incremental */
		else	{if(!flagot){length=strlen(newl);
				writelb(fo,newl,length);}
			 else{
				i=fputs(newl ,diskout);
				if(i==EOF){printf(" fputs error");exit(0);}
			     }
			 }
			 /*fall thru */
	case 7:/* clear line*/
		clearh();/* clear and home*/
		menu();
		margin();
		colno();
		break;
	case -1:
	case 26:
		if(coln>1)writelb(fo,line,coln);/*GET INPUT CONSOLE*/
		if(flagin){ flagin=0;fo=open("con:",O_RDONLY);break;}
		else
		 { /*^z taken to mean end of input */
			break;}
	case 127:
	case 8  : 
		if(coln>1)coln--;
		cursor(3,coln);
		colno();
		break;
	case 19: /* serial parallel */
		flags= flags ? 0 : 1 ;
		if(flags)fo=fopen("com1:","w");
			else fo=fopen("lst","w");
		newl= flags ? newls : newlp ;
		break;
	case 15: /* overstrike, i.e., backspace */
		outpt(BACKSP);
		coln--;cursor(3,coln);colno();
		break;
	case 16: /* set tab value in spaces*/
		cursor(20,1);
		printf("expand tab (spaces)=");
		fscanf(fi,"%d",&(tabs));
		cursor(3,coln);
		break;
	case 12:/*set lmargin*/
		lmargin=coln;
		break;
	case 18:rmargin=coln;
		break;
	case 9: /*tab*/
		if(flagt) {/* jump to next tab stop*/
				chr=&( tline[coln]);
				chrr=index(chr,'T');
				if(!chrr)break;
				tab= chrr - chr+1;
			  }
		else tab=tabs;
			for(i=0;i<tab;i++) outpt(SPACE);				 	break;
	case 20: /* set tab*/
		pos=coln-1;
		if(tline[pos]!='T') /* set tab if not there*/
		{ flagt=1; tline[pos]='T';/* set tab*/break;}
		else{/* clear tab*/
			tline[pos]='.'; i=coln % 5 ;
			if(!i)
			{
			i= coln % 10;
			tline[pos]= i ? '5' : '0' ;
			}/* end if*/
		/*unset flagt?*/ flagt=0;
			for(i=0;i<80;i++)
			    flagt=(tline[i]=='T')? 1 :flagt  ;
			break;}/*end else*/

	default:
		if(flagho)chri=chri+128;
		outpt(chri);		
		break;
	 }	/*end Switch*/
	}/*end while*/

if(flagot){/* buffered i/o*/
		cursor(0,1); printf(" closing %s",fileo);
		/*debug*/ fprintf(diskout," typewriter l baker");
		i= fclose(diskout);
		printf(" close file=%s,%d",fileo,i);
	  }
clearh();
exit(0);
}/* end MAIN*/

outpt(chr)int chr;
{int err;
char chrr[3]={'\000','\000','\000'};

/* output if immediate, else put in buffer */
if(flagi){/* incremental output*/
		if(flagot)err= putc(chr,fo);
		else	{err=putc(chr,fo);}
	coln++;
	if(err==-1)printf(" error on incr. output\n");
	 }
else
	{/*place in buffer first column */
	line[coln-1]=chr;coln++;
	if(coln>80) {puts(" line buffer overflow");return;}
	}
colno();return;
}

colno(){
/* output column number on screen*/
char info[80]; int length;
info[0]='\0';
cursor( 0,1);
length=strlen(info);
if(flagi) {sprintf(&(info[length])," incremental");length=strlen(info);}
if(flagho){sprintf(&(info[length])," ho bit");length=strlen(info);}
if(flagin){sprintf(&(info[length])," input from %s",filei);length=strlen(info);}
if(flagot){sprintf(&(info[length])," output to %s",fileo);length=strlen(info);}
if(flagt){sprintf(&(info[length])," tabset");length=strlen(info);}

	else
	{if(flags&&(!flagot))sprintf(&(info[length])," serial     ");
	else
	   if(!flagot) sprintf(&(info[length])," parallel  ");};
printf(" column=%d %s lmargin=%d",coln,info,lmargin);
cursor(3,coln);
return;
}

filen(buffer)
char *buffer;
{
cursor( 20,1);
printf(" filename=");
fscanf(fi,"%s",buffer);
cursor(3,coln);
return;
}

incr(){	/* toggle increm. mode command to printer if
			its nec8023A  on  parallel*/
char strng[4];int nwrite,length,index;
if(flagot)return;/*ignore if file output*/
/*serial if flags=1, parallel if zero*/
index= (flags? 21:13) + (flagi? 0:4);
for(length=0;length<4;length++){strng[length]=cnfig[index+length];
				if(!strng[length])break;}
if(length>0){nwrite=writelb(fo,strng,length);
			cursor(22,1);
printf(" incremental print toggled\n");
			}
margin();
return;
}

menu(){
cursor(10,1);
/*PUTS SEEMS TO DO NEWLINE*/
puts(" menu: contrl-");
puts(" A- ho bit toggle, B outfile F infile E incr. toggle");
puts(" D end G line clear T toggle tab mode P tab space ct ");
puts(" S serial/parallel toggle O overstrike prev. Z endfile");
puts(" K reconfigure L leftmargin R rightmargin set");
puts(" typewriter copyright 1984 Louis Baker");
cursor(2,1);
puts(tline);
return;
}

margin(){
int j;
register i;
coln=1;
if(lmargin==1)return;
/*clear to cursor,output */
for (i=0;i<(lmargin-1);i++) {
		line[i]=SPACE;
		outpt(SPACE);/* nb- outpt incr. coln*/
		}
colno();return;
}


confgt(){
int yesno;
/*------terminal control codes-------
0-1 clear(&home code for screen) 1 is 0 if 1 chr code
cursor positioning-
2 typically ESC=27
3  = 61 for ADM3A
4 line+  (32)
5 coln+  (32)  NB- CAVEAT purdum gives -1,31 instead!
*/
/*CAVEAT- CONFIGURATIONS 2,3 UNTESTED- NO GUARANTEES,
	ESPECIALLY FOR CURSOR POSITIONING! BASED ON
	TABLE P.110 PURDUMS BOOK WHICH IS ERRONEOUS FOR
	ADM3A (=ADM3?). ASSUME his col,row numbers start
	from 1 (mine start from 0, so adders are 1 more than his*/
printf(" select printer number from menu\n");
printf(" 0=not in menu,setup by hand\n");
printf(" 1=KAYPRO,ADM3A\n");
printf(" 2= Heath,Zenith\n");
printf(" 3=SOROC,Televideo\n");
printf(" 4=IBM PC, BIOS CALLS\n");
printf(" 5=ANSI.SYS");
/* for IBM PC, rewrite routines clearh and cursor
to do BIOS 10H interrupt; see, e.g., Abel's book 
Assembler for the IBM PC,Reston Publ.,p.101 */
fscanf(fi,"%d",&yesno);
switch(yesno)
{
case 4:
case 1: return;
case 2: cnfig[0]=27;cnfig[1]=69;
	cnfig[3]=27;cnfig[4]=89;cnfig[5]=32;cnfig[6]=32;
	break;
case 3: cnfig[0]=27;cnfig[1]=42;
	cnfig[3]=27;cnfig[4]=61;cnfig[5]=32;cnfig[6]=32;
	break;
case 5:
	flaga=1;
	break;
case 0:
default:
printf(" input integer for screenclear\n");
	fscanf(fi,"%d",&(cnfig[0]));
printf(" enter 2nd code for screenclear or else 0\n");
	fscanf(fi,"%d",&(cnfig[1]));
printf("enter 2 integer cursor position seq\n");
	fscanf(fi,"%d%d",&(cnfig[2]),&(cnfig[3]));
printf("enter 2 digits added to row and col\n");
	fscanf(fi,"%d%d",&(cnfig[4]),&(cnfig[5]));
}
return;
}

confgp(printer) int printer;{/* printer=0 parallel, else 1 serial*/
int i,j,k,l,m;
/*---------printer control codes------------
			parallel	serial
cr/lf,cr,lf/cr		7-9		10-12
incr toggle:
	 on	 	13-16		21-24
 	off		17-20		25-28
AGAIN, FIRST 0 IN STRINGS TERMINATES SEQUENCE*/

printf(" type integer cr/lf seq integer/line term. by 0\n");
for(i=0;i<3;i++){
		printf(" enter chr %d\n",i+1);
		fscanf(fi,"%d",&j);
		cnfig[i+7+printer*3]=j;
		if(!j)break;
		}/* end for to input cr/lf seq. */
printf(" end line send sq: %d %d %d\n",
cnfig[7+printer*3],cnfig[printer*3+8],cnfig[printer*3+9]);
for(i=0;i<3;i++) newlc[i]=cnfig[7+printer*3+i];
/* INCREMENTAL TOGGLE*/
for(m=0;m<2;m++){
		  if(m==0)printf(" type incrm. on");
		  else	 printf(" type incrm. off seq.");
			for(i=0;i<4;i++)
			{fscanf(fi,"%d",&j);
			l=m?17:13;
			cnfig[printer*8+i+l]=j;
			if(j==0)break;
			}/*end for i on/off */
		}/* end for m to input cr/lf seq. */
return;
}
/*MACHINE DEPENDENT SECTION*/

putn(numb) int numb;
{
/* numb 0-99 assumed*/
int hi,lo;
hi=numb/10;
lo=numb % 10;
if(hi)putchar(hi+48);
putchar(lo+48);
}

cursor(row,co)
/*row,column*/
int row,co;
{
if(!flaga)
{
#ifdef DESMET
scr_rowcol(row,co);
#endif
#ifdef TURBOC
gotoxy(row,co);
#endif
#ifdef CPM
putchar(cnfig[2]);putchar(cnfig[3]);
putchar(cnfig[4]+row);putchar(cnfig[5]+co);
#endif
}
else
	{/* ansi cmd seq.*/
	putchar(ESC);putchar('[');putn(row);putchar(';');
	putn(co);putchar('H');/* f or H */
	}
return;
}

clearh(){
if(flaga)
	{
	putchar(ESC);putchar('[');putchar('2');putchar('J');
	cursor(1,1);
	}
else
	{
#ifdef DESMET
scr_clr();
#endif
#ifdef TURBOC
clrscr();
#endif
#ifdef CPM
putchar(cnfig[0]);putchar(cnfig[1]);
#endif
	}
return;
}

int get(){
int x;/* ff=255*/
#ifdef DESMET
while((x=scr_csts())==0){};
#endif
#ifdef TURBOC
x=getch();
#endif
/* bdos returns 0 if chr not input- we must do the polling*/
return(x);
}

writelb(stream,string,count)
int count; char *string;
FILE *stream;
{
if(stream)write(stream,string,count);

else
   fprintf(stdprn,"%s",string);
}
/*END MACHINE DEPENDENT SECTION*/


