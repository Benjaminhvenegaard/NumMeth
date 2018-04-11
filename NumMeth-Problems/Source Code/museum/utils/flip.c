/* Program for converting any UNIX, PC, MAC end of line style file into any
   other PC, MAC end of line style file.
   Written 2/96 by Seth Finkelstein, sethf@mit.edu
   Syntax: name <-u | -p | -m> [filenames]
   If no file names are given, converts from standard input to standard output
   Otherwise, writes data from each file into a temporary file, and renames.
*/

#include <stdio.h>
#ifdef _MSC_VER
#include <fcntl.h>
#include <io.h>
#endif

#define NORMAL 0
#define CONTROL_J 10
#define CONTROL_M 13

#define UNIX 1
#define PC 2
#define MAC 3

int debug = 0;

void end_of_line(FILE *outfile,int kind)
{
  switch (kind) {
  case UNIX:
    fputc(CONTROL_J,outfile);
    break;
  case MAC:
    fputc(CONTROL_M,outfile);
    break;
  case PC:
    fputc(CONTROL_M,outfile);
    fputc(CONTROL_J,outfile);
    break;
  default:
    fprintf(stderr,"Unknown conversion, type = %d\n",kind);
  }
}

int convert(FILE *infile, FILE *outfile, int kind) {
  int i = 0, character, state = NORMAL;
  int controlj = 0, controlm = 0, nontext = 0;

  /* Don't pass through any end-of-line, instead write out the indicated one */
  while ((character = getc(infile)) != EOF) {
    if (character == CONTROL_J) {
      if (state != CONTROL_M) { end_of_line(outfile,kind); }
      state = CONTROL_J;
      controlj++;
    }
    else if (character == CONTROL_M) {
      end_of_line(outfile,kind);
      state = CONTROL_M;
      controlm++;
    }
    else {
      i++;
      if (putc(character,outfile) == EOF) {
	fprintf(stderr,"An error occured, while writing out character number %d, code %d\n",i,character);
      }
      state = NORMAL;
      if (character > 127 || (character > 13 && character < 26)) { nontext++; }
      if (nontext > 5) {
        fprintf(stderr,"Too many non-text characters seen, last one was %d, conversion aborted\n",character);
        return 1;
      }
    }
  }

  if (controlj == controlm || (controlj && !controlm) || (controlm && !controlj)) {
    return 0;
  }
  return 1;
}

int main(int argc, char **argv) {
  int i, result, kind = PC;
  FILE *infile = stdin, *outfile = stdout;
  char *inname, *outname;

  if (argc == 1) {
    fprintf(stderr,"Usage: %s <-u or -m or -p> [names of files]\n",argv[0]);
    fprintf(stderr,"Converts files into the specified end-of-line convention.\n");
    fprintf(stderr,"-u converts to UNIX (only control-j)\n");
    fprintf(stderr,"-m converts to MAC (only control-m)\n");
    fprintf(stderr,"-p converts to PC (control-m,control-j)\n");
    fprintf(stderr,"If no file names are given, converts from standard input to standard output.\n");
    fprintf(stderr,"Otherwise, writes data from each file into a temporary file, and renames.\n");
    exit(1);
  }

  for (i = 1; i < argc; i++) {
    if (!strcmp(argv[i],"-u")) { kind = UNIX; }
    else if (!strcmp(argv[i],"-p")) { kind = PC; }
    else if (!strcmp(argv[i],"-m")) { kind = MAC; }
    else if (!strcmp(argv[i],"-d")) { debug = 1; }
    else {
      inname = argv[i];
      infile = fopen(inname,"rb");
      if (infile == NULL) {
	fprintf(stderr,"Error opening file %s\n",inname);
	exit(10);
      }
      outname = tmpnam((char *) NULL);
      if (outname == NULL) {
	fprintf(stderr,"Error making temporary file name for %s\n",inname);
	exit(11);
      }
      outfile = fopen(outname,"wb");
      if (outfile == NULL) {
	fprintf(stderr,"Error processing %s, can't open temporary output file %s\n",inname,outname);
	exit(12);
      }
      if (debug) {
	fprintf(stderr,"Processing %s\n",inname);
      }
      result = convert(infile,outfile,kind); 
      fclose(outfile);
      fclose(infile);
      if (result == 0) {
        remove(inname);
        if (rename(outname,inname) != 0) {
	  fprintf(stderr,"Can't rename converted data file %s, to original file %s\n",outname,inname);
  	exit(13);
        }
      }
      else {
	fprintf(stderr,"Problem processing %s, appears to be a binary file, nothing done\n",inname);
        remove(outname);
      }
    }
  }
  if (infile == stdin) {
#ifdef _MSC_VER
    if (_setmode(_fileno(stdin),_O_BINARY) == -1) {
      fprintf(stderr,"Error setting binary mode for standard input\n");
    }
    if (_setmode(_fileno(stdout),_O_BINARY) == -1) {
      fprintf(stderr,"Error setting binary mode for standard output\n");
    }
#endif
    convert(infile,outfile,kind);
  }

  exit(0);
}
