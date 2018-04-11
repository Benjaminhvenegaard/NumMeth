#include <stdio.h>
#include <stdlib.h>
#include <stdio.h>
#include <sys/stat.h>
#include <string.h>
#include <unistd.h>
#ifdef LOCKS
#include <sys/file.h>
#endif

#define MAXLEN 256

#if defined(MSDOS) || defined(_MSDOS)
#define NR_RANLIST_DEFAULT "\\NRRANLST"
#define NR_RANSTATE_DEFAULT "\\NRRANSTT"
#else
#define NR_RANLIST_DEFAULT "/u7/ran/filelist"
#define NR_RANSTATE_DEFAULT "/u7/ran/lock"
#endif

FILE *fopen_safe(char *name, char *mode) {
  FILE *fp;

  fp = fopen(name,mode);
  if (fp == NULL) {
    fprintf(stderr,"Cannot open the file named %s for ",name);
    if (mode[0] == 'r') {
      fprintf(stderr,"reading\n");
    }
    else if (mode[0] == 'w') {
      fprintf(stderr,"writing\n");
    }
    else {
      fprintf(stderr,"the unknown mode %s\n",mode);
    }
    exit(3);
  }
  return fp;
}
 
char *get_next_ran_file(char *nr_ranfile, char *nr_ranlist, char *nr_ranstate) {
  char line[MAXLEN];
  FILE *nr_ranlistp;
  size_t nr_ranfile_len;

  nr_ranlistp = fopen_safe(nr_ranlist,"r");
  nr_ranfile_len = strlen(nr_ranfile);
  while (fgets(line,MAXLEN,nr_ranlistp) != NULL) {
    if (*nr_ranfile == '\0') {
      if (sscanf(line,"%s",nr_ranfile) != 1) {
	fprintf(stderr,"Couldn't find filename in line %s in file list %s\n",line,nr_ranlist);
	exit(4);
      }
      return nr_ranfile;
    }
    if(!strncmp(line,nr_ranfile,nr_ranfile_len)) { /* found the corresponding line */
      if (fgets(line,MAXLEN,nr_ranlistp) == NULL) {
	fprintf(stderr,"No more files in %s\n",nr_ranlist);
	fprintf(stderr,"Perhaps you should remove the file %s to start over\n",nr_ranstate);
	exit(5);
      }
      if (sscanf(line,"%s",nr_ranfile) != 1) {
	fprintf(stderr,"Couldn't find filename in line %s in file list %s\n",line,nr_ranlist);
	exit(6);
      }
      return nr_ranfile;
    }
  }
  fclose(nr_ranlistp);
  fprintf(stderr,"Could not get file to succeed \"%s\" in file list \"%s\"\n",nr_ranfile,nr_ranlist);
  fprintf(stderr,"Extraneous spaces and tabs in %s may cause this search to fail!\n",nr_ranlist);
  exit(7);
}


unsigned char *getran(unsigned char *arr, size_t len) {
  char nr_ranlist[MAXLEN], nr_ranstate[MAXLEN], nr_ranfile[MAXLEN], *stmp;
  unsigned char *ret;
  struct stat statinfo;
  FILE *nr_ranstatep, *nr_ranfilep;
  size_t count, have_left, to_get, need = len;
  long where_we_are;
    
  if (len == 0) return arr;
  ret = arr;
  
  if ((stmp = getenv("NRRANLIST")) != NULL) {
    strcpy(nr_ranlist,stmp);
  }
  else if ((stmp = getenv("HOME")) != NULL) {
    sprintf(nr_ranlist,"%s/.nr_ranlist",stmp);
    if (access(nr_ranlist,F_OK) != 0){
      strcpy(nr_ranlist,NR_RANLIST_DEFAULT);
    }
  }
  else {
    strcpy(nr_ranlist,NR_RANLIST_DEFAULT);
  }

  if ((stmp = getenv("NRRANSTATE")) != NULL) {
    strcpy(nr_ranstate,stmp);
  }
  else if ((stmp = getenv("HOME")) != NULL) {
    sprintf(nr_ranstate,"%s/.nr_ranstate",stmp);
  }
  else {
    strcpy(nr_ranstate,NR_RANSTATE_DEFAULT);    
  }

  if ((nr_ranstatep = fopen(nr_ranstate,"r")) == NULL) {
    *nr_ranfile = '\0';
    get_next_ran_file(nr_ranfile, nr_ranlist, nr_ranstate);
    where_we_are = 0;
  }
  else {
    if (fscanf(nr_ranstatep,"%s %ld",nr_ranfile, &where_we_are) != 2) {
      fprintf(stderr,"Could not find current file and place in state file %s\n",nr_ranstate);
      exit(8);
    }
    fclose(nr_ranstatep);
  }

  nr_ranfilep = fopen_safe(nr_ranfile,"rb");
  fstat(fileno(nr_ranfilep),&statinfo);
  fseek(nr_ranfilep, where_we_are, SEEK_SET);
  
  need = len;
  while (need > 0) {
    have_left = statinfo.st_size - where_we_are;
    to_get = ((need < have_left) ? need : have_left);
      
    count = fread( (void *) arr, sizeof(char), to_get, nr_ranfilep);
    if (count != to_get) {
      fprintf(stderr,"Error reading %ld bytes, only got %ld bytes\n", (long) to_get, (long) count);
      exit(9);
    }
    need -= to_get;
    have_left -= to_get;
    where_we_are += to_get;
    if (need > 0) {
      arr += to_get;
      if (have_left == 0) {
	fclose(nr_ranfilep);
	get_next_ran_file(nr_ranfile, nr_ranlist, nr_ranstate);
	nr_ranfilep = fopen_safe(nr_ranfile,"r");
	fstat(fileno(nr_ranfilep),&statinfo);
	where_we_are = 0;
      }
    }
  }
  nr_ranstatep = fopen_safe(nr_ranstate,"w");
#ifdef LOCKS
  if (flock(fileno(nr_ranstatep), LOCK_SH) != 0) {
    fprintf(stderr,"Error in setting lock for file %s\n",nr_ranstate);
  }
#endif
  fprintf(nr_ranstatep,"%s %ld\n",nr_ranfile,where_we_are);
#ifdef LOCKS
  if (flock(fileno(nr_ranstatep), LOCK_UN) != 0) {
    fprintf(stderr,"Error in removing lock for file %s\n",nr_ranstate);
  }
#endif
  fclose(nr_ranstatep);
  fclose(nr_ranfilep);
  return ret;
}
