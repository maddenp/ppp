#include <fcntl.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/un.h>
#include <unistd.h>

void fail()
{
  perror("ERROR");
  exit(1);
}

void term(int sock)
{
  int status;
  status=write(sock,"\n",1);
  if (status<0) fail();
}

void senderr(char *component)
{
  printf("Could not send %s to server\n",component);
  fail();
}

void usage(char *exe)
{
  printf("usage: %s <socket> <infile> <outfile>|<moddir> modinfo|passthrough|translate fixed|free [<dir>[:<dir>:...]]\n",exe);
  exit(1);
}

int main (int argc,char **argv)
{
  char *action,c,*dst_file,*exe,*form,*incdirs,*lensrcstr,*socket_file,*src_file,*src;
  int bytesread,fd,digits,lensrc,modinfo,sock,status;
  struct sockaddr_un server;
  struct stat fileinfo;

  exe=argv[0];

  if ((argc<6)||(argc>7)) usage(exe);

  // Make note of arguments.

  socket_file=argv[1];
  src_file=argv[2];
  dst_file=argv[3];
  action=argv[4];
  form=argv[5];
  incdirs=argv[6];

  // Verify existence of socket.

  if (access(socket_file,F_OK)<0)
  {
    printf("<socket> '%s' not found\n",src_file);
    usage(exe);
  }

  // Verify existence of source file.

  if (access(src_file,F_OK)<0)
  {
    printf("<infile> '%s' not found\n",src_file);
    usage(exe);
  }

  // Note whether only module info was requested.

  modinfo=strcmp(action,"modinfo")==0?1:0;

  // Validate requested action.

  if (                                    \
    strcmp(action,"passthrough") !=0 &&   \
    strcmp(action,"translate"  ) !=0 &&   \
    !modinfo                              \
    ) usage(exe);

  // Validate source form.

  if (strcmp(form,"fixed")!=0 && \
      strcmp(form,"free" )!=0    \
    ) usage(exe);


  // If only module info was requested, verify that the specified path exists
  // and is a directory.

  if (modinfo)
  {
    if ((fd=open(dst_file,O_RDONLY))<0)
    {
      printf("<moddir> output directory '%s' not found\n",dst_file);
      usage(exe);
    }
    if (fstat(fd,&fileinfo)<0)
    {
      printf("Could not get <moddir> '%s' info\n",dst_file);
      fail();
    }
    if (!(fileinfo.st_mode&S_IFDIR))
    {
      printf("'%s' requires output directory <moddir>, not '%s'\n",action,dst_file);
      usage(exe);
    }
    close(fd);
  }

  // Read in source file.

  if ((fd=open(src_file,O_RDONLY))<0)
  {
    printf("Could not open <infile> '%s'\n",src_file);
    fail();
  }
  if (fstat(fd,&fileinfo)<0)
  {
    printf("Could not get <infile> '%s' info\n",src_file);
    fail();
  }
  lensrc=fileinfo.st_size;
  if ((src=(char*)malloc(lensrc)+1)==NULL)
  {
    printf("Could not allocate input buffer for <infile> '%s'\n",src_file);
    fail();
  }
  if (read(fd,src,lensrc)<0)
  {
    printf("Could not read <infile> '%s'\n",src_file);
    fail();
  }
  close(fd);

  // Connect to server on socket.

  sock=socket(AF_UNIX,SOCK_STREAM,0);
  if (sock<0)
  {
    printf("Could not create socket\n");
    fail();
  }
  server.sun_family=AF_UNIX;
  strcpy(server.sun_path,socket_file);
  if (connect(sock,(struct sockaddr *)&server,sizeof(struct sockaddr_un))<0)
  {
    close(sock);
    printf("Could not connect to server on socket '%s'\n",socket_file);
    fail();
  }

  // Create source-length string.

  digits=(int)log10((double)lensrc);
  if ((lensrcstr=(char*)malloc(digits+2))==NULL)
  {
    printf("Could not allocate source-length string\n");
    fail();
  }
  sprintf(lensrcstr,"%d\n",lensrc-1);

  // Send header and source to server.

  status=write(sock,src_file,strlen(src_file));
  if (status<0) senderr("input file name");
  term(sock);
  status=write(sock,dst_file,strlen(dst_file));
  if (status<0) senderr("output path");
  term(sock);
  status=write(sock,action,strlen(action));
  if (status<0) senderr("action");
  term(sock);
  status=write(sock,form,strlen(form));
  if (status<0) senderr("form");
  term(sock);
  if (incdirs!=NULL) status=write(sock,incdirs,strlen(incdirs));
  if (status<0) senderr("includes");
  term(sock);
  status=write(sock,lensrcstr,strlen(lensrcstr));
  if (status<0) senderr("source-length string");
  status=write(sock,src,lensrc);
  if (status<0) senderr("source");

  // Open output file unless only module info was requested.

  if (!modinfo)
    if ((fd=open(dst_file,O_CREAT|O_TRUNC|O_WRONLY,S_IRUSR|S_IWUSR|S_IRGRP|S_IROTH))<0)
    {
      printf("Could not open output file '%s' for writing\n",dst_file);
      fail();
    }

  // Read response from server, and write to output file unless only module info
  // was requested. Server sends a NULL if the request failed.

  bytesread=0;
  while (read(sock,&c,1)>0)
  {
    if (c=='\0') break;
    ++bytesread;
    if (!modinfo) status=write(fd,&c,1);
  }
  if (!modinfo) close(fd);

  // Clean up and exit.

  close(sock);
  return 0;
}
