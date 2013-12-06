#include <fcntl.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/un.h>
#include <unistd.h>

void fail(char *msg)
{
  perror(msg);
  exit(1);
}

void term(int sock)
{
  int status;
  status=write(sock,"\n",1);
  if (status<0) fail("sending term");
}

void usage(char *exe)
{
  printf("Usage: %s <socket> <infile> passthrough|translate fixed|free [dir[:dir:...]]\n",exe);
  exit(1);
}

int main (int argc,char **argv)
{
  char *action,c,*exe,*form,*incdirs,*lensrcstr,*socket_file,*source_file,*src;
  int bytesout,fd,digits,lensrc,sock,status;
  struct sockaddr_un server;
  struct stat fileinfo;

  exe=argv[0];

  if ((argc<5)||(argc>6)) usage(exe);

  socket_file=argv[1];
  source_file=argv[2];
  action=argv[3];
  form=argv[4];
  incdirs=argv[5];

  if ((strcmp("fixed",form)!=0)&&(strcmp("free",form)!=0)) usage(exe);

  if ((fd=open(source_file,O_RDONLY))<0) fail("opening infile");
  if (fstat(fd,&fileinfo)<0) fail("getting infile info");

  lensrc=fileinfo.st_size;

  if ((src=(char*)malloc(lensrc)+1)==NULL) fail("allocating 'src'");
  if (read(fd,src,lensrc)<0) fail("reading source");

  close(fd);

  sock=socket(AF_UNIX,SOCK_STREAM,0);
  if (sock<0) fail("opening socket");
  server.sun_family=AF_UNIX;
  strcpy(server.sun_path,socket_file);

  if (connect(sock,(struct sockaddr *)&server,sizeof(struct sockaddr_un))<0)
  {
    close(sock);
    fail("connecting to server");
  }

  status=write(sock,source_file,strlen(source_file));
  if (status<0) fail("sending infile");
  term(sock);

  status=write(sock,action,strlen(action));
  if (status<0) fail("sending action");
  term(sock);

  status=write(sock,form,strlen(form));
  if (status<0) fail("sending form");
  term(sock);

  if (incdirs!=NULL) status=write(sock,incdirs,strlen(incdirs));
  if (status<0) fail("sending includes");
  term(sock);

  digits=(int)log10((double)lensrc);
  if ((lensrcstr=(char*)malloc(digits+2))==NULL) fail("allocating 'lensrcstr'");

  sprintf(lensrcstr,"%d\n",lensrc-1);
  status=write(sock,lensrcstr,strlen(lensrcstr));
  if (status<0) fail("sending lensrcstr");

  status=write(sock,src,lensrc);
  if (status<0) fail("sending src");

  bytesout=0;
  while (read(sock,&c,1)>0)
  {
    if (c=='\0') break;
    ++bytesout;
    printf("%c",c);
  }

  if (bytesout==0) exit(1);

  close(sock);
  return 0;
}
