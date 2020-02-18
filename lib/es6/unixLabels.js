

import * as Unix from "./unix.js";

var Unix_error = Unix.Unix_error;

var error_message = Unix.error_message;

var handle_unix_error = Unix.handle_unix_error;

var environment = Unix.environment;

var getenv = Unix.getenv;

var unsafe_getenv = Unix.unsafe_getenv;

var putenv = Unix.putenv;

var execv = Unix.execv;

var execve = Unix.execve;

var execvp = Unix.execvp;

var execvpe = Unix.execvpe;

var fork = Unix.fork;

var wait = Unix.wait;

var waitpid = Unix.waitpid;

var system = Unix.system;

var getpid = Unix.getpid;

var getppid = Unix.getppid;

var nice = Unix.nice;

var stdin = Unix.stdin;

var stdout = Unix.stdout;

var stderr = Unix.stderr;

var openfile = Unix.openfile;

var close = Unix.close;

var read = Unix.read;

var write = Unix.write;

var single_write = Unix.single_write;

var write_substring = Unix.write_substring;

var single_write_substring = Unix.single_write_substring;

var in_channel_of_descr = Unix.in_channel_of_descr;

var out_channel_of_descr = Unix.out_channel_of_descr;

var descr_of_in_channel = Unix.descr_of_in_channel;

var descr_of_out_channel = Unix.descr_of_out_channel;

var lseek = Unix.lseek;

var truncate = Unix.truncate;

var ftruncate = Unix.ftruncate;

var stat = Unix.stat;

var lstat = Unix.lstat;

var fstat = Unix.fstat;

var isatty = Unix.isatty;

var LargeFile = Unix.LargeFile;

var map_file = Unix.map_file;

var unlink = Unix.unlink;

var rename = Unix.rename;

var link = Unix.link;

var chmod = Unix.chmod;

var fchmod = Unix.fchmod;

var chown = Unix.chown;

var fchown = Unix.fchown;

var umask = Unix.umask;

var access = Unix.access;

var dup = Unix.dup;

var dup2 = Unix.dup2;

var set_nonblock = Unix.set_nonblock;

var clear_nonblock = Unix.clear_nonblock;

var set_close_on_exec = Unix.set_close_on_exec;

var clear_close_on_exec = Unix.clear_close_on_exec;

var mkdir = Unix.mkdir;

var rmdir = Unix.rmdir;

var chdir = Unix.chdir;

var getcwd = Unix.getcwd;

var chroot = Unix.chroot;

var opendir = Unix.opendir;

var readdir = Unix.readdir;

var rewinddir = Unix.rewinddir;

var closedir = Unix.closedir;

var pipe = Unix.pipe;

var mkfifo = Unix.mkfifo;

var create_process = Unix.create_process;

var create_process_env = Unix.create_process_env;

var open_process_in = Unix.open_process_in;

var open_process_out = Unix.open_process_out;

var open_process = Unix.open_process;

var open_process_full = Unix.open_process_full;

var close_process_in = Unix.close_process_in;

var close_process_out = Unix.close_process_out;

var close_process = Unix.close_process;

var close_process_full = Unix.close_process_full;

var symlink = Unix.symlink;

var has_symlink = Unix.has_symlink;

var readlink = Unix.readlink;

var select = Unix.select;

var lockf = Unix.lockf;

var kill = Unix.kill;

var sigprocmask = Unix.sigprocmask;

var sigpending = Unix.sigpending;

var sigsuspend = Unix.sigsuspend;

var pause = Unix.pause;

var time = Unix.time;

var gettimeofday = Unix.gettimeofday;

var gmtime = Unix.gmtime;

var localtime = Unix.localtime;

var mktime = Unix.mktime;

var alarm = Unix.alarm;

var sleep = Unix.sleep;

var times = Unix.times;

var utimes = Unix.utimes;

var getitimer = Unix.getitimer;

var setitimer = Unix.setitimer;

var getuid = Unix.getuid;

var geteuid = Unix.geteuid;

var setuid = Unix.setuid;

var getgid = Unix.getgid;

var getegid = Unix.getegid;

var setgid = Unix.setgid;

var getgroups = Unix.getgroups;

var setgroups = Unix.setgroups;

var initgroups = Unix.initgroups;

var getlogin = Unix.getlogin;

var getpwnam = Unix.getpwnam;

var getgrnam = Unix.getgrnam;

var getpwuid = Unix.getpwuid;

var getgrgid = Unix.getgrgid;

var inet_addr_of_string = Unix.inet_addr_of_string;

var string_of_inet_addr = Unix.string_of_inet_addr;

var inet_addr_any = Unix.inet_addr_any;

var inet_addr_loopback = Unix.inet_addr_loopback;

var inet6_addr_any = Unix.inet6_addr_any;

var inet6_addr_loopback = Unix.inet6_addr_loopback;

var socket = Unix.socket;

var domain_of_sockaddr = Unix.domain_of_sockaddr;

var socketpair = Unix.socketpair;

var accept = Unix.accept;

var bind = Unix.bind;

var connect = Unix.connect;

var listen = Unix.listen;

var shutdown = Unix.shutdown;

var getsockname = Unix.getsockname;

var getpeername = Unix.getpeername;

var recv = Unix.recv;

var recvfrom = Unix.recvfrom;

var send = Unix.send;

var send_substring = Unix.send_substring;

var sendto = Unix.sendto;

var sendto_substring = Unix.sendto_substring;

var getsockopt = Unix.getsockopt;

var setsockopt = Unix.setsockopt;

var getsockopt_int = Unix.getsockopt_int;

var setsockopt_int = Unix.setsockopt_int;

var getsockopt_optint = Unix.getsockopt_optint;

var setsockopt_optint = Unix.setsockopt_optint;

var getsockopt_float = Unix.getsockopt_float;

var setsockopt_float = Unix.setsockopt_float;

var getsockopt_error = Unix.getsockopt_error;

var open_connection = Unix.open_connection;

var shutdown_connection = Unix.shutdown_connection;

var establish_server = Unix.establish_server;

var gethostname = Unix.gethostname;

var gethostbyname = Unix.gethostbyname;

var gethostbyaddr = Unix.gethostbyaddr;

var getprotobyname = Unix.getprotobyname;

var getprotobynumber = Unix.getprotobynumber;

var getservbyname = Unix.getservbyname;

var getservbyport = Unix.getservbyport;

var getaddrinfo = Unix.getaddrinfo;

var getnameinfo = Unix.getnameinfo;

var tcgetattr = Unix.tcgetattr;

var tcsetattr = Unix.tcsetattr;

var tcsendbreak = Unix.tcsendbreak;

var tcdrain = Unix.tcdrain;

var tcflush = Unix.tcflush;

var tcflow = Unix.tcflow;

var setsid = Unix.setsid;

export {
  Unix_error ,
  error_message ,
  handle_unix_error ,
  environment ,
  getenv ,
  unsafe_getenv ,
  putenv ,
  execv ,
  execve ,
  execvp ,
  execvpe ,
  fork ,
  wait ,
  waitpid ,
  system ,
  getpid ,
  getppid ,
  nice ,
  stdin ,
  stdout ,
  stderr ,
  openfile ,
  close ,
  read ,
  write ,
  single_write ,
  write_substring ,
  single_write_substring ,
  in_channel_of_descr ,
  out_channel_of_descr ,
  descr_of_in_channel ,
  descr_of_out_channel ,
  lseek ,
  truncate ,
  ftruncate ,
  stat ,
  lstat ,
  fstat ,
  isatty ,
  LargeFile ,
  map_file ,
  unlink ,
  rename ,
  link ,
  chmod ,
  fchmod ,
  chown ,
  fchown ,
  umask ,
  access ,
  dup ,
  dup2 ,
  set_nonblock ,
  clear_nonblock ,
  set_close_on_exec ,
  clear_close_on_exec ,
  mkdir ,
  rmdir ,
  chdir ,
  getcwd ,
  chroot ,
  opendir ,
  readdir ,
  rewinddir ,
  closedir ,
  pipe ,
  mkfifo ,
  create_process ,
  create_process_env ,
  open_process_in ,
  open_process_out ,
  open_process ,
  open_process_full ,
  close_process_in ,
  close_process_out ,
  close_process ,
  close_process_full ,
  symlink ,
  has_symlink ,
  readlink ,
  select ,
  lockf ,
  kill ,
  sigprocmask ,
  sigpending ,
  sigsuspend ,
  pause ,
  time ,
  gettimeofday ,
  gmtime ,
  localtime ,
  mktime ,
  alarm ,
  sleep ,
  times ,
  utimes ,
  getitimer ,
  setitimer ,
  getuid ,
  geteuid ,
  setuid ,
  getgid ,
  getegid ,
  setgid ,
  getgroups ,
  setgroups ,
  initgroups ,
  getlogin ,
  getpwnam ,
  getgrnam ,
  getpwuid ,
  getgrgid ,
  inet_addr_of_string ,
  string_of_inet_addr ,
  inet_addr_any ,
  inet_addr_loopback ,
  inet6_addr_any ,
  inet6_addr_loopback ,
  socket ,
  domain_of_sockaddr ,
  socketpair ,
  accept ,
  bind ,
  connect ,
  listen ,
  shutdown ,
  getsockname ,
  getpeername ,
  recv ,
  recvfrom ,
  send ,
  send_substring ,
  sendto ,
  sendto_substring ,
  getsockopt ,
  setsockopt ,
  getsockopt_int ,
  setsockopt_int ,
  getsockopt_optint ,
  setsockopt_optint ,
  getsockopt_float ,
  setsockopt_float ,
  getsockopt_error ,
  open_connection ,
  shutdown_connection ,
  establish_server ,
  gethostname ,
  gethostbyname ,
  gethostbyaddr ,
  getprotobyname ,
  getprotobynumber ,
  getservbyname ,
  getservbyport ,
  getaddrinfo ,
  getnameinfo ,
  tcgetattr ,
  tcsetattr ,
  tcsendbreak ,
  tcdrain ,
  tcflush ,
  tcflow ,
  setsid ,
  
}
/* Unix Not a pure module */
