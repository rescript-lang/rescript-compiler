'use strict';

var Unix = require("./unix.js");

var Unix_error = Unix.Unix_error;

var error_message = Unix.error_message;

var handle_unix_error = Unix.handle_unix_error;

var environment = Unix.environment;

var getenv = Unix.getenv;

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

exports.Unix_error = Unix_error;
exports.error_message = error_message;
exports.handle_unix_error = handle_unix_error;
exports.environment = environment;
exports.getenv = getenv;
exports.putenv = putenv;
exports.execv = execv;
exports.execve = execve;
exports.execvp = execvp;
exports.execvpe = execvpe;
exports.fork = fork;
exports.wait = wait;
exports.waitpid = waitpid;
exports.system = system;
exports.getpid = getpid;
exports.getppid = getppid;
exports.nice = nice;
exports.stdin = stdin;
exports.stdout = stdout;
exports.stderr = stderr;
exports.openfile = openfile;
exports.close = close;
exports.read = read;
exports.write = write;
exports.single_write = single_write;
exports.write_substring = write_substring;
exports.single_write_substring = single_write_substring;
exports.in_channel_of_descr = in_channel_of_descr;
exports.out_channel_of_descr = out_channel_of_descr;
exports.descr_of_in_channel = descr_of_in_channel;
exports.descr_of_out_channel = descr_of_out_channel;
exports.lseek = lseek;
exports.truncate = truncate;
exports.ftruncate = ftruncate;
exports.stat = stat;
exports.lstat = lstat;
exports.fstat = fstat;
exports.isatty = isatty;
exports.LargeFile = LargeFile;
exports.unlink = unlink;
exports.rename = rename;
exports.link = link;
exports.chmod = chmod;
exports.fchmod = fchmod;
exports.chown = chown;
exports.fchown = fchown;
exports.umask = umask;
exports.access = access;
exports.dup = dup;
exports.dup2 = dup2;
exports.set_nonblock = set_nonblock;
exports.clear_nonblock = clear_nonblock;
exports.set_close_on_exec = set_close_on_exec;
exports.clear_close_on_exec = clear_close_on_exec;
exports.mkdir = mkdir;
exports.rmdir = rmdir;
exports.chdir = chdir;
exports.getcwd = getcwd;
exports.chroot = chroot;
exports.opendir = opendir;
exports.readdir = readdir;
exports.rewinddir = rewinddir;
exports.closedir = closedir;
exports.pipe = pipe;
exports.mkfifo = mkfifo;
exports.create_process = create_process;
exports.create_process_env = create_process_env;
exports.open_process_in = open_process_in;
exports.open_process_out = open_process_out;
exports.open_process = open_process;
exports.open_process_full = open_process_full;
exports.close_process_in = close_process_in;
exports.close_process_out = close_process_out;
exports.close_process = close_process;
exports.close_process_full = close_process_full;
exports.symlink = symlink;
exports.readlink = readlink;
exports.select = select;
exports.lockf = lockf;
exports.kill = kill;
exports.sigprocmask = sigprocmask;
exports.sigpending = sigpending;
exports.sigsuspend = sigsuspend;
exports.pause = pause;
exports.time = time;
exports.gettimeofday = gettimeofday;
exports.gmtime = gmtime;
exports.localtime = localtime;
exports.mktime = mktime;
exports.alarm = alarm;
exports.sleep = sleep;
exports.times = times;
exports.utimes = utimes;
exports.getitimer = getitimer;
exports.setitimer = setitimer;
exports.getuid = getuid;
exports.geteuid = geteuid;
exports.setuid = setuid;
exports.getgid = getgid;
exports.getegid = getegid;
exports.setgid = setgid;
exports.getgroups = getgroups;
exports.setgroups = setgroups;
exports.initgroups = initgroups;
exports.getlogin = getlogin;
exports.getpwnam = getpwnam;
exports.getgrnam = getgrnam;
exports.getpwuid = getpwuid;
exports.getgrgid = getgrgid;
exports.inet_addr_of_string = inet_addr_of_string;
exports.string_of_inet_addr = string_of_inet_addr;
exports.inet_addr_any = inet_addr_any;
exports.inet_addr_loopback = inet_addr_loopback;
exports.inet6_addr_any = inet6_addr_any;
exports.inet6_addr_loopback = inet6_addr_loopback;
exports.socket = socket;
exports.domain_of_sockaddr = domain_of_sockaddr;
exports.socketpair = socketpair;
exports.accept = accept;
exports.bind = bind;
exports.connect = connect;
exports.listen = listen;
exports.shutdown = shutdown;
exports.getsockname = getsockname;
exports.getpeername = getpeername;
exports.recv = recv;
exports.recvfrom = recvfrom;
exports.send = send;
exports.send_substring = send_substring;
exports.sendto = sendto;
exports.sendto_substring = sendto_substring;
exports.getsockopt = getsockopt;
exports.setsockopt = setsockopt;
exports.getsockopt_int = getsockopt_int;
exports.setsockopt_int = setsockopt_int;
exports.getsockopt_optint = getsockopt_optint;
exports.setsockopt_optint = setsockopt_optint;
exports.getsockopt_float = getsockopt_float;
exports.setsockopt_float = setsockopt_float;
exports.getsockopt_error = getsockopt_error;
exports.open_connection = open_connection;
exports.shutdown_connection = shutdown_connection;
exports.establish_server = establish_server;
exports.gethostname = gethostname;
exports.gethostbyname = gethostbyname;
exports.gethostbyaddr = gethostbyaddr;
exports.getprotobyname = getprotobyname;
exports.getprotobynumber = getprotobynumber;
exports.getservbyname = getservbyname;
exports.getservbyport = getservbyport;
exports.getaddrinfo = getaddrinfo;
exports.getnameinfo = getnameinfo;
exports.tcgetattr = tcgetattr;
exports.tcsetattr = tcsetattr;
exports.tcsendbreak = tcsendbreak;
exports.tcdrain = tcdrain;
exports.tcflush = tcflush;
exports.tcflow = tcflow;
exports.setsid = setsid;
/* Unix Not a pure module */
