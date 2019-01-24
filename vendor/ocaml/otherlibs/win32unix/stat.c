/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*                David Allsopp, MetaStack Solutions Ltd.                 */
/*                                                                        */
/*   Copyright 2015 MetaStack Solutions Ltd.                              */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

#include <errno.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/signals.h>
#include <caml/osdeps.h>
#include "unixsupport.h"
#include "cst2constr.h"
#define _INTEGRAL_MAX_BITS 64
#include <sys/types.h>
#include <sys/stat.h>
#include <time.h>
#include <winioctl.h>

#ifndef S_IFLNK
/*
 * The Microsoft CRT doesn't support lstat and so has no S_IFLNK
 * The implementation uses comparison, so rather than allocating another bit, in
 * a potentially future-incompatible way, just create a value with multiple bits
 * set.
 */
#define S_IFLNK (S_IFDIR | S_IFREG)
#endif
#ifndef S_IFIFO
#ifdef _S_IFIFO
#define S_IFIFO _S_IFIFO
#else
#define S_IFIFO (S_IFREG | S_IFCHR)
#endif
#endif
#ifndef S_IFSOCK
#define S_IFSOCK (S_IFDIR | S_IFCHR)
#endif
#ifndef S_IFBLK
#define S_IFBLK 0
#endif

static int file_kind_table[] = {
  S_IFREG, S_IFDIR, S_IFCHR, S_IFBLK, S_IFLNK, S_IFIFO, S_IFSOCK
};

static value stat_aux(int use_64, __int64 st_ino, struct _stat64 *buf)
{
  CAMLparam0 ();
  CAMLlocal1 (v);

  v = caml_alloc (12, 0);
  Store_field (v, 0, Val_int (buf->st_dev));
  Store_field (v, 1, Val_int (st_ino ? st_ino & Max_long : buf->st_ino));
  Store_field (v, 2, cst_to_constr (buf->st_mode & S_IFMT, file_kind_table,
                                    sizeof(file_kind_table) / sizeof(int), 0));
  Store_field (v, 3, Val_int(buf->st_mode & 07777));
  Store_field (v, 4, Val_int (buf->st_nlink));
  Store_field (v, 5, Val_int (buf->st_uid));
  Store_field (v, 6, Val_int (buf->st_gid));
  Store_field (v, 7, Val_int (buf->st_rdev));
  Store_field (v, 8,
               use_64 ? caml_copy_int64(buf->st_size) : Val_int (buf->st_size));
  Store_field (v, 9, caml_copy_double((double) buf->st_atime / 10000000.0));
  Store_field (v, 10, caml_copy_double((double) buf->st_mtime / 10000000.0));
  Store_field (v, 11, caml_copy_double((double) buf->st_ctime / 10000000.0));
  CAMLreturn (v);
}

/*
 * The long and ugly story of Microsoft CRT stat and symbolic links
 *
 * msvcrt.dll - which is now a core Windows component - is basically Visual
 * Studio .NET 2003 CRT (Version 7). It is the version usually linked against by
 * mingw64-gcc Its behaviour is as follows:
 *   a) st_mode is correctly populated
 *   b) st_atime, st_mtime and st_ctime are those for the symbolic link, not the
 *      target
 *   c) stat incorrectly returns information even if the target doesn't exist
 *
 * The next CRT of interest is Visual Studio 2008 (Version 9 - msvcr900.dll), as
 * that's included with the Windows 7 SDK. This worked until 2011 when Microsoft
 * produced security advisory KB2467174 (see https://bugs.python.org/issue6727)
 * at which point stat returns ENOENT for symbolic links.
 *
 * This persists until Visual Studio 2010, when a hotfix
 * (https://support.microsoft.com/en-gb/kb/2890375) was produced which was
 * supposed to fix this behaviour. This CRT has one problem: it returns S_REG
 * instead of S_DIR for directory symbolic links because of a subtle error in
 * its implementation (it calls fstat which quite reasonably always assumes its
 * looking at a regular file).
 *
 * The bug persists in Visual Studio 2012. Visual Studio 2015 features the
 * "great refactored" CRT (written in C++!). This CRT correctly returns st_mode
 * for directory symbolic links. Its two limitations are that it doesn't return
 * the st_size correctly for symbolic links and it doesn't populate st_nlink
 * correctly.
 *
 * However, even if fixed, mingw64 is limited to msvcrt.dll (by default, anyway)
 * and that's a lot of buggy CRTs out there.
 *
 * There is also no implementation given for lstat in any CRT.
 *
 * do_stat therefore reimplements stat - but the algorithms for populating the
 * resulting _stat64 are identical to Microsoft's (with the exception of correct
 * handling of st_nlink for symbolic links), being based upon the code for the
 * Microsoft CRT given in Microsoft Visual Studio 2013 Express
 */

static int convert_time(FILETIME* time, __time64_t* result, __time64_t def)
{
  /* Tempting though it may be, MSDN prohibits casting FILETIME directly
   * to __int64 for alignment concerns. While this doesn't affect our supported
   * platforms, it's easier to go with the flow...
   */
  ULARGE_INTEGER utime = {{time->dwLowDateTime, time->dwHighDateTime}};

  if (utime.QuadPart) {
    /* There are 11644473600000 seconds between 1 January 1601 (the NT Epoch)
     * and 1 January 1970 (the Unix Epoch). FILETIME is measured in 100ns ticks.
     */
    *result = (utime.QuadPart - INT64_LITERAL(116444736000000000U));
  }
  else {
    *result = def;
  }

  return 1;
}

/* path allocated outside the OCaml heap */
static int safe_do_stat(int do_lstat, int use_64, wchar_t* path, HANDLE fstat, __int64* st_ino, struct _stat64* res)
{
  BY_HANDLE_FILE_INFORMATION info;
  int i;
  wchar_t* ptr;
  char c;
  HANDLE h;
  unsigned short mode;
  int is_symlink = 0;

  if (!path) {
    h = fstat;
  }
  else {
    caml_enter_blocking_section();
    h = CreateFile(path,
                   FILE_READ_ATTRIBUTES,
                   FILE_SHARE_DELETE | FILE_SHARE_READ | FILE_SHARE_WRITE,
                   NULL,
                   OPEN_EXISTING,
                   FILE_FLAG_BACKUP_SEMANTICS | FILE_FLAG_OPEN_REPARSE_POINT,
                   NULL);
    caml_leave_blocking_section();
  }
  if (h == INVALID_HANDLE_VALUE) {
    errno = ENOENT;
    return 0;
  }
  else {
    caml_enter_blocking_section();
    if (!GetFileInformationByHandle(h, &info)) {
      win32_maperr(GetLastError());
      caml_leave_blocking_section();
      if (path) CloseHandle(h);
      return 0;
    }
    caml_leave_blocking_section();

    /*
     * It shouldn't be possible to call this via fstat and have a reparse point
     * open, but the test on path guarantees this.
     */
    if (info.dwFileAttributes & FILE_ATTRIBUTE_REPARSE_POINT && path) {
      /*
       * Only symbolic links should be processed specially. The call to
       * DeviceIoControl solves two problems at the same time:
       *   a) Although FindFirstFileEx gives the reparse tag in dwReserved0,
       *      GetFileInformationByHandle does not and using the Ex version (or
       *      GetFileAttributesEx) makes Windows XP support harder
       *   b) Windows returns 0 for the size of a symbolic link - reading the
       *      reparse point allows a POSIX-compatible value to be returned in
       *      st_size
       */
      char buffer[16384];
      DWORD read;
      REPARSE_DATA_BUFFER* point;

      caml_enter_blocking_section();
      if (DeviceIoControl(h, FSCTL_GET_REPARSE_POINT, NULL, 0, buffer, 16384, &read, NULL)) {
        if (((REPARSE_DATA_BUFFER*)buffer)->ReparseTag == IO_REPARSE_TAG_SYMLINK) {
          is_symlink = do_lstat;
          res->st_size = ((REPARSE_DATA_BUFFER*)buffer)->SymbolicLinkReparseBuffer.SubstituteNameLength / 2;
        }
      }
      caml_leave_blocking_section();

      if (!is_symlink) {
        CloseHandle(h);
        caml_enter_blocking_section();
        if ((h = CreateFile(path,
                            FILE_READ_ATTRIBUTES,
                            FILE_SHARE_DELETE | FILE_SHARE_READ | FILE_SHARE_WRITE,
                            NULL,
                            OPEN_EXISTING,
                            FILE_FLAG_BACKUP_SEMANTICS,
                            NULL)) == INVALID_HANDLE_VALUE) {
          errno = ENOENT;
          caml_leave_blocking_section();
          return 0;
        }
        else {
          if (!GetFileInformationByHandle(h, &info)) {
            win32_maperr(GetLastError());
            caml_leave_blocking_section();
            CloseHandle(h);
            return 0;
          }
          caml_leave_blocking_section();
        }
      }
    }

    if (path) CloseHandle(h);

    if (!is_symlink) {
      /*
       * The size returned seems to vary depending on whether it's a directory
       * (in which case it's 0) or a symbolic link (in which case it looks like
       * allocated sector size).
       * Neither is interesting, so return 0.
       */
      if (info.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) {
        res->st_size = 0;
      }
      else {
        res->st_size = ((__int64)(info.nFileSizeHigh)) << 32 |
                       ((__int64)info.nFileSizeLow);
      }
    }

    if (!use_64 && res->st_size > Max_long) {
      win32_maperr(ERROR_ARITHMETIC_OVERFLOW);
      return 0;
    }

    if (!convert_time(&info.ftLastWriteTime, &res->st_mtime, 0) ||
        !convert_time(&info.ftLastAccessTime, &res->st_atime, res->st_mtime) ||
        !convert_time(&info.ftCreationTime, &res->st_ctime, res->st_mtime)) {
      win32_maperr(GetLastError());
      return 0;
    }

    /*
     * Note MS CRT (still) puts st_nlink = 1 and gives st_ino = 0
     */
    res->st_nlink = info.nNumberOfLinks;
    res->st_dev = info.dwVolumeSerialNumber;
    *st_ino = ((__int64)(info.nFileIndexHigh)) << 32 | ((__int64)info.nFileIndexLow);
  }

  if (do_lstat && is_symlink) {
    mode = S_IFLNK | S_IEXEC | S_IWRITE;
  }
  else {
    mode = (info.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY ? _S_IFDIR | _S_IEXEC : _S_IFREG);
  }
  mode |= (info.dwFileAttributes & FILE_ATTRIBUTE_READONLY ? _S_IREAD : _S_IREAD | _S_IWRITE);
  /*
   * The simulation of the execute bit is ignored for fstat. It could be
   * emulated using GetFinalPathNameByHandle, but the pre-Vista emulation is a
   * bit too much effort for a simulated value, so it's simply ignored!
   */
  if (path && (ptr = wcsrchr(path, '.')) && (!_wcsicmp(ptr, L".exe") ||
                                             !_wcsicmp(ptr, L".cmd") ||
                                             !_wcsicmp(ptr, L".bat") ||
                                             !_wcsicmp(ptr, L".com"))) {
    mode |= _S_IEXEC;
  }
  mode |= (mode & 0700) >> 3;
  mode |= (mode & 0700) >> 6;
  res->st_mode = mode;
  res->st_uid = res->st_gid = res->st_ino = 0;
  res->st_rdev = res->st_dev;

  return 1;
}

static int do_stat(int do_lstat, int use_64, char* opath, HANDLE fstat, __int64* st_ino, struct _stat64* res)
{
  wchar_t* wpath;
  int ret;
  wpath = caml_stat_strdup_to_utf16(opath);
  ret = safe_do_stat(do_lstat, use_64, wpath, fstat, st_ino, res);
  caml_stat_free(wpath);
  return ret;
}

CAMLprim value unix_stat(value path)
{
  struct _stat64 buf;
  __int64 st_ino;

  caml_unix_check_path(path, "stat");
  if (!do_stat(0, 0, String_val(path), NULL, &st_ino, &buf)) {
    uerror("stat", path);
  }
  return stat_aux(0, st_ino, &buf);
}

CAMLprim value unix_stat_64(value path)
{
  struct _stat64 buf;
  __int64 st_ino;

  caml_unix_check_path(path, "stat");
  if (!do_stat(0, 1, String_val(path), NULL, &st_ino, &buf)) {
    uerror("stat", path);
  }
  return stat_aux(1, st_ino, &buf);
}

CAMLprim value unix_lstat(value path)
{
  struct _stat64 buf;
  __int64 st_ino;

  caml_unix_check_path(path, "lstat");
  if (!do_stat(1, 0, String_val(path), NULL, &st_ino, &buf)) {
    uerror("lstat", path);
  }
  return stat_aux(0, st_ino, &buf);
}

CAMLprim value unix_lstat_64(value path)
{
  struct _stat64 buf;
  __int64 st_ino;

  caml_unix_check_path(path, "lstat");
  if (!do_stat(1, 1, String_val(path), NULL, &st_ino, &buf)) {
    uerror("lstat", path);
  }
  return stat_aux(1, st_ino, &buf);
}

static value do_fstat(value handle, int use_64)
{
  int ret;
  struct _stat64 buf;
  __int64 st_ino;
  HANDLE h;
  DWORD ft;

  st_ino = 0;
  memset(&buf, 0, sizeof buf);
  buf.st_nlink = 1;

  h = Handle_val(handle);
  ft = GetFileType(h) & ~FILE_TYPE_REMOTE;
  switch(ft) {
  case FILE_TYPE_DISK:
    if (!safe_do_stat(0, use_64, NULL, Handle_val(handle), &st_ino, &buf)) {
      uerror("fstat", Nothing);
    }
    break;
  case FILE_TYPE_CHAR:
    buf.st_mode = S_IFCHR;
    break;
  case FILE_TYPE_PIPE:
    {
      DWORD n_avail;
      if (Descr_kind_val(handle) == KIND_SOCKET) {
        buf.st_mode = S_IFSOCK;
      }
      else {
        buf.st_mode = S_IFIFO;
      }
      if (PeekNamedPipe(h, NULL, 0, NULL, &n_avail, NULL)) {
        buf.st_size = n_avail;
      }
    }
    break;
  case FILE_TYPE_UNKNOWN:
    unix_error(EBADF, "fstat", Nothing);
  default:
    win32_maperr(GetLastError());
    uerror("fstat", Nothing);
  }
  return stat_aux(use_64, st_ino, &buf);
}

CAMLprim value unix_fstat(value handle)
{
  return do_fstat(handle, 0);
}

CAMLprim value unix_fstat_64(value handle)
{
  return do_fstat(handle, 1);
}
