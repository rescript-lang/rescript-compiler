/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*                 David Allsopp, OCaml Labs, Cambridge.                  */
/*                                                                        */
/*   Copyright 2017 MetaStack Solutions Ltd.                              */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#include <windows.h>

typedef union ufiletime_int64
{
  unsigned __int64 scalar;
  FILETIME ft;
} filetime_int64;

static filetime_int64 clk;
static DWORD wall = 0;
static unsigned __int64 bias = 0LL;

BOOL WINAPI FakeConvert(const FILETIME* lpFileTime, LPFILETIME lpLocalFileTime)
{
  filetime_int64 result;
  memcpy(&result.ft, lpFileTime, sizeof(FILETIME));
  result.scalar += bias;
  memcpy(lpLocalFileTime, &result.ft, sizeof(FILETIME));
  return TRUE;
}

void WINAPI FakeClock(LPFILETIME result)
{
  DWORD now = GetTickCount();
  /* Take a risk on this: GetTickCount64 is not available in Windows XP... */
  /* GetTickCount is in ms, clk.scalar is in 100ns intervals */
  clk.scalar += ((now - wall) * 10000);
  wall = now;

  memcpy(result, &clk.ft, sizeof(FILETIME));

  return;
}

/* Assuming that nowhere transitions DST in February... */
static short mon_days[13] = {0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};

void SetBias(void)
{
  TIME_ZONE_INFORMATION tzInfo;
  filetime_int64 dst;
  SYSTEMTIME dst_start;

  switch (GetTimeZoneInformation(&tzInfo)) {
    case TIME_ZONE_ID_INVALID:
    case TIME_ZONE_ID_UNKNOWN:
      /* Default to GMT */
      tzInfo.DaylightDate.wYear = 0;
      tzInfo.DaylightDate.wMonth = 3;
      tzInfo.DaylightDate.wDay = 5;
      tzInfo.DaylightDate.wDayOfWeek = 0;
      tzInfo.DaylightDate.wHour = 1;
      tzInfo.StandardBias = 0;
      tzInfo.DaylightBias = -60;
  }

  /* If wYear is given, then DaylightDate is a date, otherwise the transition
   * is the wDay'th wDayOfWeek of wMonth (where the 5th wDayOfWeek means last
   * when there are only 4 wDayOfWeek's in wMonth)
   */
  if (!tzInfo.DaylightDate.wYear) {
    int wday;
    /* Get the clock date in order to determine wYear */
    FileTimeToSystemTime(&clk.ft, &dst_start);
    /* Back-up DST transition details */
    dst_start.wDay = tzInfo.DaylightDate.wDay;
    dst_start.wDayOfWeek = tzInfo.DaylightDate.wDayOfWeek;
    /* Set tzInfo to be first day of month on DST change */
    tzInfo.DaylightDate.wYear = dst_start.wYear;
    tzInfo.DaylightDate.wDay = 1;
    /* Normalise tzInfo.DaylightDate (need wDayOfWeek) */
    SystemTimeToFileTime(&tzInfo.DaylightDate, &dst.ft);
    FileTimeToSystemTime(&dst.ft, &tzInfo.DaylightDate);
    /* First to first weekday of DST transition */
    if ((wday = dst_start.wDayOfWeek - tzInfo.DaylightDate.wDayOfWeek) < 0)
      tzInfo.DaylightDate.wDay += wday + 7;
    else
      tzInfo.DaylightDate.wDay += wday;
    tzInfo.DaylightDate.wDayOfWeek =
      (mon_days[tzInfo.DaylightDate.wMonth] - tzInfo.DaylightDate.wDay) / 7;
    if (dst_start.wDay > tzInfo.DaylightDate.wDayOfWeek)
      dst_start.wDay = tzInfo.DaylightDate.wDayOfWeek;
    tzInfo.DaylightDate.wDay += 7 * dst_start.wDay;
  }
  SystemTimeToFileTime(&tzInfo.DaylightDate, &dst.ft);
  bias = -(clk.scalar >= dst.scalar ? tzInfo.DaylightBias
                                    : tzInfo.StandardBias) * 600000000LL;
  return;
}

void ReplaceFunction(char* fn, char* module, void* pNew)
{
  HMODULE hModule = LoadLibrary(module);
  void* pCode;
  DWORD dwOldProtect = 0;
#ifdef _M_X64
  SIZE_T jmpSize = 13;
  BYTE jump[13];
#else
  SIZE_T jmpSize = 5;
  BYTE jump[5];
#endif
  SIZE_T bytesWritten;

  /* Patching is permitted to fail (missing API, etc.) */
  if (!hModule) return;
  pCode = GetProcAddress(hModule, fn);
  if (!pCode) return;

  /* Overwrite the code with a jump to our function */
  if (VirtualProtect(pCode, jmpSize, PAGE_EXECUTE_READWRITE, &dwOldProtect)) {
#ifdef _M_X64
    jump[0] = 0x49;             /* REX.WB prefix */
    jump[1] = 0xBB;             /* MOV r11, ... */
    memcpy(jump + 2, &pNew, 8); /* imm64 */
    jump[10] = 0x41;            /* REX.B prefix */
    jump[11] = 0xFF;            /* JMP */
    jump[12] = 0xE3;            /* r11 */
#else
    /* JMP rel32 to FakeClock */
    DWORD dwRelativeAddr = (DWORD)pNew - ((DWORD)pCode + 5);
    jump[0] = 0xE9;
    memcpy(jump + 1, &dwRelativeAddr, 4);
#endif

    if (WriteProcessMemory(GetCurrentProcess(), pCode, jump, jmpSize, NULL)) {
      VirtualProtect(pCode, jmpSize, dwOldProtect, &dwOldProtect);
    }
  }

  return;
}

#define CAML_NAME_SPACE
#include <caml/mlvalues.h>
#include <caml/memory.h>

static int patched = 0;

CAMLprim value set_fake_clock(value time)
{
  CAMLparam1(time);

  clk.scalar = Int64_val(time);
  wall = GetTickCount();
  SetBias();

  if (!patched) {
    patched = 1;
    /* Patch Windows 8 and later (UCRT) */
    ReplaceFunction("GetSystemTimePreciseAsFileTime",
                    "api-ms-win-core-sysinfo-l1-2-1.dll", &FakeClock);
    ReplaceFunction("GetSystemTimeAsFileTime",
                    "api-ms-win-core-sysinfo-l1-2-1.dll", &FakeClock);
    /* Patch Windows 7 API Set */
    ReplaceFunction("GetSystemTimeAsFileTime",
                    "api-ms-win-core-sysinfo-l1-1-0.dll", &FakeClock);
    /* Patch Windows 7 and previous (standard CRT) */
    ReplaceFunction("GetSystemTimeAsFileTime",
                    "kernel32.dll", &FakeClock);
    ReplaceFunction("FileTimeToLocalFileTime", "kernel32.dll", &FakeConvert);
  }

  CAMLreturn(Val_unit);
}
