#include <stdio.h>
#include <windows.h>

int main(int argc, char** argv)
{
  DWORD pid;
  HANDLE hProcess;

  if (argc != 2) {
    printf("Usage: %s pid\n", argv[0]);
    return 1;
  }

  pid = atoi(argv[1]);
  hProcess = OpenProcess(SYNCHRONIZE, FALSE, pid);

  if (!hProcess) {
    printf("Process %lu not found!\n", pid);
    return 1;
  }

  FreeConsole();

  if (!AttachConsole(pid)) {
    printf("Failed to attach to console of Process %lu\n", pid);
    CloseHandle(hProcess);
    return 1;
  }

  SetConsoleCtrlHandler(NULL, TRUE);
  GenerateConsoleCtrlEvent(0, 0);
  WaitForSingleObject(hProcess, INFINITE);
  CloseHandle(hProcess);
  FreeConsole();

  return 0;
}
