#include <stdio.h>
#include <assert.h>

#ifdef _WIN32

#include <Windows.h>

int wmain(int argc, char ** argv, wchar_t ** envp)
{
  wchar_t * p;
  char * s;
  int i = 0, len;
  while (envp[i]) {
    p = envp[i++];
    len = WideCharToMultiByte(CP_UTF8, 0, p, -1, NULL, 0, NULL, NULL);
    assert(len != 0);
    s = malloc(len);
    len = WideCharToMultiByte(CP_UTF8, 0, p, -1, s, len, NULL, NULL);
    assert(len != 0);
    printf("%s\n", s);
    free(s);
  }
  return 0;
}

#else

int main(int argc, char ** argv, char ** env)
{
  int i = 0;
  while (env[i])
    printf("%s\n", env[i++]);
  return 0;
}

#endif
