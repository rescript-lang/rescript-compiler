#ifdef __STDC__
typedef signed char schar;
#else
typedef char schar;
#endif

#define get8u(input) getc(input)
#define get8s(input) (schar) getc(input)

int get16u(), get16s(), get24u(), get24s(), get32u(), get32s();
