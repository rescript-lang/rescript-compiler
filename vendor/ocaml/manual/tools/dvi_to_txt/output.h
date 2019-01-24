#define SCALEX 404685
#define SCALEY 786432

int scalex;
int scaley;

#define PLAIN 0
#define ITALICS 1
#define BOLD 2
#define MONOSPACED 3

void begin_document();
void end_document();
void clear_page();
void output_page();
void out();

int output_device;
int standout_tt;

#define OUTPUT_PLAIN 0
#define OUTPUT_PRINTER 1
#define OUTPUT_RTF 2
#define OUTPUT_STYL 3
