OBJS=io.o interp.o output.o main.o print.o print_rtf.o print_styl.o
CFLAGS=-g

../dvi2txt: $(OBJS)
	$(CC) $(CFLAGS) -o ../dvi2txt $(OBJS)

clean:
	rm -f ../dvi2txt *.o *~ #*#
