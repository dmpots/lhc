
SRCS:=$(wildcard *.c)
OBJS:=$(SRCS:%.c=%.o)
CFLAGS+=-m32 -I/opt/include/gc -Iinclude
LDFLAGS:=-L/opt/lib/gc
LibRts:=libHSRts.a

$(LibRts): $(OBJS)
	ar rcs $@ $(OBJS)

.PHONEY: clean
clean:
	rm -f $(OBJS) $(LibRts)
