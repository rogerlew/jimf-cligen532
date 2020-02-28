#
#
# makefile for solaris 8 wepp executable.
#

FC_IF     = ifort
FC_G95        = g95
FC_GNU        = gfortran
FC_IF     = ifort
FC_G95        = g95
FC_GNU        = gfortran
FC_2      = f2c
FFLAGS_IFORT    = -c -autodouble -O -mia32 -align dcommons
#FFLAGS_G95    = -c -Wall -r8 -malign-double -O3 -march=pentium4
FFLAGS_G95     = -c -Wall -r8 -malign-double -O2 -freal=zero -finteger=0
FFLAGS_GNU     = -c -fdefault-real-8 -Wall -malign-double -O2 -fno-align-commons
FFLAGS_2 = -c -r
LINK_IF    = ifort
LINK_G95    = g++
LINK_GNU     = gfortran
LINK_2    = g++
PROGRAM   = cligen532 

FFLAGS = $(FFLAGS_IFORT)
FC = $(FC_IF)
LINKER = $(LINK_IF)
#FFLAGS = $(FFLAGS_G95)
#FC = $(FC_G95)
#LINKER = $(LINK_G95)
#FFLAGS = $(FFLAGS_GNU)
#FC = $(FC_GNU)
#LINKER = $(LINK_GNU)

#LDFLAGS   = -i -static /usr/lib/gcc-lib/i686-pc-linux-gnu/4.1.1/libgcc.a /usr/lib/gcc-lib/i686-pc-linux-gnu/4.1.1/libgcc_eh.a /usr/lib/gcc-lib/i686-pc-linux-gnu/4.1.1/libf95.a
LDFLAGS = -static
#LDFLAGS = -g

DEST      = .
.SUFFIXES: .for .o
MAKEFILE  = Makefile
OBJS            = cligen.o
SRCS            = cligen.f 

.for.o:
	$(FC) $(FFLAGS) $<

$(PROGRAM):     $(OBJS) $(LIBS)
	$(LINKER) $(LDFLAGS) $(OBJS) -o $(PROGRAM)

all: $(PROGRAM)
 
clean:
	rm -f $(OBJS)
	rm -f cligen532

