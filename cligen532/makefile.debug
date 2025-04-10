# Makefile for building cligen532 using gfortran with debug and checks

FC_GNU       = gfortran
FFLAGS_GNU   = -c -O0 -g -fcheck=all -Wall -fbacktrace -ffpe-trap=invalid,zero,overflow

LINK_GNU     = gfortran
LDFLAGS      = -g -fbacktrace

PROGRAM      = cligen532
FFLAGS       = $(FFLAGS_GNU)
FC           = $(FC_GNU)
LINKER       = $(LINK_GNU)

DEST         = .
.SUFFIXES: .for .o
MAKEFILE     = Makefile
OBJS         = cligen.o
SRCS         = cligen.f

.for.o:
	$(FC) $(FFLAGS) $<

$(PROGRAM): $(OBJS) $(LIBS)
	$(LINKER) $(LDFLAGS) $(OBJS) -o $(PROGRAM)

all: $(PROGRAM)

clean:
	rm -f $(OBJS)
	rm -f $(PROGRAM)