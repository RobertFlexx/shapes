FC = gfortran
FFLAGS ?= -O3 -std=f2018 -Wall -Wextra
CFLAGS ?= -O2 -std=c99 -Wall -Wextra -Wpedantic
CPPFLAGS ?=
LDFLAGS ?=
LDLIBS ?=
EXEEXT ?=
PYTHON ?= python3

.PHONY: all clean run check
all: shapes$(EXEEXT)

shapes$(EXEEXT): shapes.o terminal.o
	$(FC) $(LDFLAGS) shapes.o terminal.o $(LDLIBS) -o $@

shapes.o: shapes.f90
	$(FC) $(FFLAGS) -c shapes.f90 -o $@

terminal.o: terminal.c
	$(CC) $(CPPFLAGS) $(CFLAGS) -c terminal.c -o $@

run: shapes$(EXEEXT)
	./shapes$(EXEEXT)

check: shapes$(EXEEXT)
	./shapes$(EXEEXT) --check-meshes
	$(PYTHON) tests/test_shapes.py ./shapes$(EXEEXT)

clean:
	rm -f shapes shapes.exe shapes.o terminal.o
