SMLSHARP = smlsharp
SMLFLAGS = -O2
LIBS =
all: converter
converter: converter.smi converter.o
	$(SMLSHARP) $(LDFLAGS) -o converter converter.smi $(LIBS)
converter.o: converter.sml converter.smi
	$(SMLSHARP) $(SMLFLAGS) -o converter.o -c converter.sml
