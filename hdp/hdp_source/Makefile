CC = g++ -Wall
#CFLAGS = -g -Wall -O3 -ffast-math -DHAVE_INLINE -DGSL_RANGE_CHECK_OFF
# CFLAGS = -g -Wall
LDFLAGS = -lgsl -lm -lgslcblas

GSL_INCLUDE = "E:/software/GnuWin32/include/"
GSL_LIB = "E:/software/GnuWin32/lib/"

LSOURCE =  utils.cpp corpus.cpp state.cpp hdp.cpp main.cpp
LHEADER =  utils.h corpus.h hdp.h state.h

hdp: $(LSOURCE) $(HEADER)
	#$(CC) $(LSOURCE) -o $@ $(LDFLAGS)
	$(CC) $(LSOURCE) -I$(GSL_INCLUDE) -L$(GSL_LIB) -o $@ $(LDFLAGS)

clean:
	rm -f *.o hdp