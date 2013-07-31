small: rtsdir/main_code.c dummy.c
	gcc rtsdir/rts/rts_support.c rtsdir/rts/jhc_rts.c rtsdir/rts/gc_jgc.c rtsdir/rts/stableptr.c rtsdir/rts/conc.c dummy.c -Irtsdir/cbits -Irtsdir rtsdir/main_code.c -o hs.out '-std=gnu99' -D_GNU_SOURCE '-falign-functions=4' -ffast-math -Wextra -Wall -Wno-unused-parameter -fno-strict-aliasing -DNDEBUG -O3 '-D_JHC_GC=_JHC_GC_JGC' '-D_JHC_CONC=_JHC_CONC_NONE' -o small

rtsdir/main_code.c: Small.hs
	ajhc --tdir rtsdir -C Small.hs

clean:
	rm -rf rtsdir small *~
