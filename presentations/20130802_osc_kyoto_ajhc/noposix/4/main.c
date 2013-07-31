#include "jhc_rts_header.h"

int
main(int argc, char *argv[])
{
        hs_init(&argc,&argv);
	_amain();
        hs_exit();
        return 0;
}
