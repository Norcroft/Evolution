
all:
	-rm */*.bak */*.stackdump *~ *.tgz *.gz
	-rm */*.o */*.a */*.bak */*~ */*.s */*.exe */*.map /*.stackdump
	-rm */testcode
	tar cvfz c-lib.tgz c-lib c-libhdr c-lib-test

# end of Makefile
