#_______________________________________________________________________________
#
#               CMake Project Wrapper Makefile
#_______________________________________________________________________________
#

SHELL := /bin/bash
RM    := rm -rf

#_______________________________________________________________________________
#                                                              HELPER FUNCTIONS

CHDIR_SHELL := $(SHELL)
define chdir

    $(eval _D=$(firstword $(1) $(@D)))
    $(info $(MAKE): cd $(_D)) $(eval SHELL = cd $(_D); $(CHDIR_SHELL))

endef

#_______________________________________________________________________________
#                                                                IMPLICIT RULES

all: ./build/Makefile
        # if you want to see CMake compiler options add VERBOSE=1
	@ $(MAKE) -C build

./build/Makefile:
	@ mkdir -p build
	@ (cd build >/dev/null 2>&1 && cmake ..)

distclean:
	@- (cd build >/dev/null 2>&1 && cmake . >/dev/null 2>&1)
	@- $(MAKE) --silent -C build clean || true
	@- $(RM) ./build/Makefile
	@- $(RM) ./build/src
	@- $(RM) ./build/_CPack_Packages
	@- $(RM) ./build/CMake*
	@- $(RM) ./build/Testing*
	@- $(RM) ./build/cmake.*
	@- $(RM) ./build/*.cmake
	@- $(RM) ./build/*.txt
	@- $(RM) ./docs/*.html
	@- $(RM) ./docs/*.idx
	@- $(RM) ./docs/*.css
	@- $(RM) ./docs/*.png
	@- $(RM) ./docs/*.jpg
	@- $(RM) ./docs/*.gif
	@- $(RM) ./docs/*.tiff
	@- $(RM) ./docs/*.php
	@- $(RM) ./docs/search
	@- $(RM) ./docs/installdox


ifeq ($(findstring distclean,$(MAKECMDGOALS)),)

    $(MAKECMDGOALS): ./build/Makefile
	@ $(MAKE) -C build $(MAKECMDGOALS)

endif
