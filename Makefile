#
# Main Makefile, intended for use on Linux/X11 and compatible platforms
# using GNU Make.
#
# It should guess the paths to the game dependencies on its own, except for
# Boost which is assumed to be installed to the default locations. If you have
# installed Boost to a non-standard location, you will need to override CXXFLAGS
# and LDFLAGS with any applicable -I and -L arguments.
#
# The main options are:
#
#   CCACHE           The ccache binary that should be used when USE_CCACHE is
#                     enabled (see below). Defaults to 'ccache'.
#   CXX              C++ compiler comand line.
#   CXXFLAGS         Additional C++ compiler options.
#   OPTIMIZE         If set to 'yes' (default), builds with compiler
#                     optimizations enabled (-O2). You may alternatively use
#                     CXXFLAGS to set your own optimization options.
#   LDFLAGS          Additional linker options.
#   USE_CCACHE       If set to 'yes' (default), builds using the CCACHE binary
#                     to run the compiler. If ccache is not installed (i.e.
#                     found in PATH), this option has no effect.
#   USE_DB_CLIENT    If set to 'yes' and couchbase is present will build.
#

OPTIMIZE?=yes
USE_LUA?=yes
USE_BOX2D?=yes
CCACHE?=ccache
USE_CCACHE?=$(shell which $(CCACHE) 2>&1 > /dev/null && echo yes)
ifneq ($(USE_CCACHE),yes)
CCACHE=
endif

SANITIZE_ADDRESS?=
ifneq ($(SANITIZE_ADDRESS), yes)
SANITIZE_ADDRESS=
endif

ifeq ($(OPTIMIZE),yes)
BASE_CXXFLAGS += -O2
endif

ifneq ($(USE_LUA), yes)
USE_LUA=
endif

BASE_CXXFLAGS += -Wall -Werror

ifneq (,$(findstring clang, `$(CXX)`))
BASE_CXXFLAGS += -Qunused-arguments -Wno-unknown-warning-option -Wno-deprecated-register
ifeq ($(USE_LUA), yes)
BASE_CXXFLAGS += -Wno-pointer-bool-conversion -Wno-parentheses-equality
endif
else ifneq (, $(findstring g++, `$(CXX)`))
GCC_GTEQ_490 := $(shell expr `$(CXX) -dumpversion | sed -e 's/\.\([0-9][0-9]\)/\1/g' -e 's/\.\([0-9]\)/0\1/g' -e 's/^[0-9]\{3,4\}$$/&00/'` \>= 40900)
BASE_CXXFLAGS += -Wno-literal-suffix -Wno-sign-compare
ifeq "$(GCC_GTEQ_490)" "1"
BASE_CXXFLAGS += -fdiagnostics-color=auto -fsanitize=undefined
endif
endif

SDL2_CONFIG?=sdl2-config
USE_SDL2?=$(shell which $(SDL2_CONFIG) 2>&1 > /dev/null && echo yes)

ifneq ($(USE_SDL2),yes)
$(error SDL2 not found, SDL-1.2 is no longer supported)
endif

# Initial compiler options, used before CXXFLAGS and CPPFLAGS. -rdynamic -Wno-literal-suffix
BASE_CXXFLAGS += -std=c++0x -g -fno-inline-functions \
	-fthreadsafe-statics \
	-Wno-narrowing -Wno-reorder -Wno-unused \
	-Wno-unknown-pragmas -Wno-overloaded-virtual

LDFLAGS?=-rdynamic

# Check for sanitize-address option
ifeq ($(SANITIZE_ADDRESS), yes)
BASE_CXXFLAGS += -g3 -fsanitize=address
LDFLAGS += -fsanitize=address
endif

# Compiler include options, used after CXXFLAGS and CPPFLAGS.
INC := -isystem external/include $(shell pkg-config --cflags x11 sdl2 glew SDL2_image SDL2_ttf libpng zlib cairo freetype2)

ifdef STEAM_RUNTIME_ROOT
	INC += -isystem $(STEAM_RUNTIME_ROOT)/include
endif

# Linker library options.
LIBS := $(shell pkg-config --libs x11 gl ) \
	$(shell pkg-config --libs sdl2 glew SDL2_image libpng zlib cairo freetype2) \
	-lSDL2_ttf -lSDL2_mixer -lrt

MODULES   := kre

SRC_DIR   := $(addprefix src/,$(MODULES)) src
BUILD_DIR := $(addprefix build/,$(MODULES)) build

SRC       := $(foreach sdir,$(SRC_DIR),$(wildcard $(sdir)/*.cpp))
OBJ       := $(patsubst src/%.cpp,./build/%.o,$(SRC))
DEPS      := $(patsubst src/%.cpp,./build/%.o.d,$(SRC))
INCLUDES  := $(addprefix -I,$(SRC_DIR))

vpath %.cpp $(SRC_DIR)

define cc-command
$1/%.o: %.cpp
	@echo "Building:" $$<
	@$(CCACHE) $(CXX) $(BASE_CXXFLAGS) $(CXXFLAGS) $(CPPFLAGS) $(INC) $(INCLUDES) -c -o $$@ $$<
	@$(CXX) $(BASE_CXXFLAGS) $(CXXFLAGS) $(CPPFLAGS) $(INC) $(INCLUDES) -MM $$< > $$@.d
	@mv -f $$@.d $$@.d.tmp
	@sed -e 's|.*:|$$@:|' < $$@.d.tmp > $$@.d
	@sed -e 's/.*://' -e 's/\\$$$$//' < $$@.d.tmp | fmt -1 | \
		sed -e 's/^ *//' -e 's/$$$$/:/' >> $$@.d
	@rm -f $$@.d.tmp
endef

.PHONY: all checkdirs clean

all: checkdirs castles

castles: $(OBJ)
	@echo "Linking : castles"
	@$(CXX) \
		$(BASE_CXXFLAGS) $(LDFLAGS) $(CXXFLAGS) $(CPPFLAGS) \
		$(OBJ) -o castles \
		$(LIBS) -lboost_regex -lboost_system -lboost_filesystem -lpthread -fthreadsafe-statics

checkdirs: $(BUILD_DIR)

$(BUILD_DIR):
	@mkdir -p $@

clean:
	rm -f $(foreach bdir,$(BUILD_DIR),$(bdir)/*.o) $(foreach bdir,$(BUILD_DIR),$(bdir)/*.o.d) castles

$(foreach bdir,$(BUILD_DIR),$(eval $(call cc-command,$(bdir))))

# pull in dependency info for *existing* .o files
-include $(OBJ:.o=.o.d)

