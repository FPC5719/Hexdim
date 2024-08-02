VERILATOR = verilator
VERILATOR_CFLAGS += -MMD --build -cc --exe \
	-O3 --x-assign fast --x-initial fast --noassert -Wno-WIDTHTRUNC

# NVBoard Rules

NVTOP = nvTop
NVBIN = ./obj_dir/V$(NVTOP)
NVVSRCS = ../verilog/Hexdim.NVBoard.nvTop/nvTop.v
NVCSRCS = ./nvboard.cpp

INC_PATH ?=

include $(NVBOARD_HOME)/scripts/nvboard.mk

INCFLAGS = $(addprefix -I, $(INC_PATH))
CXXFLAGS += $(INCFLAGS) -DTOP_NAME="\"V$(NVTOP)\""

$(NVBIN): $(NVVSRCS) $(NVCSRCS) $(NVBOARD_ARCHIVE)
	$(VERILATOR) $(VERILATOR_CFLAGS) \
		--top-module $(NVTOP) $^ \
		$(addprefix -CFLAGS , $(CXXFLAGS)) $(addprefix -LDFLAGS , $(LDFLAGS))

nvboard: $(NVBIN)
	@$^

# Verilator Rules

TESTTOP = testTop
TESTBIN = ./obj_dir/V$(TESTTOP)
TESTVSRCS = ../verilog/Hexdim.Test.testTop/testTop.v
TESTCSRCS = ./test.cpp

$(TESTBIN): $(TESTVSRCS) $(TESTCSRCS)
	$(VERILATOR) $(VERILATOR_CFLAGS) \
		--top-module $(TESTTOP) $^

test: $(TESTBIN)
	@$^

.PHONY: nvboard test
