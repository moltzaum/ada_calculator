
OBJ=obj
SRC=src
RUN ?= no
TARGET ?= calculator
#TARGET=string_reverse

# Pass these to gnatmake?
#-fstack-check -- Stack overflow
#-gnato -- Arithmetic overflow
#-gnat2012 -- Use Ada 2012

calc: 
	@gnatmake $(SRC)/$(TARGET).adb -o ../$(TARGET) --subdirs=$(OBJ) 
ifeq ($(RUN), yes)
	@echo "Running $(TARGET)"
	@./$(TARGET)
endif

help:
	@echo "Welcome to the makefile for my calculator"
	@echo 
	@echo "Availible make targets:"
	@echo "   calc  --build the calculator"
	@echo "   help  --display this"

clean:
	rm -f $(OBJ)/* $(TARGET)

sense:
	@echo "I do make sense."
	
