help:
	@echo 'Help details:'
	@echo 'hex: compile hex file'
	@echo 'flash: install hex file'
	@echo 'program: compile hex and install'


hex:
	avr-gcc -std=c99 -Os -DF_CPU=16000000 -mmcu=atmega328p -c test.c
	avr-gcc -DF_CPU=16000000 -mmcu=atmega328p -o test.elf test.o

	avr-objcopy -O ihex test.elf test.hex
	rm test.o
	rm test.elf


flash:
	avrdude -b 57600 -c arduino -p atmega328p -P /dev/ttyUSB0 -U flash:w:test.hex