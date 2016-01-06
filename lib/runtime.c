#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>

// LCD
//------------------------------------------------------------------------------
#ifndef FORCOMPUTER

#define E_PIN 1
#define LED_PIN 2
#define RS_PIN 0

#define LCD_WIDTH  20
#define LCD_HEIGHT 4

#define TAB_WIDTH 4

#ifndef F_CPU
#define F_CPU 16000000UL  	// 16 MHz ATmega328P
#endif


#include <avr/io.h>
#include <string.h>
#include <stdint.h>
#include <util/delay.h>
#include <stdarg.h>


static uint8_t lcd_y = 0;
static uint8_t lcd_x = 0;


void lcd_init();

void lcd_write(char *text, uint8_t anz);

void lcd_printf(char *msg, ...);
void lcd_print_char(uint8_t daten);
void lcd_print_string(char *text);
void lcd_print_uint(uint16_t n);
void lcd_print_int(int16_t n);
void lcd_print_tab();

void lcd_set_backlight(uint8_t val);
void lcd_set_cursor(uint8_t y, uint8_t x);
void lcd_clear();




static void lcd_send_byte(uint8_t wert, uint8_t RS)
{
	if (RS) PORTC |= (1<<RS_PIN);
	else PORTC &= ~(1<<RS_PIN);

	PORTD &= 0x0F;
	PORTD |= wert << 4;
	PORTC |= (1<<E_PIN); // Enable
	_delay_us(200);
	PORTC &= ~(1<<E_PIN); // Disable
}

static void lcd_command(uint8_t data)
{
	lcd_send_byte(data >> 4,0);
	lcd_send_byte(data & 0x0F,0);
}

static void lcd_raw_char(uint8_t data)
{
	lcd_send_byte(data >> 4,1);
	lcd_send_byte(data & 0x0F,1);
}

void lcd_print_tab()
{
	if (lcd_x % TAB_WIDTH == 0 && lcd_x != LCD_WIDTH) {
		lcd_print_char(' ');
	}
	while (lcd_x % TAB_WIDTH != 0) {
		lcd_print_char(' ');
	}
}

void lcd_print_char(uint8_t data)
{

	if (lcd_y == LCD_HEIGHT) {
		lcd_clear();
		lcd_set_cursor(0, 0);
	}
	if (lcd_x == LCD_WIDTH) {
		lcd_set_cursor(lcd_y + 1, 0);
	}

	if (data == '\n') {
		if (lcd_x == 0) {
			return;
		}
		uint8_t y = lcd_y;
		for (int i = lcd_x; i < LCD_WIDTH; i++) {
			lcd_raw_char(' ');
		}
		lcd_set_cursor(y + 1, 0);
		return;
	}

	if (data == '\t') {
		lcd_print_tab();
		return;
	}

	lcd_x++;


	lcd_raw_char(data);
}

void lcd_printf(char *msg, ...)
{
	va_list l;
	va_start(l, msg);

	while (*msg != '\0') {
		if (*msg == '%') {
			msg++;
			switch (*msg) {
			case 'u':
				lcd_print_uint(va_arg(l, uint16_t));
				msg++;
				break;
			case 'd':
			case 'i':
				lcd_print_int(va_arg(l, int16_t));
				msg++;
				break;
			case 's':
				lcd_print_string(va_arg(l, char *));
				msg++;
				break;
			case 'c':
				lcd_print_char((uint8_t) va_arg(l, int));
				msg++;
				break;
			case '%':
			default:
				lcd_print_char('%');
				msg++;
				break;
			}
		}
		lcd_print_char(*msg);
		msg++;
	}

	va_end(l);
}

void lcd_write(char *text, uint8_t anz)
{
	uint8_t i;
	for (i=0; i<anz; i++)
	{
		lcd_print_char(text[i]);
	}
}

void lcd_set_cursor(uint8_t y, uint8_t x)
{
	lcd_y = y;
	lcd_x = x;
	if (y % 2 == 1)
		lcd_command(0xC0 + ((y - 1) / 2) * LCD_WIDTH + x);
	else
		lcd_command(0x80 + (y / 2) * LCD_WIDTH + x);
}
void lcd_print_string(char * text)
{
	while (*text != '\0') {
		lcd_print_char(*text);
		text++;
	}
}

static void lcd_print_uint_(uint16_t n)
{
	if (n == 0) {
		return;
	}
	lcd_print_uint_(n / 10);
	lcd_print_char('0' + n % 10);
}

void lcd_print_uint(uint16_t n)
{
	if (n == 0) {
		lcd_print_char('0');
		return;
	}
	lcd_print_uint_(n);
}

void lcd_print_int(int16_t n)
{
	if (n == 0) {
		lcd_print_char('0');
		return;
	}
	if (n < 0) {
		lcd_print_char('-');
		n = -n;
	}
	lcd_print_uint_((uint16_t) n);
}

void lcd_set_backlight(uint8_t val)
{
	if (val == 0) {
		PORTC |= 1 << LED_PIN;
	} else {
		PORTC &= ~(1 << LED_PIN);
	}
}

void lcd_clear()
{
	lcd_command(0x01);
	_delay_ms(15);
}

void lcd_init()
{
	DDRD |= 0xF0;
	DDRC |= 0x07;
	_delay_ms(25);
	lcd_send_byte(0x03,0);		// 8 Bit Mode
	_delay_ms(80);
	lcd_send_byte(0x03,0); 	// 8 Bit Mode
	_delay_ms(15);
	lcd_send_byte(0x03,0);		// 8 Bit Mode
	_delay_ms(15);
	lcd_send_byte(0x02,0);  	// 4 Bit Mode

	lcd_command(0x28);	 	// 4 Bit Mode
	lcd_command(0x08);	 	// Display aus
	lcd_command(0x01);	 	// Display loeschen
	_delay_ms(15);
	lcd_command(0x06);	 	// Cursor inc, Anzeige nicht schieben
	lcd_command(0x0C);	 	// Cursor aus, Anzeige an
}

// -----------------------------------------------------------------------------

#else

# include <stdio.h>
# define lcd_printf printf
# define lcd_init()

#endif





typedef uint8_t (*__fun)(uint8_t);

struct __val;

struct __val {
    void *data;
    struct __node *args;
};

struct __node {
    struct __val v;
    struct __node *next;
};


#ifndef STACKSIZE

# define STACKSIZE 128

#endif


static struct __val __val_stack[STACKSIZE / sizeof(struct __val)];

static uint16_t __ip = 0;

struct __node *__create_node(struct __val a, struct __node *next)
{
    struct __node *n = malloc(sizeof(struct __node));
	if (n == NULL)
		lcd_printf("ERRNOMEM\n");
    n->v = a;
    n->next = next;
    return n;
}

uint8_t __num_args(struct __node *r)
{
    uint8_t n = 0;
    while (r != NULL) {
        n++;
        r = r->next;
    }
    return n;
}

void __free_args(struct __node *r)
{
    if (r == NULL) {
        return;
    }
    __free_args(r->next);
    free(r);
}

struct __val __val_create_num(uint16_t n)
{
    struct __val v;
    v.data = (void *) n;
    return v;
}

struct __val __val_create_str(char *s)
{
    struct __val v;
    v.data = (void *) s;
    return v;
}

void __push_val(struct __val v)
{
    __val_stack[__ip] = v;
    __ip++;
}

void __push_num(uint16_t val)
{
    __push_val(__val_create_num(val));
}

void __push_str(char *s)
{
    __push_val(__val_create_str(s));
}

struct __val __read_val(uint16_t o)
{
    return __val_stack[__ip - 1 - o];
}

void __pop()
{
    __ip--;
}

void __pop_n(uint16_t n)
{
	while (n > 0) {
		__pop();
		n--;
	}
}

uint16_t __pop_num(uint16_t o)
{
    uint16_t val = (uint16_t) __read_val(0).data;
    __pop();
    return val;
}

void __args_to_stack(struct __node *n)
{
    while (n != NULL) {
        __push_val(n->v);
        n = n->next;
    }
    return;
}

void __call_once()
{
    struct __val v = __read_val(0);
    struct __val a = __read_val(1);
    v.args = __create_node(a, v.args);
	uint8_t n = __num_args(v.args);
    __pop();
    __pop();
	__args_to_stack(v.args);
    if (((__fun) (v.data))(n) == 0) {
        // the function was not executed
		__pop_n(n);
		__push_val(v);
		return;
    }
#ifdef FREEARGS
	__free_args(v.args);
#endif
}

void __call(uint16_t n)
{
    while (n > 0) {
        __call_once();
        n--;
    }
}

void __unwind(uint16_t o)
{
    struct __val v = __read_val(0);
    while (o > 0) {
        __pop();
        o--;
    }
    __val_stack[__ip - 1] = v;
}





uint8_t __add__(uint8_t n)
{
	if (n != 2) {
		return 0;
	}
    struct __val a = __read_val(0);
    struct __val b = __read_val(1);
    __push_num((uint16_t) a.data + (uint16_t) b.data);
    __unwind(2);
    return 1;
}

struct __val builtin__add__builtin()
{
    struct __val v = {};
    v.data = __add__;
    return v;
}


uint8_t __sub__(uint8_t n)
{
	if (n != 2) {
		return 0;
	}
    struct __val a = __read_val(0);
    struct __val b = __read_val(1);
    __push_num((uint16_t) a.data - (uint16_t) b.data);
    __unwind(2);
    return 1;
}

struct __val builtin__sub__builtin()
{
    struct __val v = {};
    v.data = __sub__;
    return v;
}


uint8_t __mult__(uint8_t n)
{
	if (n != 2) {
		return 0;
	}
    struct __val a = __read_val(0);
    struct __val b = __read_val(1);
    __push_num((uint16_t) a.data * (uint16_t) b.data);
    __unwind(2);
    return 1;
}

struct __val builtin__mult__builtin()
{
    struct __val v = {};
    v.data = __mult__;
    return v;
}


uint8_t __div__(uint8_t n)
{
	if (n != 2) {
		return 0;
	}
    struct __val a = __read_val(0);
    struct __val b = __read_val(1);
    __push_num((uint16_t) a.data / (uint16_t) b.data);
    __unwind(2);
    return 1;
}

struct __val builtin__div__builtin()
{
    struct __val v = {};
    v.data = __div__;
    return v;
}


uint8_t __mod__(uint8_t n)
{
	if (n != 2) {
		return 0;
	}
    struct __val a = __read_val(0);
    struct __val b = __read_val(1);
    __push_num((uint16_t) a.data % (uint16_t) b.data);
    __unwind(2);
    return 1;
}

struct __val builtin__mod__builtin()
{
    struct __val v = {};
    v.data = __mod__;
    return v;
}


uint8_t __equal__(uint8_t n)
{
	if (n != 2) {
		return 0;
	}
    struct __val a = __read_val(0);
    struct __val b = __read_val(1);
    __push_num((uint16_t) a.data == (uint16_t) b.data);
    __unwind(2);
    return 1;
}

struct __val builtin__equal__builtin()
{
    struct __val v = {};
    v.data = __equal__;
    return v;
}


uint8_t __notequal__(uint8_t n)
{
	if (n != 2) {
		return 0;
	}
    struct __val a = __read_val(0);
    struct __val b = __read_val(1);
    __push_num((uint16_t) a.data != (uint16_t) b.data);
    __unwind(2);
    return 1;
}

struct __val builtin__notequal__builtin()
{
    struct __val v = {};
    v.data = __notequal__;
    return v;
}


uint8_t __shiftl__(uint8_t n)
{
	if (n != 2) {
		return 0;
	}
    struct __val a = __read_val(0);
    struct __val b = __read_val(1);
    __push_num((uint16_t) a.data << (uint16_t) b.data);
    __unwind(2);
    return 1;
}

struct __val builtin__shiftl__builtin()
{
    struct __val v = {};
    v.data = __shiftl__;
    return v;
}


uint8_t __shiftr__(uint8_t n)
{
	if (n != 2) {
		return 0;
	}
    struct __val a = __read_val(0);
    struct __val b = __read_val(1);
    __push_num((uint16_t) a.data >> (uint16_t) b.data);
    __unwind(2);
    return 1;
}

struct __val builtin__shiftr__builtin()
{
    struct __val v = {};
    v.data = __shiftr__;
    return v;
}


uint8_t __and__(uint8_t n)
{
	if (n != 2) {
		return 0;
	}
    struct __val a = __read_val(0);
    struct __val b = __read_val(1);
    __push_num((uint16_t) a.data & (uint16_t) b.data);
    __unwind(2);
    return 1;
}

struct __val builtin__and__builtin()
{
    struct __val v = {};
    v.data = __and__;
    return v;
}


uint8_t __or__(uint8_t n)
{
	if (n != 2) {
		return 0;
	}
    struct __val a = __read_val(0);
    struct __val b = __read_val(1);
    __push_num((uint16_t) a.data | (uint16_t) b.data);
    __unwind(2);
    return 1;
}

struct __val builtin__or__builtin()
{
    struct __val v = {};
    v.data = __or__;
    return v;
}


uint8_t __xor__(uint8_t n)
{
	if (n != 2) {
		return 0;
	}
    struct __val a = __read_val(0);
    struct __val b = __read_val(1);
    __push_num((uint16_t) a.data ^ (uint16_t) b.data);
    __unwind(2);
    return 1;
}

struct __val builtin__xor__builtin()
{
    struct __val v = {};
    v.data = __xor__;
    return v;
}


uint8_t __lower__(uint8_t n)
{
	if (n != 2) {
		return 0;
	}
    struct __val a = __read_val(0);
    struct __val b = __read_val(1);
    __push_num((uint16_t) a.data < (uint16_t) b.data);
    __unwind(2);
    return 1;
}

struct __val builtin__lower__builtin()
{
    struct __val v = {};
    v.data = __lower__;
    return v;
}


uint8_t __greater__(uint8_t n)
{
	if (n != 2) {
		return 0;
	}
    struct __val a = __read_val(0);
    struct __val b = __read_val(1);
    __push_num((uint16_t) a.data < (uint16_t) b.data);
    __unwind(2);
    return 1;
}

struct __val builtin__greater__builtin()
{
    struct __val v = {};
    v.data = __greater__;
    return v;
}
