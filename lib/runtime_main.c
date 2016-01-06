

int main()
{
    lcd_init();
    struct __val v = b_main_b();
    lcd_printf("main --> %u\n", v.data);
#ifdef FORCOMPUTER
    return 0;
#else
    while (42) {}
    return 0;
#endif
}
