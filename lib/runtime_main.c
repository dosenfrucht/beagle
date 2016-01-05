

int main()
{
    lcd_init();
    struct __val v = b_main_b();
    lcd_printf("main -> %d\n", v.data);
    while (42) {}
    return 0;
}
