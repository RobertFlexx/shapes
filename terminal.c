/* Keep platform ABIs and terminal state out of the Fortran renderer. */
#define _POSIX_C_SOURCE 200809L
#define _DARWIN_C_SOURCE 1
#define _DEFAULT_SOURCE 1
#define __EXTENSIONS__ 1
#include <errno.h>
#include <limits.h>
#include <locale.h>
#include <stdlib.h>
#include <string.h>

#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#ifndef _WIN32_WINNT
#define _WIN32_WINNT 0x0600
#endif
#include <windows.h>
#include <stdio.h>
#include <wchar.h>
#ifndef ENABLE_VIRTUAL_TERMINAL_PROCESSING
#define ENABLE_VIRTUAL_TERMINAL_PROCESSING 0x0004
#endif
static HANDLE input_handle, output_handle;
static DWORD saved_input_mode, saved_output_mode;
static UINT saved_code_page;
static int console_input, console_output;
static volatile LONG interrupted, resized;
static DWORD last_buttons;

static int is_pty_pipe(HANDLE handle)
{
    union {
        FILE_NAME_INFO info;
        unsigned char storage[sizeof(FILE_NAME_INFO) + 2048];
    } name;
    size_t length;
    WCHAR *path = name.info.FileName;
    if (GetFileType(handle) != FILE_TYPE_PIPE ||
        !GetFileInformationByHandleEx(handle, FileNameInfo, &name, sizeof(name))) return 0;
    length = name.info.FileNameLength / sizeof(WCHAR);
    if (length >= 1024) return 0;
    path[length] = L'\0';
    return (wcsstr(path, L"msys-") || wcsstr(path, L"cygwin-")) && wcsstr(path, L"-pty");
}
#else
#include <poll.h>
#include <signal.h>
#include <sys/ioctl.h>
#ifdef __sun
#include <sys/termios.h>
#endif
#include <termios.h>
#include <time.h>
#include <unistd.h>
static struct termios saved_termios, raw_termios;
static volatile sig_atomic_t interrupted, resized, suspended;
static const int watched_signals[] = {SIGINT, SIGTERM, SIGHUP, SIGQUIT, SIGWINCH, SIGTSTP, SIGCONT};
static struct sigaction saved_handlers[sizeof(watched_signals) / sizeof(watched_signals[0])];
#endif

static int active, alternate_screen, mouse_enabled, extended_terminal;

static int contains(const char *value, const char *part)
{
    return value && strstr(value, part) != NULL;
}

static int env_dimension(const char *name, int fallback)
{
    const char *value = getenv(name);
    char *end;
    long number;
    if (!value || !*value) return fallback;
    errno = 0;
    number = strtol(value, &end, 10);
    return !errno && !*end && number > 0 && number <= INT_MAX ? (int)number : fallback;
}

static void emit(const char *value)
{
    size_t remaining = strlen(value);
    while (remaining) {
#ifdef _WIN32
        DWORD count = 0;
        if (!WriteFile(output_handle, value, (DWORD)remaining, &count, NULL) || !count) break;
#else
        ssize_t count = write(STDOUT_FILENO, value, remaining);
        if (count < 0 && errno == EINTR) continue;
        if (count <= 0) break;
#endif
        value += count;
        remaining -= (size_t)count;
    }
}

static void enter_screen(void)
{
    if (alternate_screen) emit("\033[?1049h");
    emit("\033[0m\033[?25l\033[2J\033[H");
    if (mouse_enabled) emit("\033[?1002h\033[?1006h");
}

static void leave_screen(void)
{
    if (mouse_enabled) emit("\033[?1006l\033[?1002l");
    emit("\033[0m\033[?25h");
    if (alternate_screen) emit("\033[?1049l");
    else emit("\033[H\033[2J");
}

#ifdef _WIN32
static BOOL WINAPI console_signal(DWORD event)
{
    if (event == CTRL_C_EVENT || event == CTRL_BREAK_EVENT) {
        InterlockedExchange(&interrupted, 1);
        return TRUE;
    }
    return FALSE;
}
#else
static void terminal_signal(int number)
{
    if (number == SIGWINCH || number == SIGCONT) resized = 1;
    else if (number == SIGTSTP) suspended = 1;
    else interrupted = 1;
}
#endif

void sl_terminal_close(void)
{
    if (!active) return;
    leave_screen();
#ifdef _WIN32
    if (console_input) SetConsoleMode(input_handle, saved_input_mode);
    if (console_output) {
        SetConsoleMode(output_handle, saved_output_mode);
        SetConsoleOutputCP(saved_code_page);
    }
    SetConsoleCtrlHandler(console_signal, FALSE);
#else
    size_t i;
    tcsetattr(STDIN_FILENO, TCSANOW, &saved_termios);
    for (i = 0; i < sizeof(watched_signals) / sizeof(watched_signals[0]); ++i)
        sigaction(watched_signals[i], &saved_handlers[i], NULL);
#endif
    active = 0;
}

/* Requests: -1 means auto; colors are 0, 16, 256, or 16777216. plain=-1 forces basic ANSI. */
int sl_terminal_open(int *color, int *unicode, int *mouse, int plain, int alternate)
{
    const char *term = getenv("TERM");
    const char *colorterm = getenv("COLORTERM");
    const char *locale = setlocale(LC_CTYPE, "");
    int ansi, native_vt = 0, force_ansi = plain < 0;
    if (force_ansi) plain = 0;
    if (!force_ansi && term && !strcmp(term, "dumb")) plain = 1;
    extended_terminal = contains(term, "xterm") || contains(term, "screen") || contains(term, "tmux") ||
        contains(term, "rxvt") || contains(term, "kitty") || contains(term, "wezterm") ||
        contains(term, "alacritty") || contains(term, "foot") || contains(term, "st-") ||
        contains(term, "nsterm") || contains(term, "contour") || (term && !strcmp(term, "st"));
    ansi = extended_terminal || contains(term, "ansi") || contains(term, "vt100") ||
        contains(term, "vt102") || contains(term, "vt220") || contains(term, "linux") ||
        contains(term, "cygwin") || contains(term, "sun") || contains(term, "cons25") ||
        contains(term, "wsvt25") || contains(term, "pcvt") || contains(term, "dtterm") || force_ansi;
#ifdef _WIN32
    input_handle = GetStdHandle(STD_INPUT_HANDLE);
    output_handle = GetStdHandle(STD_OUTPUT_HANDLE);
    console_input = GetConsoleMode(input_handle, &saved_input_mode) != 0;
    console_output = GetConsoleMode(output_handle, &saved_output_mode) != 0;
    if (!plain && console_input && console_output) {
        native_vt = SetConsoleMode(output_handle,
            saved_output_mode | ENABLE_PROCESSED_OUTPUT | ENABLE_VIRTUAL_TERMINAL_PROCESSING) != 0;
        if (native_vt) ansi = extended_terminal = 1;
    }
    if (!native_vt && !(ansi && is_pty_pipe(input_handle) && is_pty_pipe(output_handle))) plain = 1;
#else
    if (!isatty(STDIN_FILENO) || !isatty(STDOUT_FILENO)) plain = 1;
#endif
    if (!native_vt && (!ansi || (!force_ansi && term && !strcmp(term, "dumb")))) plain = 1;
    if (*color < 0) {
        if (getenv("NO_COLOR")) *color = 0;
        else if (native_vt || contains(colorterm, "truecolor") || contains(colorterm, "24bit") ||
                 contains(term, "direct") || contains(term, "kitty")) *color = 16777216;
        else if (contains(term, "256color") || contains(getenv("TERM_PROGRAM"), "Apple_Terminal")) *color = 256;
        else if (contains(term, "vt100") || contains(term, "vt102") || contains(term, "vt220") ||
                 contains(term, "sun") || !ansi) *color = 0;
        else *color = 16;
    }
    if (*unicode < 0) *unicode = native_vt || contains(locale, "UTF-8") || contains(locale, "utf8") ||
        contains(locale, "UTF8") || contains(locale, "utf-8");
    if (!*color) *unicode = 0;
    if (*mouse < 0) *mouse = extended_terminal;
    mouse_enabled = *mouse && extended_terminal;
    alternate_screen = alternate && extended_terminal;
    if (plain) {
        *color = *unicode = *mouse = 0;
        return 0;
    }
#ifdef _WIN32
    if (console_input) {
        DWORD mode = (saved_input_mode | ENABLE_WINDOW_INPUT | ENABLE_EXTENDED_FLAGS) &
            ~(ENABLE_LINE_INPUT | ENABLE_ECHO_INPUT | ENABLE_QUICK_EDIT_MODE | ENABLE_VIRTUAL_TERMINAL_INPUT);
        if (mouse_enabled) mode |= ENABLE_MOUSE_INPUT;
        else mode &= ~ENABLE_MOUSE_INPUT;
        if (!SetConsoleMode(input_handle, mode)) {
            SetConsoleMode(output_handle, saved_output_mode);
            return 0;
        }
    }
    if (console_output) {
        saved_code_page = GetConsoleOutputCP();
        SetConsoleOutputCP(CP_UTF8);
    }
    SetConsoleCtrlHandler(console_signal, TRUE);
#else
    if (tcgetattr(STDIN_FILENO, &saved_termios) != 0) return 0;
    raw_termios = saved_termios;
    raw_termios.c_lflag &= (tcflag_t)~(ECHO | ICANON | IEXTEN);
    raw_termios.c_iflag &= (tcflag_t)~(IXON | ICRNL | INLCR);
    raw_termios.c_cc[VMIN] = 0;
    raw_termios.c_cc[VTIME] = 0;
    if (tcsetattr(STDIN_FILENO, TCSANOW, &raw_termios) != 0) return 0;
    {
        size_t i;
        struct sigaction action;
        memset(&action, 0, sizeof(action));
        action.sa_handler = terminal_signal;
        sigemptyset(&action.sa_mask);
        for (i = 0; i < sizeof(watched_signals) / sizeof(watched_signals[0]); ++i)
            sigaction(watched_signals[i], &action, &saved_handlers[i]);
    }
#endif
    *mouse = mouse_enabled;
    active = 1;
    atexit(sl_terminal_close);
    enter_screen();
    return 1;
}

void sl_terminal_size(int *columns, int *rows, double *aspect)
{
    *columns = env_dimension("COLUMNS", 80);
    *rows = env_dimension("LINES", 24);
    *aspect = 0.5;
#ifdef _WIN32
    CONSOLE_SCREEN_BUFFER_INFO info;
    if (GetConsoleScreenBufferInfo(GetStdHandle(STD_OUTPUT_HANDLE), &info)) {
        *columns = info.srWindow.Right - info.srWindow.Left + 1;
        *rows = info.srWindow.Bottom - info.srWindow.Top + 1;
    }
#else
    struct winsize size;
    memset(&size, 0, sizeof(size));
    if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &size) == 0 || ioctl(STDIN_FILENO, TIOCGWINSZ, &size) == 0) {
        if (size.ws_col) *columns = size.ws_col;
        if (size.ws_row) *rows = size.ws_row;
        if (size.ws_xpixel && size.ws_ypixel && size.ws_col && size.ws_row) {
            double measured = (double)size.ws_xpixel * size.ws_row / ((double)size.ws_ypixel * size.ws_col);
            if (measured >= 0.2 && measured <= 1.2) *aspect = measured;
        }
    }
#endif
}

int sl_terminal_events(void)
{
#ifdef _WIN32
    int events = InterlockedCompareExchange(&interrupted, 0, 0) ? 1 : 0;
#else
    int events = interrupted ? 1 : 0;
    if (suspended && !interrupted) {
        suspended = 0;
        leave_screen();
        tcsetattr(STDIN_FILENO, TCSANOW, &saved_termios);
        raise(SIGSTOP);
        if (!interrupted) {
            tcsetattr(STDIN_FILENO, TCSANOW, &raw_termios);
            enter_screen();
            resized = 1;
        }
        events = interrupted ? 1 : 0;
    }
#endif
#ifdef _WIN32
    if (InterlockedExchange(&resized, 0)) events |= 2;
#else
    if (resized) { events |= 2; resized = 0; }
#endif
    return events;
}

int sl_terminal_read(char *buffer, int capacity)
{
    if (!active || capacity <= 0) return 0;
#ifdef _WIN32
    DWORD count;
    int used = 0;
    if (!console_input) {
        if (!PeekNamedPipe(input_handle, NULL, 0, NULL, &count, NULL)) return -1;
        if (!count) return 0;
        if (count > (DWORD)capacity) count = (DWORD)capacity;
        return ReadFile(input_handle, buffer, count, &count, NULL) ? (int)count : -1;
    }
    if (!GetNumberOfConsoleInputEvents(input_handle, &count)) return -1;
    while (count-- && used + 64 <= capacity) {
        INPUT_RECORD record;
        DWORD read_count;
        if (!ReadConsoleInputW(input_handle, &record, 1, &read_count)) return -1;
        if (record.EventType == WINDOW_BUFFER_SIZE_EVENT) resized = 1;
        if (record.EventType == KEY_EVENT && record.Event.KeyEvent.bKeyDown) {
            KEY_EVENT_RECORD key = record.Event.KeyEvent;
            const char *sequence = NULL;
            unsigned int repeat;
            if (key.wVirtualKeyCode == VK_UP) sequence = "\033[A";
            if (key.wVirtualKeyCode == VK_DOWN) sequence = "\033[B";
            if (key.wVirtualKeyCode == VK_RIGHT) sequence = "\033[C";
            if (key.wVirtualKeyCode == VK_LEFT) sequence = "\033[D";
            for (repeat = 0; repeat < key.wRepeatCount && used + 4 < capacity; ++repeat) {
                if (sequence) { memcpy(buffer + used, sequence, 3); used += 3; }
                else if (key.uChar.UnicodeChar && key.uChar.UnicodeChar < 128)
                    buffer[used++] = (char)key.uChar.UnicodeChar;
            }
        }
        if (record.EventType == MOUSE_EVENT && mouse_enabled) {
            MOUSE_EVENT_RECORD mouse = record.Event.MouseEvent;
            CONSOLE_SCREEN_BUFFER_INFO info;
            int code = 0, release = 0, x, y;
            if (!GetConsoleScreenBufferInfo(output_handle, &info)) continue;
            x = mouse.dwMousePosition.X - info.srWindow.Left + 1;
            y = mouse.dwMousePosition.Y - info.srWindow.Top + 1;
            if (mouse.dwEventFlags == MOUSE_WHEELED) code = (SHORT)HIWORD(mouse.dwButtonState) > 0 ? 64 : 65;
            else if (mouse.dwButtonState & FROM_LEFT_1ST_BUTTON_PRESSED) code = 0;
            else if (last_buttons & FROM_LEFT_1ST_BUTTON_PRESSED) release = 1;
            else continue;
            if (mouse.dwEventFlags == MOUSE_MOVED) code |= 32;
            last_buttons = mouse.dwButtonState;
            used += snprintf(buffer + used, (size_t)(capacity - used), "\033[<%d;%d;%d%c", code, x, y, release ? 'm' : 'M');
        }
    }
    return used;
#else
    struct pollfd descriptor;
    ssize_t count;
    descriptor.fd = STDIN_FILENO;
    descriptor.events = POLLIN;
    descriptor.revents = 0;
    if (poll(&descriptor, 1, 0) <= 0) return 0;
    if (descriptor.revents & (POLLHUP | POLLERR | POLLNVAL)) return -1;
    count = read(STDIN_FILENO, buffer, (size_t)capacity);
    if (count < 0 && (errno == EINTR || errno == EAGAIN)) return 0;
    return count > 0 ? (int)count : -1;
#endif
}

void sl_terminal_sleep(int milliseconds)
{
    if (milliseconds <= 0) return;
#ifdef _WIN32
    Sleep((DWORD)milliseconds);
#else
    struct timespec duration;
    duration.tv_sec = milliseconds / 1000;
    duration.tv_nsec = (long)(milliseconds % 1000) * 1000000L;
    nanosleep(&duration, NULL);
#endif
}
