# Shape Lab

An interactive 3D shape viewer for the terminal, written in modern Fortran with a small C terminal adapter. Twenty meshes have depth buffering, smooth or flat normals, metallic lighting, soft shadows, five environments, and virtual trackball rotation.

The scene fits the available width **and** height, preserves proportions, adjusts mesh detail to the viewport, and responds to resizing. The interface shrinks with the window and gets out of the way on tiny screens. Only changed terminal cells are transmitted, after color quantization.

## Build and run

You need GNU Fortran with Fortran 2018 support, a C99 compiler, and Make. No curses library or shell commands are needed at runtime. Python 3.8+ is optional for the integration tests.

```sh
make
./shapes
```

On macOS, install GCC with `brew install gcc`; the C adapter also builds with Apple's Clang. On Linux, install your distribution's `gfortran`, C compiler, and Make packages.

On FreeBSD, for example:

```sh
pkg install gcc14 gmake
gmake FC=gfortran14
./shapes
```

On other BSDs and illumos, use the available GNU Fortran compiler and GNU Make, specifying their paths if necessary:

```sh
gmake FC=/path/to/gfortran CC=/path/to/cc
```

On Windows, build in an [MSYS2 UCRT64 shell](https://www.msys2.org/):

```sh
pacman -S --needed make mingw-w64-ucrt-x86_64-gcc-fortran
make CC=gcc EXEEXT=.exe
./shapes.exe
```

The resulting Windows executable also runs from PowerShell or Command Prompt when its MinGW runtime DLLs are on `PATH`. WSL and Cygwin use the POSIX adapter when built with their own compilers.

Builds use portable optimization flags. You can override `FC`, `CC`, `FFLAGS`, `CFLAGS`, `CPPFLAGS`, `LDFLAGS`, `LDLIBS`, and `EXEEXT` on the Make command line.

## Terminal compatibility

| Terminal capability | Display |
| --- | --- |
| True color and UTF-8 | RGB foreground/background with two vertical pixels per cell |
| 256 or 16 colors | Colors mapped to the terminal palette; UTF-8 or ASCII glyphs |
| ANSI with a non-UTF-8 locale | Lit ASCII shapes with available colors |
| Monochrome or `NO_COLOR` | ASCII shading with cursor positioning |
| `TERM=dumb`, unknown terminal, or redirected input/output | One plain ASCII frame, without escape sequences or input waits |

Auto detection uses the terminal name, locale, `COLORTERM`, `TERM_PROGRAM`, and native Windows console capabilities. Apple Terminal defaults to its 256-color mode unless true color is explicitly advertised. A font must contain the upper half block character for the UTF-8 renderer; `--ascii` provides a fallback.

The POSIX adapter uses the host's `TIOCGWINSZ` definition and saved `termios` settings on macOS, Linux, BSD, and illumos. The Windows adapter enables [virtual terminal output](https://learn.microsoft.com/en-us/windows/console/setconsolemode), reads native key and mouse events, and measures the visible console window. MSYS2 pipe terminals use ANSI input/output; if a mintty setup buffers keyboard input, run `winpty ./shapes.exe` or use Windows Terminal. Consoles that cannot enable VT output fall back to a plain frame.

Resizing is detected through native events and periodic size checks. `COLUMNS` and `LINES` supply fallback dimensions. Where reported, window pixel dimensions determine character proportions; otherwise characters are assumed twice as tall as they are wide. One terminal column is reserved to avoid scrolling at the bottom right. Viewports are bounded to 1000 columns and 500 rows.

On exit, the adapter restores the saved input configuration, cursor, mouse reporting, and screen. POSIX interrupt, termination, and hangup signals take the cleanup path; Ctrl+Z restores the terminal before suspension and reopens the viewer on resume. Windows also restores its console modes and output code page.

For an unrecognized terminal that supports ANSI cursor positioning, use `--ansi`. Alternate screens and mouse reporting are enabled only for recognized terminal families. Very old terminals can always use `--snapshot`; an interactive color display requires the corresponding terminal capabilities.

```sh
./shapes --color 256                    # Palette terminal or multiplexer
./shapes --ascii --color 16 --no-mouse   # Console or limited font
./shapes --color mono                   # Monochrome interactive display
./shapes --ansi --no-alt-screen         # Basic ANSI terminal
./shapes --cell-aspect 0.6              # Correct unusual font proportions
./shapes --fps 15                       # Reduce animation traffic over SSH
./shapes --snapshot --shape torus-knot --size 100x30
./shapes --shape mobius-strip --theme studio-neutral
```

`NO_COLOR` selects monochrome automatically, including when set to an empty string. An explicit `--color` overrides auto detection. `--snapshot` always produces plain ASCII, including when color or Unicode was requested. `--size` sets snapshot dimensions or limits an interactive viewport to a smaller area.

## Shapes and controls

Cube, icosahedron, sphere, torus, tetrahedron, octahedron, dodecahedron, cylinder, cone, capsule, triangular prism, hexagonal prism, pyramid, rounded cube, torus knot, Möbius strip, star, crystal, spring, and ellipsoid. Use `--list-shapes` for numbers and `--shape NAME` to start with one; names accept spaces, hyphens, or underscores.

| Input | Action |
| --- | --- |
| Mouse drag | Virtual trackball rotation with inertia |
| Mouse wheel / `Z`, `X` | Zoom in / out |
| Arrow keys / `WASD` | Rotate |
| `1`–`9`, `0` | Select shapes 1–10 |
| `N` / Tab, `P` | Next / previous shape, through all 20 |
| `T` | Cycle themes |
| Space | Pause / resume automatic motion |
| `[` / `]`, `-` / `+` | Decrease / increase spin speed |
| `E` | Toggle edge highlights |
| `R` | Reset orientation and zoom |
| `H` | Hide / show the interface |
| `Q` / Ctrl+C / Ctrl+D | Quit |

Keyboard controls work without a mouse. CSI and application-mode arrows, modified arrows, split escape sequences, and SGR mouse events are supported. Bracketed paste and terminal title responses are consumed without treating their contents as commands.

`--help` lists all options, including `--no-spin`, `--no-hud`, `--no-edges`, `--no-mouse`, `--no-alt-screen`, and `--frames N`.

## Verification

```sh
make check
```

Mesh validation checks all 20 shapes at three detail levels for finite coordinates, valid indices, unit normals, framing bounds, and degenerate triangles. The Python integration suite renders every shape at multiple sizes and checks aspect ratios, CLI errors, plain output, and bounds. On POSIX it also opens real pseudo terminals to test color modes, keyboard and mouse input, live resizing, saved terminal settings, signals, and suspend/resume. Windows skips these POSIX-only tests.

The GitHub Actions workflow builds and tests on macOS, Linux, Windows, and FreeBSD, and compiles the terminal adapter on OmniOS/illumos. These jobs run when changes are pushed; a local pass does not establish that every terminal emulator or remote platform has been exercised.
