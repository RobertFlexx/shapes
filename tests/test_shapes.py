"""Integration checks using real pipes and, on POSIX, a pseudo terminal."""
import contextlib
import os
from pathlib import Path
import re
import select
import signal
import struct
import subprocess
import sys
import time
import unittest

try:
    import fcntl
    import pty
    import termios
except ImportError:
    pty = None

BINARY = str(Path(sys.argv.pop(1) if len(sys.argv) > 1 else "./shapes").resolve())
CURSOR = re.compile(rb"\x1b\[(\d+);(\d+)H")


def terminal_settings(descriptor):
    attributes = termios.tcgetattr(descriptor)
    # BSD kernels set this transient input-queue flag when returning to canonical mode.
    attributes[3] &= ~getattr(termios, "PENDIN", 0)
    return attributes


def run(*args, **kwargs):
    return subprocess.run([BINARY, *args], stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                          timeout=15, **kwargs)


class TerminalSession(contextlib.AbstractContextManager):
    def __init__(self, *args, width=81, height=24, env=None, redirected_input=False):
        self.master, self.slave = pty.openpty()
        self.resize(width, height)
        self.saved = terminal_settings(self.slave)
        environment = os.environ.copy()
        environment.pop("NO_COLOR", None)
        environment.update(TERM="xterm-256color", COLORTERM="truecolor", LC_ALL="C")
        environment.update(env or {})
        self.process = subprocess.Popen(
            [BINARY, *args], stdin=subprocess.DEVNULL if redirected_input else self.slave,
            stdout=self.slave, stderr=subprocess.PIPE, env=environment)
        os.set_blocking(self.master, False)
        self.output = bytearray()

    def resize(self, width, height):
        fcntl.ioctl(self.slave, termios.TIOCSWINSZ, struct.pack("HHHH", height, width, 0, 0))

    def drain(self, duration=0.05):
        deadline = time.monotonic() + duration
        while time.monotonic() < deadline:
            if not select.select([self.master], [], [], max(0, deadline - time.monotonic()))[0]:
                break
            try:
                data = os.read(self.master, 65536)
            except (BlockingIOError, OSError):
                break
            if not data:
                break
            self.output.extend(data)

    def wait_for(self, value, start=0):
        deadline = time.monotonic() + 5
        while time.monotonic() < deadline:
            self.drain()
            if value in self.output[start:]:
                return
            if self.process.poll() is not None:
                break
        raise AssertionError(f"missing {value!r}: {bytes(self.output[-500:])!r}")

    def send(self, data):
        os.write(self.master, data)

    def finish(self, data=None):
        if data is not None and self.process.poll() is None:
            self.send(data)
        deadline = time.monotonic() + 5
        while self.process.poll() is None and time.monotonic() < deadline:
            self.drain()
        if self.process.poll() is None:
            raise AssertionError("terminal application did not exit")
        self.drain()
        error = self.process.stderr.read()
        if self.process.returncode:
            raise AssertionError(f"exit {self.process.returncode}: {error!r}")
        if error:
            raise AssertionError(f"unexpected stderr: {error!r}")
        if terminal_settings(self.slave) != self.saved:
            raise AssertionError("original terminal attributes were not restored")
        return bytes(self.output)

    def __exit__(self, *exc):
        if self.process.poll() is None:
            self.process.kill()
        self.process.wait(timeout=5)
        self.process.stderr.close()
        os.close(self.master)
        os.close(self.slave)


class SnapshotTests(unittest.TestCase):
    def test_help_and_catalog(self):
        result = run("--help")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn(b"--snapshot", result.stdout)
        catalog = run("--list-shapes")
        self.assertEqual(catalog.returncode, 0, catalog.stderr)
        self.assertEqual(len(catalog.stdout.splitlines()), 20)
        self.assertIn(b"Mobius strip", catalog.stdout)

    def test_geometry_validation(self):
        result = run("--check-meshes")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn(b"All 20 meshes passed", result.stdout)

    def test_redirected_output_is_one_plain_frame(self):
        result = run("--size", "60x20", "--color", "truecolor", "--unicode", "--frames", "100")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertNotIn(b"\x1b", result.stdout)
        self.assertTrue(result.stdout.isascii())
        self.assertEqual(len(result.stdout.splitlines()), 20)
        self.assertTrue(all(len(row) <= 60 for row in result.stdout.splitlines()))

    def test_every_shape_at_two_sizes(self):
        for shape in range(1, 21):
            for width, height in [(80, 24), (23, 9)]:
                with self.subTest(shape=shape, size=(width, height)):
                    result = run("--snapshot", "--shape", str(shape), "--no-hud",
                                 "--size", f"{width}x{height}")
                    self.assertEqual(result.returncode, 0, result.stderr)
                    lines = result.stdout.splitlines()
                    self.assertEqual(len(lines), height)
                    self.assertTrue(all(len(line) <= width for line in lines))
                    self.assertRegex(result.stdout, rb"[:=+*#%@]")
                    self.assertNotIn(b"\x1b", result.stdout)

    def test_tiny_and_large_windows(self):
        for size in ["1x1", "2x2", "9x3", "160x48", "400x100"]:
            with self.subTest(size=size):
                result = run("--snapshot", "--shape", "sphere", "--size", size)
                self.assertEqual(result.returncode, 0, result.stderr)
                width, height = map(int, size.split("x"))
                self.assertEqual(len(result.stdout.splitlines()), height)
                self.assertTrue(all(len(row) <= width for row in result.stdout.splitlines()))

    def test_cell_aspect_preserves_geometry(self):
        bounds = []
        for aspect in ["0.5", "1.0"]:
            result = run("--snapshot", "--shape", "sphere", "--no-hud", "--no-edges",
                         "--size", "100x24", "--cell-aspect", aspect)
            self.assertEqual(result.returncode, 0, result.stderr)
            points = [(x, y) for y, row in enumerate(result.stdout.splitlines())
                      for x, char in enumerate(row) if char != 32]
            bounds.append((max(x for x, _ in points) - min(x for x, _ in points),
                           max(y for _, y in points) - min(y for _, y in points)))
        self.assertAlmostEqual(bounds[0][0] / bounds[1][0], 2, delta=0.2)
        self.assertAlmostEqual(bounds[0][1], bounds[1][1], delta=1)

    def test_names_and_themes(self):
        for name in ["TORUS-KNOT", "rounded_cube", "triangular prism", "Mobius strip"]:
            result = run("--snapshot", "--shape", name, "--theme=studio-neutral")
            self.assertEqual(result.returncode, 0, result.stderr)
            self.assertIn(b"Studio Neutral", result.stdout)

    def test_invalid_options_do_not_touch_terminal(self):
        for args in [("--shape", "0"), ("--shape", "21"), ("--shape", "unknown"),
                     ("--size", "0x1"), ("--size", "1x501"), ("--size", "invalid"),
                     ("--size", "999999999999999999999x1"), ("--fps", "0"),
                     ("--fps", "121"), ("--frames", "-1"), ("--color", "rainbow"),
                     ("--cell-aspect", "NaN"), ("--cell-aspect", "0"),
                     ("--frames", "1,2"), ("--shape", "1,2"), ("--size", "80,2x24"),
                     ("--cell-aspect", "0.5,1"),
                     ("--no-spin=yes",), ("--bogus",), ("--shape",)]:
            with self.subTest(args=args):
                result = run(*args)
                self.assertNotEqual(result.returncode, 0)
                self.assertIn(b"shapes:", result.stderr)
                self.assertNotIn(b"\x1b", result.stdout + result.stderr)

    def test_invalid_environment_sizes_are_bounded(self):
        env = dict(os.environ, COLUMNS="999999999999999999999999", LINES="-20")
        result = run("--snapshot", env=env)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(len(result.stdout.splitlines()), 24)
        self.assertTrue(all(len(row) <= 80 for row in result.stdout.splitlines()))


@unittest.skipIf(pty is None, "POSIX pseudo terminal APIs are unavailable")
class TerminalTests(unittest.TestCase):
    def test_real_terminal_size_overrides_stale_environment(self):
        with TerminalSession("--frames", "1", width=41, height=12,
                             env={"COLUMNS": "500", "LINES": "400"}) as terminal:
            output = terminal.finish()
            positions = [(int(row), int(col)) for row, col in CURSOR.findall(output)]
            self.assertTrue(positions)
            self.assertLessEqual(max(row for row, _ in positions), 12)
            self.assertLessEqual(max(col for _, col in positions), 40)
            self.assertIn(b"\x1b[?1049l", output)
            self.assertIn(b"\x1b[?25h", output)

    def test_color_and_glyph_modes(self):
        for color, marker in [("truecolor", b"38;2;"), ("256", b"38;5;"), ("16", b"\x1b[96m")]:
            with self.subTest(color=color):
                with TerminalSession("--frames", "1", "--color", color, "--unicode") as terminal:
                    output = terminal.finish()
                    if color != "16":
                        self.assertIn(marker, output)
                    else:
                        self.assertRegex(output, rb"\x1b\[(?:3\d|9\d)m")
                        self.assertNotIn(b"38;", output)
                    self.assertIn("▀".encode(), output)
        with TerminalSession("--frames", "1", env={"NO_COLOR": ""}) as terminal:
            output = terminal.finish()
            self.assertTrue(output.isascii())
            self.assertNotIn(b"38;", output)
            self.assertNotIn(b"48;", output)
        with TerminalSession("--frames", "1", "--ascii") as terminal:
            self.assertTrue(terminal.finish().isascii())

    def test_limited_terminals_and_redirected_input(self):
        for term in ["dumb", "unknown-terminal"]:
            with self.subTest(term=term):
                with TerminalSession(env={"TERM": term}) as terminal:
                    self.assertNotIn(b"\x1b", terminal.finish())
        with TerminalSession(redirected_input=True) as terminal:
            self.assertNotIn(b"\x1b", terminal.finish())
        with TerminalSession("--frames", "1", env={"TERM": "vt100", "COLORTERM": ""}) as terminal:
            output = terminal.finish()
            self.assertTrue(output.isascii())
            self.assertNotIn(b"38;", output)
            self.assertNotIn(b"?1049", output)
            self.assertNotIn(b"?1006", output)

    def test_basic_ansi_override_and_console_families(self):
        for term in ["unknown-terminal", "dumb", "cons25", "wsvt25", "sun"]:
            with self.subTest(term=term):
                args = ("--ansi",) if term in ["unknown-terminal", "dumb"] else ()
                with TerminalSession("--frames", "1", *args, env={"TERM": term, "COLORTERM": ""}) as terminal:
                    output = terminal.finish()
                    self.assertTrue(CURSOR.search(output))
                    self.assertNotIn(b"?1049", output)
                    self.assertNotIn(b"?1006", output)

    def test_environment_capabilities(self):
        with TerminalSession("--frames", "1", env={"COLORTERM": "", "TERM_PROGRAM": "Apple_Terminal"}) as terminal:
            output = terminal.finish()
            self.assertIn(b"38;5;", output)
            self.assertNotIn(b"38;2;", output)
            self.assertTrue(output.isascii())  # The C locale does not promise UTF-8.
        with TerminalSession("--frames", "1", "--color", "truecolor", env={"NO_COLOR": "1"}) as terminal:
            self.assertIn(b"38;2;", terminal.finish())

    def test_keyboard_mouse_and_fragmented_escape_sequences(self):
        with TerminalSession("--no-spin") as terminal:
            terminal.wait_for(b"1/20 Cube")
            self.assertFalse(termios.tcgetattr(terminal.slave)[3] & termios.ECHO)
            for fragment in [b"\x1b[1;", b"5A\x1bO", b"D\x1b[<0;40;", b"12M",
                             b"\x1bO1;", b"5D",
                             b"\x1b[<32;45;13M\x1b[<0;45;13m", b"\x1b[<64;40;12M",
                             b"\x1b[200~qntH\x1b[201~", b"\x1b]0;qntH\x07"]:
                terminal.send(fragment)
                terminal.drain(0.04)
            self.assertIsNone(terminal.process.poll())
            self.assertNotIn(b"5/20 Tetrahedron", terminal.output)
            terminal.send(b"0")
            terminal.wait_for(b"10/20 Capsule")
            terminal.send(b"n")
            terminal.wait_for(b"11/20 Triangular prism")
            terminal.finish(b"q")

    def test_all_shapes_are_reachable_and_wrap(self):
        with TerminalSession("--shape", "20", "--no-spin") as terminal:
            terminal.wait_for(b"20/20 Ellipsoid")
            terminal.send(b"\t")
            terminal.wait_for(b"1/20 Cube")
            terminal.send(b"p")
            terminal.wait_for(b"20/20 Ellipsoid")
            terminal.finish(b"\x04")

    def test_resize_smaller_and_larger(self):
        with TerminalSession("--no-spin", width=100, height=35) as terminal:
            terminal.wait_for(b"1/20 Cube")
            for width, height in [(24, 7), (2, 2), (120, 40)]:
                start = len(terminal.output)
                terminal.resize(width, height)
                terminal.process.send_signal(signal.SIGWINCH)
                terminal.wait_for(b"\x1b[2J", start)
                terminal.drain(0.15)
                positions = [(int(row), int(col)) for row, col in CURSOR.findall(terminal.output[start:])]
                self.assertTrue(positions)
                self.assertLessEqual(max(row for row, _ in positions), height)
                self.assertLessEqual(max(col for _, col in positions), width - 1)
            terminal.finish(b"q")

    def test_signals_restore_exact_terminal_state(self):
        for number in [signal.SIGINT, signal.SIGTERM, signal.SIGHUP]:
            with self.subTest(signal=number):
                with TerminalSession() as terminal:
                    terminal.wait_for(b"1/20 Cube")
                    terminal.process.send_signal(number)
                    output = terminal.finish()
                    self.assertIn(b"\x1b[?1006l", output)
                    self.assertIn(b"\x1b[?1049l", output)

    def test_suspend_and_resume(self):
        with TerminalSession("--no-spin") as terminal:
            terminal.wait_for(b"1/20 Cube")
            terminal.process.send_signal(signal.SIGTSTP)
            deadline = time.monotonic() + 5
            stopped = False
            while time.monotonic() < deadline:
                terminal.drain()
                pid, status = os.waitpid(terminal.process.pid, os.WUNTRACED | os.WNOHANG)
                if pid and os.WIFSTOPPED(status):
                    stopped = True
                    break
            self.assertTrue(stopped, "application did not suspend")
            self.assertEqual(terminal_settings(terminal.slave), terminal.saved)
            start = len(terminal.output)
            terminal.process.send_signal(signal.SIGCONT)
            terminal.wait_for(b"1/20 Cube", start)
            terminal.finish(b"q")


if __name__ == "__main__":
    unittest.main(verbosity=2)
