"""Console status control: A thin wrapper around rich functionality."""

import logging
import sys
from functools import cache
from typing import NoReturn

import typer
from loguru import logger
from rich.console import Console
from rich.panel import Panel
from rich.theme import Theme

# Copy some settings from typer.
from typer.rich_utils import (
    ALIGN_ERRORS_PANEL,
    ERRORS_PANEL_TITLE,
    STYLE_ERRORS_PANEL_BORDER,
)

THEME = Theme(
    {
        "info": "dim cyan",
        "warning": "magenta",
        "error": "bold red",
        "heading": "bold underline blue",
        "success": "bold green",
    }
)

_LOGGING_READY = False
_DEBUG = False


class BDHSConsole:
    """Simplify what we need from Console by wrapping it."""

    def __init__(self):
        self.console = Console(stderr=True, theme=THEME)

    def print(self, *args, **kwargs):  # noqa A003
        self.console.print(*args, **kwargs)

    def exit_with_error(self, message: str, exit_code: int = 1) -> NoReturn:
        self.console.print(
            Panel(
                message,
                border_style=STYLE_ERRORS_PANEL_BORDER,
                title=ERRORS_PANEL_TITLE,
                title_align=ALIGN_ERRORS_PANEL,
            )
        )
        raise typer.Exit(exit_code)


def init_logging(debug: bool = False):
    """Replaces logging handlers with a handler for using the custom handler."""

    # Make sure we only do this once.
    global _LOGGING_READY
    global _DEBUG
    if not _LOGGING_READY:
        _DEBUG = debug
        _LOGGING_READY = True

    # Two levels for simplicity.
    if _DEBUG:
        level = logging.DEBUG
    else:
        level = logging.INFO

    # Remove the default, add in a single to stderr.
    logger.remove()
    logger.add(sys.stderr, level=level)


@cache
def get_console():
    return BDHSConsole()
