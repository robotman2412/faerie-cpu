
from enum import Enum

from srcfile import *
from typing import Callable



class Severity(Enum):
    HINT  = 0
    WARN  = 1
    ERROR = 2


msg_name = ["Hint", "Warning", "Error"]
msg_col  = ["\033[35m", "\033[33m", "\033[31m"]
no_col   = "\033[0m"

def print_msg(type: Severity, msg: str, loc: Location = None):
    def _print(type: Severity, msg: str, loc_str: str):
        print(msg_col[type.value] + msg_name[type.value] + " " + loc_str + ": " + msg + no_col)
    if not loc:
        _print(type, msg, "<anonymous>:?")
        return
    
    # Show message location.
    _print(type, msg, str(loc))
    loc.show()
    link = loc
    while link.inc_from:
        _print(Severity.HINT, f"Included from {link.file.name}", str(link.inc_from))
        link.inc_from.show()
        link = link.inc_from
    
    # Show macro it was expanded from.
    file = loc.file
    while loc.exp_from:
        _print(Severity.HINT, f"Expanded from macro {loc.exp_from.macro}", str(loc.exp_from))
        loc.exp_from.show()
        link = loc.exp_from
        while link.inc_from and link.file is not file:
            _print(Severity.HINT, f"Included from {link.file.name}", str(link.inc_from))
            link.inc_from.show()
            link = link.inc_from
        loc = loc.exp_from


_msg_log: list[tuple[Severity,str,Location]] = []

def get_msg_log():
    return _msg_log

def clear_msg_log():
    global _msg_log
    _msg_log = []

def log_msg(type: Severity, msg: str, loc: Location = None):
    _msg_log.append((type, msg, loc))


_on_msg_handler = print_msg

def set_msg_handler(handler: Callable[[Severity,str,Location], None]):
    global _on_msg_handler
    _on_msg_handler = handler

def get_msg_handler() -> Callable[[Severity,str,Location], None]:
    return _on_msg_handler


_has_errors = False

def has_errors():
    return _has_errors

def clear_errors():
    global _has_errors
    _has_errors = False


def raise_msg(type: Severity, msg: str, loc: Location = None):
    _on_msg_handler(type, msg, loc)

def raise_warn(msg: str, loc: Location = None):
    raise_msg(Severity.WARN, msg, loc)

def raise_err(msg: str, loc: Location = None):
    global _has_errors
    _has_errors = True
    raise_msg(Severity.ERROR, msg, loc)

class AsmError(Exception):
    def __init__(self, msg: str, loc: Location = None):
        Exception.__init__(self, msg)
        self.loc = loc
