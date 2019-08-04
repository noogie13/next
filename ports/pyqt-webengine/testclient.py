import random
import sys
from traceback import print_exc

import dbus


"""
Simple code to send dbus signals to the running server.
For development.
"""


def main():
    OBJECT_PATH = "/engineer/atlas/next/platform"
    INTERFACE_PATH = "engineer.atlas.next.platform"

    bus = dbus.SessionBus()

    args = []
    if "-d" in sys.argv:
        # Delete window.
        window_id = sys.argv[-1]
        # We must use getattr because of dotted function names/signal names.
        command = "window_delete"
        args = [window_id, ]

    elif "-t" in sys.argv:
        # Set title.
        window_id = sys.argv[-2]
        title = sys.argv[-1]
        command = "window_set_title"
        args = [window_id, title]

    elif "-a" in sys.argv:
        # Active window ?
        command = "window_active"

    elif "-h" in sys.argv:
        # set minibuffer height (int).
        command = "window_set_minibuffer_height"
        args = [sys.argv[-2], int(sys.argv[-1])]

    elif "-k" in sys.argv:
        # kill all windows.
        command = "window_killall"

    elif "-e" in sys.argv:
        # generate an input event
        command = "generate_input_event"

    elif "-mj" in sys.argv:
        # Minibuffer evaluate JS.
        command = "minibuffer_evaluate_javascript"
        script = "document.body.innerHTML = \"{}\"".format(sys.argv[-1])
        args = [sys.argv[-2], script]

    elif "-w" in sys.argv:
        # list windows.
        command = "window_list"

    elif "-b" in sys.argv:
        # list buffers.
        command = "buffer_list"

    else:
        # Make window.
        # give a pseudo random str as window id, we don't keep count of windows here.
        uid = str(random.randrange(10))
        print("window make id {}:".format(uid))
        command = "window_make"
        args = [uid, ]

    # Run command.
    try:
        proxy_object = bus.get_object(INTERFACE_PATH,
                                      OBJECT_PATH)
        reply = getattr(proxy_object, command)(*args)
        # hello_reply = proxy_object.ping(dbus_interface=INTERFACE_PATH)
    except dbus.DBusException:
        print_exc()
        sys.exit(1)
    print(reply)

if __name__ == "__main__":

    main()
