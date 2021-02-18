Connections
===========

Defining connection types
-------------------------

The code which establishes connections (i.e., implementations of the
``ESTABLISH-CONNECTION`` generic) is like code in ``:posix`` properties -- it
should restrict its I/O to ``RUN``, ``RUNLINES``, ``READFILE`` and
``WRITEFILE``, functions which access the currently active connection.  This
is in order to permit the arbitrary nesting of connections.
