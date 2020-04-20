Reference implementation of srfi-181 (Custom Ports) and 
srfi-192 (Port positioning).

This is not intended to be a drop-in library, since it is not 
possible to extend implementation's built-in ports portably.
Rather, it intends to illustrate details.

Portabl R7RS implementation includes an auxiliary library,
(srfi 181 adapter), which replaces I/O procedures in (scheme base)
so that they can accept custom ports.
