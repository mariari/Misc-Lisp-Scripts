/*
 * LISP9 Interpreter
 * Nils M Holm, 2018,2019
 * Mariari 2021
 * In the public domain
 *
 * If your country does not have a concept like the public
 * domain, the Creative Common Zero (CC0) licence applies,
 * see https://creativecommons.org/publicdomain/zero/1.0/
 */

/* Edits will sometimes be made when following the books */

#define VERSION "2021-10-11"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <limits.h>
#include <signal.h>
#include <setjmp.h>

/*
 * Tunable parameters
 */

#define IMAGEFILE	"ls9.image"
#define IMAGESRC	"ls9.ls9"

/* number of node cells available to the interpreter */
#define NNODES		262144
/* number of vector cells available to the interpreter */
#define NVCELLS		262144

/* node is the smallest amount of storage the interpreter can
  allocate */

/* For this interpreter the allocation size is fixed at 256K nodes and
  vectors, which go to 3.25M (with an int size of 4!). Growing this is
  not included with this compiler/interpreter */

/* Number of I/O ports, this will be GC unused ports */
#define NPORTS		20
/* Max size of all kinds of tokens.... even comments! */
#define TOKLEN		80
/* the amount of size all kinds of internal objects will grow. Trade
  off between memory footprint and performance. Low numbers cause
  reallocation. */
#define CHUNKSIZE	1024
/* Number of recursive calls to the macro expander. Maximum number of
  recursive macro applications */
#define MXMAX		2000
/* Number of references to free variables that will print in case of an
  error. Debug info is space and the abstract machine keeps track of
  names stored in a ring buffer. This is the size of the ring buffer. */
#define NTRACE		10
/* Maximum depth of a structure will print before aborting */
#define PRDEPTH		1024


/*
 * Basic data types
 */

/* Used to refer to the data object in the node pool. Some objects can
  fit within a cell and do not need to be allocated to a node/vector
  pool */
#define cell	int
/* Octects in the range  [0 ... 255] */
#define byte	unsigned char
#define uint	unsigned int

/*
 * Special Objects
 */

/* Special objects need not be allocated to the heap. Any positive
 * number object must be allocated to the Heap, while these negative
 * ones aren't allocated to the heap
 */

#define specialp(x)	((x) < 0)
#define NIL		(-1)
#define TRUE		(-2)
/* Returned by input functions when they encounter end of input */
#define EOFMARK		(-3)
/* Undefined results */
#define UNDEF		(-4)
/* Used by the reader to denote when \) or . is read */
#define RPAREN		(-5)
#define DOT		(-6)


/*
 * Memory pools
 */

/* This is interesting, as the *node pool* is a vector where each
 * element has 3 fields [car, cdr, tag]. When the tag is 0, it is a
 * cons cell. the car and cdr are cells. In a diagram we can see that
 * for (123 t) that it may look like. Where our answer is in cell
 * labeled 3.
 */
/*
       1
   --------
   | TRUE | This is a list with the car being True
   | NIL  | and the Cdr being Nil
   |      |
   --------
     (t)

     2
   -----
   |   |
   |   |
   |   |
   -----
   unused

     3
   -----
   | 5 |  we can see this points to cell 5 for the car
   | 1 |  and cell 1 for the cdr
   |   |
   -----
  (123 t)

        4
   ------------
   | 123      |
   | NIL      |
   | ATOM_TAG |
   ------------

        5
   ------------
   | T_FIXNUM |
   | 4        |
   | ATOM_TAG |
   ------------
      123
 */

/* Since nodes refer to each other by index, this makes dumping the
 * image quite simple
 */

cell	*Car = NULL,
	*Cdr = NULL;
byte	*Tag = NULL;

/* The vector pool holds the value of all vectors objects such as
 * [symbols, strings, vectors]. They have a VECTOR_TAG instead of an
 * ATOM_TAG. VECTOR_TAGs have a type param in the CAR, CDR points to
 * the vector pool instead of the node pool. Vecotr pool is an array
 * of cells.
 */



int main() { return 0; }
