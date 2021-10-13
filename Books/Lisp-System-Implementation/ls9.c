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
   |   123    |
   |   NIL    |
   | ATOM_TAG |
   ------------

        5
   ------------
   | T_FIXNUM |
   |    4     |
   | ATOM_TAG |
   ------------
      123
 */

/* Since nodes refer to each other by index, this makes dumping the
 * image quite simple
 */

/* Upon review of the book, we can see that we use a Struct of arrays
  technique, where the Car Cdr and Tag are all separate arrays instead
  of having a struct with the three fields together. */
cell	*Car = NULL,
	*Cdr = NULL;
byte	*Tag = NULL;

/* The vector pool holds the value of all vectors objects such as
 * [symbols, strings, vectors]. They have a VECTOR_TAG instead of an
 * ATOM_TAG. VECTOR_TAGs have a type param in the CAR, CDR points to
 * the vector pool instead of the node pool. Vecotr pool is an array
 * of cells.
 */

cell	*Vectors = NULL;

/* These variables keep track of free space */

/* List of unused Nodes */
cell	Freelist = NIL;
/* Points to the unallocated part of vectors */
cell	Freevec = 0;

#define ATOM_TAG	0x01	/* Atom, CAR = type, CDR = next */
#define MARK_TAG	0x02	/* Mark */
#define TRAV_TAG	0x04	/* Traversal */
#define VECTOR_TAG	0x08	/* Vector, CAR = type, CDR = content */
#define PORT_TAG	0x10	/* Atom is an I/O port (with ATOM_TAG) */
#define USED_TAG	0x20	/* Port: used flag */
#define LOCK_TAG	0x40	/* Port: locked (do not close) */
#define CONST_TAG	0x80	/* Node is immutable */

/* Accessing the fields of the the nodes */
#define tag(n)		(Tag[n])
#define car(x)          (Car[x])
#define cdr(x)          (Cdr[x])

/* Time to get lisp style accessors */

#define caar(x)         (Car[Car[x]])
#define cadr(x)         (Car[Cdr[x]])
#define cdar(x)         (Cdr[Car[x]])
#define cddr(x)         (Cdr[Cdr[x]])
#define caaar(x)        (Car[Car[Car[x]]])
#define caadr(x)        (Car[Car[Cdr[x]]])
#define cadar(x)        (Car[Cdr[Car[x]]])
#define caddr(x)        (Car[Cdr[Cdr[x]]])
#define cdaar(x)        (Cdr[Car[Car[x]]])
#define cdadr(x)        (Cdr[Car[Cdr[x]]])
#define cddar(x)        (Cdr[Cdr[Car[x]]])
#define cdddr(x)        (Cdr[Cdr[Cdr[x]]])
#define caaaar(x)       (Car[Car[Car[Car[x]]]])
#define caaadr(x)       (Car[Car[Car[Cdr[x]]]])
#define caadar(x)       (Car[Car[Cdr[Car[x]]]])
#define caaddr(x)       (Car[Car[Cdr[Cdr[x]]]])
#define cadaar(x)       (Car[Cdr[Car[Car[x]]]])
#define cadadr(x)       (Car[Cdr[Car[Cdr[x]]]])
#define caddar(x)       (Car[Cdr[Cdr[Car[x]]]])
#define cadddr(x)       (Car[Cdr[Cdr[Cdr[x]]]])
#define cdaaar(x)       (Cdr[Car[Car[Car[x]]]])
#define cdaadr(x)       (Cdr[Car[Car[Cdr[x]]]])
#define cdadar(x)       (Cdr[Car[Cdr[Car[x]]]])
#define cdaddr(x)       (Cdr[Car[Cdr[Cdr[x]]]])
#define cddaar(x)       (Cdr[Cdr[Car[Car[x]]]])
#define cddadr(x)       (Cdr[Cdr[Car[Cdr[x]]]])
#define cdddar(x)       (Cdr[Cdr[Cdr[Car[x]]]])
#define cddddr(x)       (Cdr[Cdr[Cdr[Cdr[x]]]])

/*
 * Tagged data types
 */

/* Tagged Data often looks like this
 * -----------     ---------------
 * | Tag | --|---->| Value | Nil |
 * -----------     ---------------
 * With the tag in the car field.
 * NIL or a circle in the car field means it's a cons cell. Otherwise
 * it's an atom.
 */

/* Marks a chunk of bytecode generated by the compiler. */
#define T_BYTECODE	(-10)
/* Label for non local exits. */
#define T_CATCHTAG	(-11)
#define T_CHAR		(-12)
#define T_CLOSURE	(-13)
#define T_FIXNUM	(-14)
#define T_INPORT	(-15)
#define T_OUTPORT	(-16)
#define T_STRING	(-17)
#define T_SYMBOL	(-18)
#define T_VECTOR	(-19)

/*
 * Basic constructors
 */

/* Principled constructors of the LISP9 system. */

#define cons(a, d)	cons3((a), (d), 0)
#define mkatom(a, d)	cons3((a), (d), ATOM_TAG)

/*
 * Accessors
 */

/* string, vector, and symname all extract (a pointer to) the bytes of
 * a string, cells of a vector and characters of a symbol name
 * respectively.
 */

#define portno(n)	(cadr(n))
#define string(n)	((byte *) &Vectors[Cdr[n]])
#define stringlen(n)	(Vectors[Cdr[n] - 1])
#define symname(n)	(string(n))
#define symlen(n)	(stringlen(n))
#define vector(n)	(&Vectors[Cdr[n]])
#define veclink(n)	(Vectors[Cdr[n] - 2])
#define vecndx(n)	veclink(n)
#define vecsize(k)	(2 + ((k) + sizeof(cell)-1) / sizeof(cell))
#define veclen(n)	(vecsize(stringlen(n)) - 2)

/*
 * Type predicates
 */

#define charp(n) \
	(!specialp(n) && (tag(n) & ATOM_TAG) && T_CHAR == car(n))

#define closurep(n) \
	(!specialp(n) && (tag(n) & ATOM_TAG) && T_CLOSURE == car(n))

#define ctagp(n) \
	(!specialp(n) && (tag(n) & ATOM_TAG) && T_CATCHTAG == car(n))

#define eofp(n)	(EOFMARK == (n))

/*      5
 * ------------
 * | T_FIXNUM |
 * |    4     |
 * | ATOM_TAG |
 * ------------
 *    123
 */

/* Fixp makes sense given this layout in the cell itself */

#define fixp(n) \
	(!specialp(n) && (tag(n) & ATOM_TAG) && T_FIXNUM == car(n))

#define inportp(n) \
	(!specialp(n) && (tag(n) & ATOM_TAG) && \
	 (tag(n) & PORT_TAG) && T_INPORT == car(n))

#define outportp(n) \
	(!specialp(n) && (tag(n) & ATOM_TAG) && \
	 (tag(n) & PORT_TAG) && T_OUTPORT == car(n))

#define stringp(n) \
	(!specialp(n) && (tag(n) & VECTOR_TAG) && T_STRING == car(n))

#define symbolp(n) \
	(!specialp(n) && (tag(n) & VECTOR_TAG) && T_SYMBOL == car(n))

#define vectorp(n) \
	(!specialp(n) && (tag(n) & VECTOR_TAG) && T_VECTOR == car(n))

#define atomp(n) \
	(specialp(n) || (tag(n) & ATOM_TAG) || (tag(n) & VECTOR_TAG))

#define pairp(x) (!atomp(x))

#define listp(x) (NIL == (x) || pairp(x))

#define constp(n) \
	(!specialp(n) && (tag(n) & CONST_TAG))


/*
 * Abstract machine opcodes
 */

/* Only the first 29 are machine instructions */

enum {	OP_ILL, OP_APPLIS, OP_APPLIST, OP_APPLY, OP_TAILAPP, OP_QUOTE,
	OP_ARG, OP_REF, OP_PUSH, OP_PUSHTRUE, OP_PUSHVAL, OP_POP,
	OP_DROP, OP_JMP, OP_BRF, OP_BRT, OP_HALT, OP_CATCHSTAR,
	OP_THROWSTAR, OP_CLOSURE, OP_MKENV, OP_PROPENV, OP_CPREF,
	OP_CPARG, OP_ENTER, OP_ENTCOL, OP_RETURN, OP_SETARG, OP_SETREF,
	OP_MACRO,

        /* The rest of these are just inlined lisp functions */
	OP_ABS, OP_ALPHAC, OP_ATOM, OP_BITOP, OP_CAAR, OP_CADR, OP_CAR,
	OP_CDAR, OP_CDDR, OP_CDR, OP_CEQUAL, OP_CGRTR, OP_CGTEQ,
	OP_CHAR, OP_CHARP, OP_CHARVAL, OP_CLESS, OP_CLOSE_PORT,
	OP_CLTEQ, OP_CMDLINE, OP_CONC, OP_CONS, OP_CONSTP, OP_CTAGP,
	OP_DELETE, OP_DIV, OP_DOWNCASE, OP_DUMP_IMAGE, OP_EOFP, OP_EQ,
	OP_EQUAL, OP_ERROR, OP_ERROR2, OP_ERRPORT, OP_EVAL, OP_EXISTSP,
	OP_FIXP, OP_FLUSH, OP_FORMAT, OP_FUNP, OP_GC, OP_GENSYM,
	OP_GRTR, OP_GTEQ, OP_INPORT, OP_INPORTP, OP_LESS, OP_LISTSTR,
	OP_LISTVEC, OP_LOAD, OP_LOWERC, OP_LTEQ, OP_MAX, OP_MIN,
	OP_MINUS, OP_MKSTR, OP_MKVEC, OP_MX, OP_MX1, OP_NCONC,
	OP_NEGATE, OP_NRECONC, OP_NULL, OP_NUMERIC, OP_NUMSTR,
	OP_OBTAB, OP_OPEN_INFILE, OP_OPEN_OUTFILE, OP_OUTPORT,
	OP_OUTPORTP, OP_PAIR, OP_PEEKC, OP_PLUS, OP_PRIN, OP_PRINC,
	OP_QUIT, OP_READ, OP_READC, OP_RECONC, OP_REM, OP_RENAME,
	OP_SCONC, OP_SEQUAL, OP_SETCAR, OP_SETCDR, OP_SET_INPORT,
	OP_SET_OUTPORT, OP_SFILL, OP_SGRTR, OP_SGTEQ, OP_SIEQUAL,
	OP_SIGRTR, OP_SIGTEQ, OP_SILESS, OP_SILTEQ, OP_SLESS, OP_SLTEQ,
	OP_SREF, OP_SSET, OP_SSIZE, OP_STRINGP, OP_STRLIST, OP_STRNUM,
	OP_SUBSTR, OP_SUBVEC, OP_SYMBOL, OP_SYMBOLP, OP_SYMNAME,
	OP_SYMTAB, OP_SYSCMD, OP_TIMES, OP_UNTAG, OP_UPCASE, OP_UPPERC,
	OP_VCONC, OP_VECLIST, OP_VECTORP, OP_VFILL, OP_VREF, OP_VSET,
	OP_VSIZE, OP_WHITEC, OP_WRITEC };

/*
 * I/O functions
 */

void prints(char *s);
void prin(cell x);

#define printb(s) prints((char *) s)
#define nl()      prints("\n")

/* Sets the current output port to the port number passed to it. */
int set_outport(int port);

/*
 * Error reporting and handling
 */

/* These two variables form a ring buffer with TP pointing to the next
 * slot to fill in the Trace.
 */

int Trace[NTRACE];
int Tp = 0;

/* cltrace clears the buffer by setting all the values in the ring
 * buffer to -1. Since these values are indexed into the node buffer
 * and thus offsets can't be negative this works
 */
void clrtrace(void) {
    int i;
    for (i = 0; i < NTRACE; i++)
        Trace[i] = -1;
}

int gottrace(void) {
    int i;
    for (i = 0; i < NTRACE; i++)
        if (Trace[i] != -1)
            return 1;
    return 0;
}

/* The Plimit variable sets a limit to printer output. Printing will be
 * aborted after passing the threshold
 */

int Plimit = 0;

/* Current input line number */
int Line = 1;
/* Current File ontop of the stack */
cell Files = NIL;

/* bound to a vector holding all Symbol names known in the system. Used
 * to report references to free variables
*/
cell Symbols;

/* converts a fixnum to a string representing it with some radex
 * r. Then writes it to an internal buffer returns a pointer to said
 * buffer.
*/
char *ntoa(int x, int r);

/* Signals an error by printing the message s. If the object is not
 * undefined, we also print the object along with the location in the
 * file.
 */
void report (char *s, cell x) {
    int i,j;

    int o = set_outport(2);
    prints("*** error: ");
    prints(s);
    if (x != UNDEF) {
        prints(": ");
        /* Very silly we set plimit so print works properly */
        Plimit = 100;
        prin(x);
        Plimit = 0;
    }
    nl();
    if (Files != NIL) {
        prints("*** file: ");
        printb(string(car(Files)));
        prints(", line: ");
        printb(ntoa(Line, 10));
        nl();
    }
    if (gottrace()) {
        prints("*** trace:");
        i = Tp;
        for (j = 0; j < NTRACE; j++) {
            if (i >= NTRACE)
                i = 0;
            if (Trace[i] != -1) {
                prints(" ");
                printb(symname(vector(Symbols)[Trace[i]]));
            }
            i++;
        }
        nl();
    }
    set_outport(o);
}

/* Long jump needed for non local exits */

/* After signaling an error control is abroted passing control to the
 * Restart Label
 */
jmp_buf	Restart;
jmp_buf	Errtag;

/* The variable handle tag, when no handler is in effect it's bound to
 * NIL
*/
cell Handler = NIL;

/* Forward declaration of variables bound to the global environment */
cell Glob;
cell S_errtag, S_errval;

/* assq searches an association list */
int  assq(cell x, cell a);
/* binds a new value to the global variable */
void bindset(cell v, cell a);
/* mkstr makes a string object */
cell mkstr(char *s, int k);


/* If an error tag is signaled, and an error handler is in effect, we
 * then goto Errtag, otherwise we go to report the error and restart
 */
void error(char *s, cell x) {
    cell n;
    n = assq(S_errtag, Glob);
    Handler = (NIL == n) ? NIL : cadr(x);
    if (Handler != NIL) {
        n = assq(S_errval, Glob);
        if (n != NIL && cadr(n) == Handler)
            bindset(S_errval, mkstr(s, strlen(s)));
        longjmp(Errtag, 1);
    }
    report(s, x);
    longjmp(Restart, 1);
}

/* (+ 1 2 "hi") */
/* *** error: +: expected fixnum: "hi" */
void expect(char *who, char *what, cell got) {
    char b[100];

    sprintf(b, "%s: expected %s", who, what);
    error(b, got);
}

/* fatal is called when there is no chance at recovery
 * This happens in instances like
 * - Unable to allocate to the memory pools
 * - finding no open input or output port
 * - attempting to load a corrupted image file
 * - encountering an error during compilation of the initial heap image
 * Calling fatal prints a message and terminates the process
 */
void fatal(char *s) {
    fprintf(stderr, "*** fatal error:");
    fprintf(stderr, "%s\n", s);
    exit(EXIT_FAILURE);
}

/*
 * Low-level input/output
 */

/* Maps port objects to C FILEs */
FILE *Ports[NPORTS];
/* Contains the garbage collector bits used in combination with ports */
char Port_flags[NPORTS];

/* Bound to stdin */
int Inport = 0;

/* Bound to stdout */
int Outport = 1;

/* bound to stdout files */
int Errport = 2;

cell Outstr = NIL;
int  Outmax = 0;
int  Outptr = 0;

char *Instr = NULL;
char Rejected = -1;

/* Principled input stream. Whenever readc is called it will return the
 * next character from the current input port (or a string, when Instr
 * is not NULL).
 */

int readc(void) {
    int c;
    if (Instr != NULL) {
        if (Rejected > -1) {
            c = Rejected;
            Rejected = -1;
            return c;
        }
        if (0 == *Instr) {
            return EOF;
        }
        else {
            return *Instr++;
        }
    }
    else {
        if (NULL == Ports[Inport])
            fatal("readc: input port is not open");
        return getc(Ports[Inport]);
    }
}

/* reject puts the character c back into the current input source When
 * the source is read for the next time it will produce that character
 * again
 */
void rejectc(int c) {
    /* We are dealing with a String */
    if (Instr != NULL) {
        Rejected = c;
    }
    else {
        ungetc(c, Ports[Inport]);
    }
}

/* Creates a port object and wrpas it in the port number in a port
 * object
 */
cell mkport(int p, cell t);

/* flush writes all pending output to the file or device */
void flush(void) {
    if (fflush(Ports[Outport]))
        error("file write error, port", mkport(Outport, T_OUTPORT));
}

/* Principled output function of the LISP9 system All output is passed
 * through this interface. The function write k characters from the
 * string s to the current output port. to or an output string if
 * Outstr is not NIL. When writing to stdout (1) or stderr (2) port
 * and the last character is a newline it will flush after writing.
 * Will also subtract k from Plimit variable and will stop when the
 * value is below 1. Note that it ignores it if it starts out as 0.
 */
void blockwrite(char *s, int k) {
    cell n;

    if (1 == Plimit)
        return;
    if (Outstr != NIL) {
        while (Outptr + k >= Outmax) {
            n = mkstr(NULL, Outmax+1000);
            memcpy(string(n), string(Outstr), Outptr);
            Outmax += 1000;
            Outstr = n;
        }
        memcpy(&string(Outstr)[Outptr], s, k);
        Outptr += k;
        string(Outstr)[Outptr] = 0;
        return;
    }
    if (NULL == Ports[Outport])
        fatal("blockwrite: output port is not open");

    int writen_chars = fwrite(s, 1, k, Ports[Outport]);
    if (writen_chars != k)
        error("file write error, port",
              mkport(Outport, T_OUTPORT));
    if ((1 == Outport || 2 == Outport) && '\n' == s[k-1])
        flush();
    if (Plimit) {
        Plimit -= k;
        if (Plimit < 1) Plimit = 1;
    }
}


/* write function writes the single character c to the current output
 * Port (or string)
 */

void writec(int c) {
    char b[1];
    b[0] = c;
    blockwrite(b, 1);
}


/* prints() writes the string x */
void prints(char *s) {
    blockwrite(s, strlen(s));
}

/*
 * Memory management
 */

/* all LISP data have indefinite extent */

/* Seems Baker and Moon wrote the Ephemeral GC of the lisp machine */

/* We shall implement 2 parts a mark-and-sweep collector, and a
 * compacting collector that defrags the vector pool
 */

/* First step is allocation */
/* The system allocates 3 arrays Car Cdr and Tag. for the node pool
 * along with a single array Vectors fro the vector objects
 */


/* Pool allocation is done via these two functions. Failure to allocate
  is a fatal error */

void alloc_nodepool(void) {
    size_t size_cell_pool = sizeof(cell) * NNODES;

    Car = malloc(size_cell_pool);
    Cdr = malloc(size_cell_pool);
    Tag = malloc(NNODES);
    if (NULL == Car || NULL == Cdr || NULL == Tag)
        fatal("alloc_nodepool: out of physical memory");
    memset(Car, 0, size_cell_pool);
    memset(Cdr, 0, size_cell_pool);
    memset(Tag, 0, NNODES);
}

void alloc_vecpool(void) {
    size_t size_pool = sizeof(cell) * NVCELLS;
    Vectors = malloc (size_pool);
    if (NULL == Vectors)
        fatal("alloc_vecpool: out of physical memory");
    memset(Vectors, 0, size_pool);
}

/* We start the discussion of memory management with reclamation of
 * unused data objects
 */

/* Due to forms like
 *
 * (defun (f x) (if (= x 0) nil (cons 1 (f (- x 1)))))
 *
 * Having objects like 0 nil and 1, once we save the code, we can't
 * just remove the ast. thus we have an Object table that contains all
 * data objects that are referenced in compiled code
 *
 * These will be stored in a LISP vector bound to the variable Obarray
 * here at the C level.
 *
 * (def foo '(1 2 3)) beinds the object (1 2 3) to the variable
 * foo. After having a name we need not keep it in the object table
 * because the binding protects it from being GCd.
 *
 * Thus we mark all objects in the OBTAB (object table) that are still
 * referenced are marked as "used". We then recycle all objects that
 * are not marked as "used". Hence this is a mark and sweep technique.
 *
 * the collector marks slots in a byte vector bound to Obmap. Each
 * byte in Obmap relates to one slot, all the bytes are "free"
 * "allocated" or "used"
*/

/* Just use a struct ☹ */

/* Marked as FREE */
#define OBFREE  0
/* Marked as alloc */
#define OBALLOC 1
/* Marked as Used */
#define	OBUSED  2

/* Complications Mark procedure has to know enough about the abstract
 * machine to be able to locate quote instructions.
 *
 * Quote loads objects from object table into registers of the
 * abstract machine. They can appear anywhere!
 *
 * OBTAB     : 『Free』『0』  『Nil』『Free』 『1』『Free』 …
 *                       ^        ^              ^
 *                       |         \              \
 * ByteCode : 『…』『Quote』『…』『Quote』『…』『Quote』『』『』『』『』『』
 *
 * This abstract machine is defined carefully to deal with this
 * issue. If it's infeasible, the compiler could extract all literal
 * data objects from the program, store them in a list and generate a
 * single instruction at the start of each chunk. We could then
 * reference by offsets into the chunk.
 *
 * For this compiler we just go through the abstract machine programs
 * locate instructions then mark them in the OBTAB directly
 */

/* In the LISP9 compiler we will have abstract machine code witch has
 * instructions with size:
 * 0 operand : size 1 byte
 * 1 operand : size 3 bytes
 * 2 operand : size 5 bytes
 */

#define ISIZE0 1
#define ISIZE1 3
#define ISIZE2 5

/* We store big endian byte ordering
 *
 * fetcharg() macro is used to retrieve an argument at position i from
 * byte vector a.
 */
#define fetcharg(a, i) \
    (((a)[i] << 8) | (a)[(i) + 1])

/* Yet another Struct of array mapping with no struct */
cell Obarray, Obmap;

/* marklit function marks literal data objects referenced in program
 * chunk (byte vector) p. p is bound to a byte vector containing
 * abstract machine instructions.
 *
 * walk through the code, identify instructions with arguments, and
 * skip over their arguments.
 *
 * When it finds OP_QUOTE, it also fetches its arguments and marks the
 * corresponding OBTAB slot as used
 */

void marklit(cell p) {
    int i, k, op;
    byte *v, *m;

    k = stringlen(p);
    v = string(p);
    m = string(Obmap);
    for (i=0; i<k; ) {
        op = v[i];
        if (OP_QUOTE == op) {
            m[fetcharg(v, i+1)] = OBUSED;
            i += ISIZE1;
        }
        else if (OP_ARG == op || OP_PUSHVAL == op || OP_JMP == op ||
                 OP_BRF == op || OP_BRT == op || OP_CLOSURE == op ||
                 OP_MKENV == op || OP_ENTER == op || OP_ENTCOL == op ||
                 OP_SETARG == op || OP_SETREF == op || OP_MACRO == op)
            {
                i += ISIZE1;
            }
        else if (OP_REF == op || OP_CPARG == op || OP_CPREF == op) {
            i += ISIZE2;
        }
        else {
            i += ISIZE0;
        }
    }
}


int main() { return 0; }
