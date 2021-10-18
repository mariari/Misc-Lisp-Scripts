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
cell *Car = NULL,
     *Cdr = NULL;
byte *Tag = NULL;

/* The vector pool holds the value of all vectors objects such as
 * [symbols, strings, vectors]. They have a VECTOR_TAG instead of an
 * ATOM_TAG. VECTOR_TAGs have a type param in the CAR, CDR points to
 * the vector pool instead of the node pool. Vecotr pool is an array
 * of cells.
 */

cell *Vectors = NULL;

/* These variables keep track of free space */

/* List of unused Nodes */
cell Freelist = NIL;
/* Points to the unallocated part of vectors */
cell Freevec = 0;

#define ATOM_TAG	0x01	/* Atom, CAR = type, CDR = next */
#define MARK_TAG	0x02	/* Mark */
#define TRAV_TAG	0x04	/* Traversal */
#define VECTOR_TAG	0x08	/* Vector, CAR = type, CDR = content */
#define PORT_TAG	0x10	/* Atom is an I/O port (with ATOM_TAG) */
#define USED_TAG	0x20	/* Port: used flag */
#define LOCK_TAG	0x40	/* Port: locked (do not close) */
#define CONST_TAG	0x80	/* Node is immutable */

/* Accessing the fields of the the nodes */
#define tag(n) (Tag[n])
#define car(x) (Car[x])
#define cdr(x) (Cdr[x])

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
/* + 2 for link + size data in the vector
 * this math works
 */
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
    Vectors = malloc(size_pool);
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
 *
 * We do this janky lookup, because the size of instructions are 2
 * bytes, as seen in the ISIZE numbers, but since our array is of
 * bytes, we have to do this shifting to get the 16 bit value
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
    for (i = 0; i < k; ) {
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

/* 7.2 Node collection */

/* All objects in the heap have their roots in the node pool
 *
 * fixnums, chars, and ports : consist of two nodes that are linked in
 * the node pool
 *
 * String, Symbols, and vectors : are nodes that link to some sequence
 * of cells in the vector pool.
 *
 * List, Trees, and graphs : collections of nodes in the node pool.
 *
 * GC roots form a set of cells that reference root nodes in the node
 * pool. Root nodes must never be recycled by the node collector.
 *
 * Such Root Nodes Include:
 * Symbol Table
 * Object Table
 * Runtime Stack
 * Global Environment
 * Registers of the abstract machine
 *
 */

/* Task of the GC is to find nodes that are live (pointed to by a GC
 * root), mark them, and then "sweep" them by adding unmakred nodes to
 * the free list
 *
 * Hardest part is to mark the live graphs in the node pool. The Task
 * can somewhat easily be broken down into.
 *
 * 1. for each atom in the set of GC roots R, mark the atom and then
 * mark the cdr part of the atom.
 *
 * 2. for each cons cell in R, mark the cell itself and then mark the
 * car parts and the cdr parts of the cell.
 *
 * 3. for each vector in R, mark the vector node and then mark each
 * element of the vector in the vector pool (where each element points
 * back to the element pool).
 *
 * For each special object and each already-marked object in R, do
 * nothing.
 *
 * this may seem easy, but consider a list, if you have a list of size
 * 10k, you'd have to recurse 9999 times. Thus we use the
 * Deutsch-Schorr-Waitre (DSW) graph marking algorithm for traversing
 * GC roots.
 *
 * We shall thus mark graphs in constant space and only 1 cell of
 * additional storage.
 */

/* 7.2.1 the DSW graph marking algorithm */

/* problem solved by DSW as follows
 * https://youtu.be/2s2_FAf-yQs?t=1868 FROM SICP
 *
 * I think I understand the algorithm, we basically reserve a bit on
 * the node stating where the parent node is, so that we can traverse
 * back pointer wise. We note that the cdr is unvisited so when we go
 * back we can traverse the tree that way.... kind of a neat algorithm
 * actually.
 */

/*
 * Mark nodes which can be accessed through N.
 * Using modified Deutsch/Schorr/Waite pointer reversal algorithm.
 * S0: M==0, T==0, unvisited, process CAR (vectors: process 1st slot);
 * S1: M==1, T==1, CAR visited, process CDR (vectors: process next slot);
 * S2: M==1, T==0, completely visited, return to parent.
 */

void mark(cell n) {
    cell x, parent, *v;
    int i;

    parent = NIL;
    while (1) {
        if (specialp(n) || (tag(n) & MARK_TAG)) {
            if (NIL == parent)
                break;
            if (tag(parent) & VECTOR_TAG) { /* S1 --> S1|done */
                i = vecndx(parent);
                v = vector(parent);
                if (tag(parent) & TRAV_TAG && i+1 < veclen(parent)) { /* S1 --> S1 */
                    x = v[i+1];
                    v[i+1] = v[i];
                    v[i] = n;
                    n = x;
                    vecndx(parent) = i+1;
                }
                else {			/* S1 --> done */
                    x          = parent;
                    parent     = v[i];
                    v[i]       = n;
                    n          = x;
                    veclink(n) = n;
                }
            }
            else if (tag(parent) & TRAV_TAG) { /* S1 --> S2 */
                x = cdr(parent);
                cdr(parent) = car(parent);
                car(parent) = n;
                tag(parent) &= ~TRAV_TAG;
                n = x;
            }
            else {				/* S2 --> done */
                x      = parent;
                parent = cdr(x);
                cdr(x) = n;
                n      = x;
            }
        }
        else if (tag(n) & VECTOR_TAG) {		/* S0 --> S1 */
            tag(n) |= MARK_TAG;
            if (T_VECTOR == car(n) && veclen(n) != 0) {
                tag(n) |= TRAV_TAG;
                vecndx(n) = 0;
                v = vector(n);
                x = v[0];
                v[0] = parent;
                parent = n;
                n = x;
            }
            else {
                veclink(n) = n;
            }
        }
        else if (tag(n) & ATOM_TAG) {		/* S0 --> S2 */
            if (cdr(n) != NIL) {
                if (T_BYTECODE == car(n)) {
                    marklit(cdr(n));
                }
                else if (T_INPORT == car(n) ||
                         T_OUTPORT == car(n)
                         )
                    Port_flags[portno(n)] |= USED_TAG;
            }
            x = cdr(n);
            cdr(n) = parent;
            parent = n;
            n = x;
            tag(parent) |= MARK_TAG;
        }
        else {					/* S0 --> S1 */
            x = car(n);
            car(n) = parent;
            tag(n) |= MARK_TAG;
            parent = n;
            n = x;
            tag(parent) |= TRAV_TAG;
        }
    }
}

/* Time for the gc of the node pool!
 *
 * We set the following steps
 *
 * 1. All ports that have their LOCK_TAG bit set will be marked by
 * setting their USED_TAG bit, all others will be unmarked
 *
 * 2. All objects can be access through a member of the GC_roots array
 * will be marked.
 *
 * 3. All unmarked nodes are consed to Freelist and all other nodes
 * have their have their MARK_TAG bit cleared
 *
 * 4. All ports that don't have their USED_TAG bit set will be closed
 * and their Ports slots will be freed.
 *
 * 5. All OBTAB slots that are not marked OBUSED will be cleared
 */
int  GC_verbose = 0;
cell *GC_roots[];
cell Rts;
int  Sp;

int gc(void) {
    int	i, n, k, sk;
    char buf[100];
    cell *a;
    byte *m;

    /* Mark */
    for (i = 0; i < NPORTS; i++) {
        if (Port_flags[i] & LOCK_TAG)
            Port_flags[i] |= USED_TAG;
        else if (i == Inport || i == Outport)
            Port_flags[i] |= USED_TAG;
        else
            Port_flags[i] &= ~USED_TAG;
    }
    if (Rts != NIL) {
        sk = stringlen(Rts);
        stringlen(Rts) = (1 + Sp) * sizeof(cell);
    }
    /* Let us mark the GC roots and things inside */
    for (i = 0; GC_roots[i] != NULL; i++) {
        mark(*GC_roots[i]);
    }
    if (Rts != NIL) {
        stringlen(Rts) = sk;
    }
    /* Time to Sweep */
    k = 0;
    Freelist = NIL;
    for (i = 0; i < NNODES; i++) {
        if (!(tag(i) & MARK_TAG)) {
            /* Kind of clever we cons onto Freelist by doing this
             * trick, where we set the cdr to the free list then set
             * the tail of the list to the list to the current node.
             */
            cdr(i) = Freelist;
            Freelist = i;
            k++;
        }
        else {
            tag(i) &= ~MARK_TAG;
        }
    }
    for (i=0; i<NPORTS; i++) {
        if (!(Port_flags[i] & USED_TAG) && Ports[i] != NULL) {
            fclose(Ports[i]);
            Ports[i] = NULL;
        }
    }
    n = NIL == Obarray? 0: veclen(Obarray);
    a = NIL == Obarray? NULL: vector(Obarray);
    m = NIL == Obmap? NULL: string(Obmap);
    for (i = 0; i < n; i++) {
        if (OBUSED  == m[i]) {
            m[i] = OBALLOC;
        }
        else {
            m[i] = OBFREE;
            a[i] = NIL;
        }
    }
    if (GC_verbose) {
        sprintf(buf, "GC: %d nodes reclaimed", k);
        prints(buf); nl();
        flush();
    }
    return k;
}

cell Tmp_car = NIL,
     Tmp_cdr = NIL;

/* pcar and pcdr are protected from gc by having cells saved here As
 * it's stored in the GC_Roots itself. An interesting note is that at
 * the start of the system when all memory is free, we actually run gc
 * to setup Freelist with some values.
 */
cell cons3(cell pcar, cell pcdr, int ptag) {
    cell n;
    int  k;

    if (NIL == Freelist) {
        if (0 == (ptag & ~CONST_TAG))
            Tmp_car = pcar;
        if (!(ptag & VECTOR_TAG))
            Tmp_cdr = pcdr;
        k = gc();
        if (k < NNODES / 2) {
            /* memory low! */
        }
        Tmp_car = Tmp_cdr = NIL;
        if (NIL == Freelist)
            error("cons3: out of nodes", UNDEF);
    }
    n = Freelist;
    Freelist = cdr(Freelist);
    car(n) = pcar;
    cdr(n) = pcdr;
    tag(n) = ptag;
    return n;
}

/* Time to deal with fragmentation.  For this book we will use pool
 * compaction. During this process all live objects move to one end of
 * the pool. And then the free space pointer is reset to the end of
 * the live data. Free space becomes continuous free vector so
 * allocation is faster after this.
 *
 * Our pool makes heavy use of meta data of objects in the vector
 * pool.
 *
 * We will now outline the format of the vectors
 *
 *                  Vector Node
 *                --------------
 *                | Type |  ●  |
 *                --------------
 *                    ^      \
 *                   /        \
 *                  /          \
 *                 /            v
 * -----------------------------------------------------
 * | Data …   | Link   | Size | Data …   | Link  | …  |
 * |          | /Index |      |          | Index | …  |
 * -----------------------------------------------------
 *                      Vector Pool
 *
 * Link : points back to the node of the vector object
 * Index: Used by the node collector during graph marking
 * Size : holds the size of the vector in bytes
 * Data : which contains the value of the vector object
 */

/* You may have noticed that the Link and Index share a slot, this is
 * because the link field is only used during compaction and the index
 * is only used during marking.
 */

/* Here we have the offsets of the vectors
 * vector nodes refer to data so from it's perspective
 * data       = 0
 * size       = -1
 * link/index = -2
 *
 * The vector pool is really a vector of cells rather than bytes.
 * Hence:
 * A 3 byte vector would allocate 1 4 byte cell
 * A 5 byte vector would allocate 2 4 byte cells
 */
#define RAW_VECLINK 0
#define RAW_VECSIZE 1
#define RAW_VECDATA 2

/* unmark_vecs unmarks all vectors by breaking their link field setting
 * it to NIL. Mark phase will later restore this making them live
 * vector objects.
 */
void unmark_vecs(void) {
    int	p, k, link;

    p = 0;
    while (p < Freevec) {
        link = p;
        k = Vectors[p + RAW_VECSIZE];
        p += vecsize(k);
        Vectors[link] = NIL;
    }
}

/* gcv function compacts the vector pool by removing the values of all
 * dead objects from the pool. It collects all the vectors at the
 * beginning of the pool.
 *
 * Since the data moves, nodes will have to update so it points to the
 * new address.
 *
 * After compacting hte pool, Freevec will point at the cell right
 * after collected data.
 */

int gcv(void) {
    int	v, k, to, from;
    char buf[100];

    unmark_vecs();
    /* re-mark live vectors, via mark in gc */
    gc();
    to = from = 0;
    while (from < Freevec) {
        v = Vectors[from + RAW_VECSIZE];
        k = vecsize(v);
        if (Vectors[from + RAW_VECLINK] != NIL) {
            if (to != from) {
                memmove( &Vectors[to]
                       , &Vectors[from]
                       , k * sizeof(cell) );
                cdr(Vectors[to + RAW_VECLINK]) =
                    to + RAW_VECDATA;
            }
            to += k;
        }
        from += k;
    }

    k = Freevec - to;

    if (GC_verbose) {
        sprintf(buf, "GCV: %d cells reclaimed", k);
        prints(buf);
        nl();
        flush();
    }
    Freevec = to;
    return k;
}

/* 7.5 Vector allocation */

/**
 * newvec function allocates a vector of the given type and size.
 * Where type is the type tag to be placed in the car field of the
 * resulting vector node, and size is the desired size in bytes.
 */

cell newvec(cell type, int size) {
    cell n;
    int v, wsize;
    wsize = vecsize(size);
    /* check for available memory */
    if (Freevec + wsize >= NVCELLS) {
        gcv();
        if (Freevec + wsize >= NVCELLS)
            error("newvec: out of vector space", UNDEF);
    }
    v        = Freevec;
    Freevec += wsize;
    n        = cons3(type, v + RAW_VECDATA, VECTOR_TAG);
    Vectors[v + RAW_VECLINK] = n;
    Vectors[v + RAW_VECSIZE] = size;
    return n;
}

/* 7.6 GC Protection */

/* When allocating complex structure of multiple nodes we often need to
 * protect a node while allocating different parts of hte structure
 *
 * (cons (cons a b) (cons c d))
 *
 * In the above we need to protect (cons a b) while (cons c d)
 * evaluates.
 *
 * We can do this in a few ways:
 *
 * 1. By binding object to the GC root Tmp
 * 2. Pushing it to the Protected stack
 *
 * Both of these would do that
 *
 * Tmp = (cons a b);
 * n   = (cons Tmp (cons c d));
 * Tmp = Nil;
 *
 * Or
 *
 * protect(n = (cons a b));
 * n = (cons n (cons c d));
 * uprot(1);
 *
 * the first technique should be used when it's a simple object, while
 * the later while complex objects are being allocated
 */

cell Protected = NIL;
cell Tmp = NIL;

#define protect(n) (Protected = cons((n), Protected))

cell unprot(int k) {
    cell n = NIL;               /* LINT */

    while (k) {
        if (NIL == Protected)
            error("unprot: stack underflow", UNDEF);
        n = car(Protected);
        Protected = cdr(Protected);
        k--;
    }
    return n;
}

/* Chapter 8 - High Level Data Types */

/* Here we will define the more abstract types, like
 * fixnums
 * Characters
 * Strings
 * Vectors
 * I/O ports
 * Symbols
 *
 * And the Hash Table that isn't available in the LISP level.
 */

/* The following layout is used for
 * -----------     ---------------
 * | Tag | --|---->| Value | Nil |
 * -----------     ---------------
 *
 * Fixnums
 * Chars
 * I/O ports
 */


/**
 * mkfix macro creates a fixnum object and stores the value of a C int
 * in it's value field.
 */
#define mkfix(n) mkatom(T_FIXNUM, mkatom((n), NIL))

/**
 * fixval extracts that value
 */
#define fixval(n) (cadr(n))

/**
 * add_ovfl and sub_ovfl return 1 if the value would return 1
 */

#define add_ovfl(a,b) \
    ((((b) > 0) && ((a) > INT_MAX - (b))) || \
     (((b) < 0) && ((a) < INT_MIN - (b))))

#define sub_ovfl(a,b) \
    (   (((b) < 0) && ((a) > INT_MAX + (b))) \
     || (((b) > 0) && ((a) < INT_MIN + (b))))

#define mkchar(c) mkatom(T_CHAR, mkatom((c) & 0xff, NIL))

#define charval(n) (cadr(n))

cell Nullstr = NIL;

cell mkstr(char *s, int k) {
    cell n;

    if (0 == k)
        return Nullstr;

    n = newvec(T_STRING, k+1);

    if (NULL == s) {
        memset(string(n), 0, k+1);
    }
    else {
        memcpy(string(n), s, k);
        string(n)[k] = 0;
    }
    return n;
}

/*             Vector Data looks like
 *
 *                  Vector Node
 *                --------------
 *                | Tag |  ●  |
 *                --------------
 *                   ^      \
 *                  /        \
 *                 /          \
 *                /            v
 * -----------------------------------------------------
 * | Data …   | Link   | Size | Data …   | Link  | …  |
 * |          | /Index |      |          | Index | …  |
 * -----------------------------------------------------
 *                 Vector Pool
 */

/**
 * mkvec creates a vector object of type vector The object returned
 * will have k slots. When k = 0, Nullvec will be allocated.
 */

cell Nullvec = NIL;

cell mkvec(int k) {
    cell n, *v;
    int i;

    if (0 == k)
        return Nullvec;
    n = newvec(T_VECTOR, k * sizeof(cell));
    v = vector(n);
    for (i = 0; i < k; i++)
        v[i] = NIL;
    return n;
}

/* type is either T_INPORT or T_OUTPORT
 *
 * portno is a port handle as delivered by the newport function.
 */

cell mkport(int portno, cell type) {
    cell n;
    int pf;

    pf = Port_flags[portno];
    Port_flags[portno] |= LOCK_TAG;
    n = mkatom(portno, NIL);
    n = cons3(type, n, ATOM_TAG|PORT_TAG);
    Port_flags[portno] = pf;
    return n;
}

/* Time to implement the hash table
 *
 * Weak hash function, so use prime numbers for the vector array
 */
int htsize(int n) {
    if (n < 47)    return 47;
    if (n < 97)    return 97;
    if (n < 199)   return 199;
    if (n < 499)   return 499;
    if (n < 997)   return 997;
    if (n < 9973)  return 9973;
    if (n < 19997) return 19997;
    return 39989;
}
/* The hashtable is a pair with a fixnum as it's car field and a vector
 * in it's cdr. The fixnum (count) equals the number of associations
 * currently stored in the hash table.
 *
 * each vector slot contains an assoc list of the form
 * ((key₁ . value₁) … (keyₙ . valueₙ))
 *
 * Each key value is an assocaiton. Any empty list is euqal to NIL.
 *
 * I believe we store a list of associations in this array (so an array of lists)
 *
 * we fit this hash table in the vector of the lisp
 *
 * I will not comment on the rest as the algorithms are already
 * somewhat known/obvious
 */

cell mkht(int k) {
    cell n;

    n = mkfix(0); /* mutable, can't use Zero */
    protect(n);
    n = cons(n, mkvec(htsize(k)));
    unprot(1);
    return n;
}

#define htlen(d)   veclen(cdr(d))
#define htelts(d)  fixval(car(d))
#define htdata(d)  cdr(d)
#define htslots(d) vector(cdr(d))

uint hash(byte *s, uint k) {
    uint h = 0xabcd;

    while (*s)
        h = ((h << 5) + h) ^ *s++;
    return h % k;
}

uint obhash(cell x, uint k) {
    if (specialp(x))
        return abs(x) % k;
    if (symbolp(x))
        return hash(symname(x), k);
    if (fixp(x))
        return abs(fixval(x)) % k;
    if (charp(x))
        return charval(x) % k;
    if (stringp(x))
        return hash(string(x), k);
    return 0;
}

int match(cell a, cell b) {
    int k;

    if (a == b) {
        return 1;
    }
    if (fixp(a) && fixp(b)) {
        return fixval(a) == fixval(b);
    }
    if (charp(a) && charp(b)) {
        return charval(a) == charval(b);
    }
    if (symbolp(a) && symbolp(b)) {
        k = symlen(a);
        if (symlen(b) != k) return 0;
        return memcmp(symname(a), symname(b), k) == 0;
    }
    if (stringp(a) && stringp(b)) {
        k = stringlen(a);
        if (stringlen(b) != k) return 0;
        return memcmp(string(a), string(b), k) == 0;
    }
    return 0;
}

void htgrow(cell d) {
    int  nk, i, h, k;
    cell nd, e, n;

    k = htlen(d);
    nk = 1 + htlen(d);
    nd = mkht(nk);
    protect(nd);
    nk = htlen(nd);
    for (i = 0; i < k; i++) {
        for (e = htslots(d)[i]; e != NIL; e = cdr(e)) {
            h = obhash(caar(e), nk);
            n = cons(car(e), htslots(nd)[h]);
            htslots(nd)[h] = n;
        }
    }
    htdata(d) = htdata(nd);
    unprot(1);
}

int htlookup(cell d, cell k) {
    cell x;
    int  h;

    h = obhash(k, htlen(d));
    x = htslots(d)[h];
    while (x != NIL) {
        if (match(caar(x), k)) return car(x);
        x = cdr(x);
    }
    return UNDEF;
}

void htadd(cell d, cell k, cell v) {
    cell e;
    int  h;

    Tmp = k;
    protect(v);
    protect(k);
    Tmp = NIL;
    if (htelts(d) >= htlen(d))
        htgrow(d);
    h = obhash(k, htlen(d));
    e = cons(k, v);
    e = cons(e, htslots(d)[h]);
    htslots(d)[h] = e;
    htelts(d)++;
    unprot(2);
}

cell htrem(cell d, cell k) {
    cell *x, *v;
    int h;

    h = obhash(k, htlen(d));
    v = htslots(d);
    x = &v[h];
    while (*x != NIL) {
        if (match(caar(*x), k)) {
            *x = cdr(*x);
            htelts(d)--;
            break;
        }
        x = &cdr(*x);
    }
    return d;
}

/* The symbol table (SYMTAB) of a LSIP system is a collection of symbol
 * atoms known to the system. The primary function of the symbol table
 * is to provide symbol identity. So all things read by the reader
 * gives us the same symbol
 *
 * It looks a little something like
 *    Symhash             Symbols
 * ------------           ------
 * | (foo . 5) |\       0 |  …  |
 * |     …     | \      1 |  …  |
 * | (bar . 2) | —————→ 2 | bar |
 * |     …     |   \  → 3 | baz |
 * | (baz . 3) | ————/  4 |  …  |
 * |     …     |    \—→ 5 | foo |
 * ------------           ------
 *
 * Also useful for error reporting as symbols themselves will be lost
 * during compilation but symbol table slot numbers are carried
 * through.
 */

cell Symhash = NIL;
cell Symbols = NIL;

/** Symptr points to the next free slot in the Symbols vector */
cell Symptr = 0;

/**
 * mksym returns a new symbol, but does not intern it! Thus the symbol
 * is not unique nor unambiguous. Only gensym creates uninterned
 * symbols
 */
cell mksym(char *s, int k) {
    cell n;
    n = newvec(T_SYMBOL, k + 1);
    strcpy((char *) symname(n), s);
    return n;
}


/**
 * findsym function looks up the symbol name s in the symbols table.
 * If such a symbol exists it returns the unique atom representing the
 * atom. Otherwise NIL is returned
 */
cell findsym(char *s) {
    cell y;
    y = mksym(s, strlen(s));
    y = htlookup(Symhash, y);
    if (y != UNDEF)
        return car (y);
    return NIL;
}

/**
 * intern, interns the symbol y by adding it both to the hash table
 * Symhash and the vector Symbols. It returns the interned symbol.
 * When the next free slot (Symptr) is outside of Symbols vector, the
 * function will extend the vector by CHUNKSIZE elements. Elements are
 * copied as the vector pool can't grow.
 */

cell intern(cell y) {
    cell n, *vn, *vs;
    int  i, k;

    protect(y);
    htadd(Symhash, y, mkfix(Symptr));
    unprot(1);
    k = veclen(Symbols);
    if (Symptr >= k) {
        n  = mkvec(k + CHUNKSIZE);
        vs = vector(Symbols);
        vn = vector(n);
        for (i = 0; i < k; i++)
            vn[i] = vs[i];
        Symbols = n;
    }
    vector(Symbols)[Symptr] = y;
    Symptr++;
    return y;
}

/**
 * symref function returns a reference to an interned symbol with the
 * name s. The symbol may not exist and may or may not be interened
 * when symref is called
 */
cell symref(char *s) {
    cell y, new;

    y = findsym(s);
    if (y != NIL)
        return y;
    new = mksym(s, strlen(s));
    return intern(new);
}

/* 9. Some Useful List Functions */

/* (do ((n n (cdr n))
 *      (m m (cons (car n) m)))
 *     ((null n) m))
 * the following is the translation of this recursive algorithm
 */

/**
 * reconc reverse its first argument n, and concatenates it to the
 * second argument m.
 */
cell reconc(cell n, cell m) {
    while (n != NIL) {
        if (atomp(n))
            error("reconc: dotted list", n);
        m = cons(car(n), m);
        n = cdr(n);
    }
    return m;
}

#define reverse(n) reconc((n), NIL)

/**
 * nreconc is the destructive version of reconc that mutates the first
 * list n.
 */
cell nreconc(cell n, cell m) {
    cell h;

    while (n != NIL) {
        if (atomp(n))
            error("nreconc: dotted list", n);
        h = cdr(n);
        cdr(n) = m;
        m = n;
        n = h;
    }
    return m;
}

#define nreverse(n) nreconc((n), NIL)


/* Why don't we define it as (reconc (reverse a) b) */
cell conc(cell a, cell b) {
    cell n;

    a = reverse(a);
    protect(a);
    /* At htis point shouldn't we just call reconc */
    n = b;
    while (a != NIL) {
        n = cons(car(a), n);
        a = cdr(a);
    }
    unprot(1);
    return n;
}

cell nconc(cell a, cell b) {
    cell n;

    n = a;
    if (NIL == a)
        return b;
    while (cdr(a) != NIL)
        a = cdr(a);
    cdr(a) = b;
    return n;
}

/* 10. High_level Port I/O */

/**
 * newport allocates an unused port number and returns it. When no
 * ports are free it gcs. If it fails it returns -1 indicating
 * failure.
 */
int newport(void) {
    int i, n;
    for (n = 0; n < 2; n++) {
        for (i = 0; i < NPORTS; i++) {
            if (NULL == Ports[i])
                return i;
        }
        if (0 == n)
            gc();
    }
    return -1;
}

/**
 * open_inport opens the file path for input, associates it with a
 * port number and returns the port number. If the port can't be
 * opened or the file, then this returns -1.
 */
int open_inport(char *path) {
    int i;

    i = newport();
    if (i < 0)
        return -1;
    Ports[i] = fopen(path, "r");
    if (NULL == Ports[i])
        return -1;
    return i;
}



/**
 * open_output is similar to open_inport but opens the file for
 * output. If the file already exists it will be truncated to zero
 * length, if the append value is non zero then the file is appended
 * to rather than truncated
 */

int open_outport(char *path, int append) {
    int i;

    i = newport();
    if (i < 0)
        return -1;
    Ports[i] = fopen(path, append? "a": "w");
    if (NULL == Ports[i])
        return -1;
    return i;
}

cell set_inport(cell port) {
    cell p = Inport;

    Inport = port;
    return p;
}

int set_outport(int port) {
    int p = Outport;

    Outport = port;
    return p;
}

void close_port(int port) {
    if (port < 0 || port >= NPORTS)
        return;
    if (NULL == Ports[port]) {
        Port_flags[port] = 0;
        return;
    }
    fclose(Ports[port]);
    Ports[port] = NULL;
    Port_flags[port] = 0;
}

void reset_stdports(void) {
    clearerr(stdin);
    clearerr(stdout);
    clearerr(stderr);
    Inport = 0;
    Outport = 1;
    Errport = 2;
}

int lock_port(int port) {
    if (port < 0 || port >= NPORTS)
        return -1;
    Port_flags[port] |= LOCK_TAG;
    return 0;
}

int unlock_port(int port) {
    if (port < 0 || port >= NPORTS)
        return -1;
    Port_flags[port] &= ~LOCK_TAG;
    return 0;
}

/* 11. The Global Environment */

/* The LISP9 System has two global environments that bind symbolic
 * names to locations. One is the deep binding environment, which new
 * bindings will be made and C functions will lookup values.
 *
 * The other is a shallow-binding environment which is used by
 * compiled programs to lookup and modify values of variables
 *
 * Deep binding environments are an assoc-list that associates symbols
 * with boxes. We get O(n) lookup but this is fine as it's in non
 * critical spots.
 *
 * The run time environment, which is the shallow-binding with
 * constant time access will be introduced later this chapter.
 */

cell Glob = NIL;

/**
 * bindnew creates a new variable by binding symbol v to a box
 * containing the value a. It adds the new value in the head of Glob.
 */

void bindnew(cell v, cell a) {
    cell n;
    n = cons(a, NIL);
    n = cons(v, n);
    Glob = cons(n, Glob);
}

/**
 * assq is almost equal to the scheme variant of the same name. It
 * looks up an association with the key x in the assoc list a and
 * returns it. If no such assoc exists it returns NIL. assq assumes
 * that a is a proper association list.
 */
int assq(int x, cell a) {
    for (; a != NIL; a = cdr(a))
        if (caar(a) == x)
            return car(a);
    return NIL;
}

/** bindset stores the value a in the box associated with the symbol v
 * in the deep-binding global environment. When no matching exists the
 * function does nothing.
 */
void bindset(cell v, cell a) {
    cell b = assq(v,Glob);
    if (b != NIL)
        cadr(b) = a;
}

/* 12. The Reader */

/* S_apply  is bound to the symbol apply */

/* This will be a normal recursive descent parser */

/* Predefined symbols */
cell S_apply, S_def, S_defmac, S_defun, S_errtag,
    S_errval, S_if, S_ifstar, S_imagefile, S_labels, S_lambda,
    S_macro, S_prog, S_quiet, S_quote, S_qquote, S_starstar,
    S_splice, S_setq, S_start, S_unquote;


/* These are the primitive functions in that they are predefined in C */
cell P_abs, P_alphac, P_atom, P_bitop, P_caar, P_cadr, P_car,
    P_catchstar, P_cdar, P_cddr, P_cdr, P_cequal, P_cgrtr, P_cgteq,
    P_char, P_charp, P_charval, P_cless, P_close_port, P_clteq,
    P_cmdline, P_conc, P_cons, P_constp, P_ctagp, P_delete, P_div,
    P_downcase, P_dump_image, P_eofp, P_eq, P_equal, P_gc, P_error,
    P_errport, P_eval, P_existsp, P_fixp, P_flush, P_format, P_funp,
    P_gensym, P_grtr, P_gteq, P_inport, P_inportp, P_less,
    P_liststr, P_listvec, P_load, P_lowerc, P_lteq, P_max, P_min,
    P_minus, P_mkstr, P_mkvec, P_mx, P_mx1, P_nconc, P_nreconc,
    P_not, P_null, P_numeric, P_numstr, P_obtab, P_open_infile,
    P_open_outfile, P_outport, P_outportp, P_pair, P_peekc, P_plus,
    P_prin, P_princ, P_quit, P_read, P_readc, P_reconc, P_rem,
    P_rename, P_sconc, P_sequal, P_set_inport, P_set_outport,
    P_setcar, P_setcdr, P_sfill, P_sgrtr, P_sgteq, P_siequal,
    P_sigrtr, P_sigteq, P_siless, P_silteq, P_sless, P_slteq,
    P_sref, P_sset, P_ssize, P_stringp, P_strlist, P_strnum,
    P_substr, P_subvec, P_symbol, P_symbolp, P_symname, P_symtab,
    P_syscmd, P_throwstar, P_times, P_untag, P_upcase, P_upperc,
    P_veclist, P_vconc, P_vectorp, P_vfill, P_vref, P_vset, P_vsize,
    P_whitec, P_writec;

/* Used to denote canceling input */
volatile int Intr;

int Inlist = 0;
int Quoting = 0;

#define octalp(c) \
    ('0' == (c) || '1' == (c) || '2' == (c) || '3' == (c) ||    \
     '4' == (c) || '5' == (c) || '6' == (c) || '7' == (c))

int octchar(char *s) {
    int	v = 0;

    if (!octalp(*s))
        return -1;
    while (octalp(*s)) {
        v = 8*v + *s - '0';
        s++;
    }
    return (*s || v > 255)? -1: v;
}

#define symbolic(c) \
    (isalpha(c) || isdigit(c) || (c && strchr("!$%^&*-/_+=~.?<>:", c)))

#define LP '('
#define RP ')'

int strcmp_ci(char *s1, char *s2) {
    int c1, c2;

    while (1) {
        c1 = tolower((int) *s1++);
        c2 = tolower((int) *s2++);
        if (!c1 || !c2 || c1 != c2)
            break;
    }
    return c1-c2;
}

char *Readerr = NULL;

void rderror(char *s, cell x) {
    if (NULL == Instr)
        error(s, x);
    Readerr = s;
}

cell rdchar(void) {
    char name[TOKLEN+1];
    int i, c, v;

    c = readc();
    name[0] = c;
    c = readc();
    for (i=1; i<TOKLEN; i++) {
        if (Intr || Readerr)
            return NIL;
        if (!isalpha(c) && !isdigit(c))
            break;
        name[i] = c;
        c = readc();
    }
    name[i] = 0;
    rejectc(c);
    if (TOKLEN == i)
        rderror("char name too long",
                mkstr(name, strlen(name)));
    if (!strcmp_ci(name, "ht")) return mkchar(9);
    if (!strcmp_ci(name, "nl")) return mkchar(10);
    if (!strcmp_ci(name, "sp")) return mkchar(' ');
    v = octchar(&name[1]);
    if ('\\' == *name && v >= 0) return mkchar(v);
    if (i != 1) rderror("bad character name",
                       mkstr(name, strlen(name)));
    return mkchar(name[0]);
}

cell xread2(void);

cell rdlist(void) {
    cell n, a, p;
    cell new;
    static char badpair[] = "malformed pair";

    Inlist++;
    n = xread2();
    if (RPAREN == n) {
        Inlist--;
        return NIL;
    }
    p = NIL;
    a = cons3(n, NIL, CONST_TAG);
    protect(a);
    while (n != RPAREN) {
        if (Intr || Readerr) {
            unprot(1);
            return NIL;
        }
        if (EOFMARK == n)  {
            unprot(1);
            rderror("missing ')'", UNDEF);
            return NIL;
        }
        else if (DOT == n) {
            if (NIL == p) {
                unprot(1);
                rderror(badpair, UNDEF);
                return NIL;
            }
            n = xread2();
            cdr(p) = n;
            if (RPAREN == n || xread2() != RPAREN) {
                unprot(1);
                rderror(badpair, UNDEF);
                return NIL;
            }
            Inlist--;
            return unprot(1);
        }
        car(a) = n;
        p = a;
        n = xread2();
        if (n != RPAREN) {
            Tmp = n;
            new = cons3(NIL, NIL, CONST_TAG);
            Tmp = NIL;
            cdr(a) = new;
            a = cdr(a);
        }
    }
    Inlist--;
    return unprot(1);
}

cell listvec(cell x, int veclit);

cell rdvec(void) {
    return listvec(rdlist(), 1);
}

int pos(int p, char *s) {
    int i;

    i = 0;
    for (; *s; s++) {
        if (p == *s)
            return i;
        i++;
    }
    return -1;
}

cell scanfix(char *s, int r, int of) {
    int v, g, i;
    char *p;
    char d[] = "0123456789abcdefghijklmnopqrstuvwxyz";

    g = 1;
    p = s;
    if ('+' == *p) {
        p++;
    }
    else if ('-' == *p) {
        p++;
        g = -1;
    }
    v = 0;
    while (*p) {
        i = pos(tolower(*p), d);
        if (i < 0 || i >= r) return NIL;
        if (	v > INT_MAX/r ||
                (v > 0 && add_ovfl(v*r, i)) ||
                (v < 0 && sub_ovfl(v*r, i)))
            {
                if (!of) return NIL;
                rderror("fixnum too big", mkstr(s, strlen(s)));
            }
        else if (v < 0)
            v = v*r - i;
        else
            v = v*r + i;
        p++;
        if (g) v *= g;
        g = 0;
    }
    if (g) return NIL;
    return mkfix(v);
}

cell rdsymfix(int c, int r, int sym) {
    char name[TOKLEN+1];
    int i;
    cell n;

    for (i=0; i<TOKLEN; i++) {
        if (!symbolic(c))
            break;
        name[i] = tolower(c);
        c = readc();
    }
    name[i] = 0;
    rejectc(c);
    if (TOKLEN == i) rderror("symbol or fixnum too long",
                            mkstr(name, strlen(name)));
    n = scanfix(name, r, 1);
    if (n != NIL) return n;
    if (!sym) rderror("invalid digits after #radixR",
                      mkstr(name, strlen(name)));
    if ('t' == name[0] && 0 == name[1])
        return TRUE;
    if (!strcmp(name, "nil"))
        return NIL;
    return symref(name);
}

cell rdfix(int c) {
    int r;

    r = 0;
    while (isdigit(c)) {
        r = r*10 + c - '0';
        c = readc();
    }
    if (c != 'r') rderror("'R' expected after #radix", UNDEF);
    if (r < 2 || r > 36) rderror("bad radix in #radixR", mkfix(r));
    c = readc();
    return rdsymfix(c, r, 0);
}

cell rdstr(void) {
    char name[TOKLEN+1];
    int i, j, c, u, v;
    cell n;

    c = readc();
    u = 0;
    for (i=0; i<TOKLEN; i++) {
        if (Intr || Readerr) return NIL;
        if ('"' == c) break;
        if ('\n' == c) Line++;
        if (EOF == c) rderror("EOF in string", UNDEF);
        if ('\\' == c) {
            c = readc();
            if ('\\' == c || '"' == c) {
                /**/
            }
            else if ('t' == c) {
                c = '\t';
            }
            else if ('n' == c) {
                c = '\n';
            }
            else if (octalp(c)) {
                v = 0;
                j = 0;
                while (j < 3 && octalp(c)) {
                    v = v * 8 + c-'0';
                    c = readc();
                    j++;
                }
                rejectc(c);
                if (v > 255) rderror("invalid char", mkfix(v));
                c = v;
            }
            else if (0 == u) {
                u = c;
            }
        }
        name[i] = c;
        c = readc();
    }
    name[i] = 0;
    if (u) rderror("unknown slash sequence", mkchar(u));
    if (i >= TOKLEN) rderror("string too long", mkstr(name, i));
    if (u) return NIL;
    n = mkstr(name, i);
    tag(n) |= CONST_TAG;
    return n;
}

cell rdquote(cell q) {
    cell n;

    Quoting++;
    n = xread2();
    Quoting--;
    return cons(q, cons(n, NIL));
}

cell meta(void) {
    int c, cmd, i;
    cell n, cmdsym;
    char s[128];

    cmd = tolower(readc());
    c = readc();
    while (' ' == c) c = readc();
    i = 0;
    while (c != '\n' && c != EOF) {
        if (i < sizeof(s) - 6)
            s[i++] = c;
        c = readc();
    }
    rejectc(c);
    s[i] = 0;
    if ('l' == cmd) strcat(s, ".ls9");
    n = mkstr(s, strlen(s));
    n = 0 == i? NIL: cons(n, NIL);
    protect(n);
    switch (cmd) {
    case 'c':	cmdsym = symref("syscmd"); break;
    case 'h':	cmdsym = symref("help"); break;
    case 'l':	cmdsym = P_load; break;
    default: 	prints(",c = syscmd"); nl();
        prints(",h = help"); nl();
        prints(",l = load"); nl();
        return NIL;
    }
    unprot(1);
    return cons(cmdsym, n);
}

/* Comment is missing, but interesting to have comments as expression. */

cell xread2(void) {
    int	c;

    c = readc();
    while (1) {
        while (' ' == c || '\t' == c || '\n' == c || '\r' == c) {
            if (Intr || Readerr) return NIL;
            if ('\n' == c) Line++;
            c = readc();
        }
        if (c != ';') break;
        while (c != '\n' && c != EOF)
            c = readc();
    }
    if (Intr || Readerr) return NIL;
    if (EOF == c) {
        return EOFMARK;
    }
    else if ('#' == c) {
        c = readc();
        if ('\\' == c) return rdchar();
        else if (LP == c) return rdvec();
        else if (isdigit(c)) return rdfix(c);
        else rderror("bad # syntax", mkchar(c));
    }
    else if ('"' == c) {
        return rdstr();
    }
    else if (LP == c) {
        return rdlist();
    }
    else if (RP == c) {
        if (!Inlist) rderror("unexpected ')'", UNDEF);
        return RPAREN;
    }
    else if ('\'' == c) {
        return rdquote(S_quote);
    }
    else if ('`' == c || '@' == c) {
        return rdquote(S_qquote);
    }
    else if (',' == c) {
        if (!Inlist && !Quoting) return meta();
        c = readc();
        if ('@' == c) return rdquote(S_splice);
        rejectc(c);
        return rdquote(S_unquote);
    }
    else if ('.' == c) {
        if (!Inlist) rderror("unexpected '.'", UNDEF);
        return DOT;
    }
    else if (symbolic(c)) {
        return rdsymfix(c, 10, 1);
    }
    else {
        rderror("funny input character, code", mkfix(c));
    }
    return NIL;
}

cell xread(void) {
    cell x;

    Inlist = 0;
    Quoting = 0;
    Readerr = NULL;
    x = xread2();
    if (Intr) error("aborted", UNDEF);
    return x;
}



int main() { return 0; }
