#ifndef _GNU_SOURCE
#define _GNU_SOURCE /* mremap */
#endif

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/mman.h>
#include <signal.h>
#include <ucontext.h>
#include <errno.h>
#include <string.h>
#include <fcntl.h>
#include <wchar.h>
#include <locale.h>

#define stack_size 100000
#define heap_size  1000000

#ifdef __APPLE__
#define SCHEME_ENTRY scheme_entry
#endif
#ifdef __linux__
#define SCHEME_ENTRY _scheme_entry
#endif

#define SCHEME_EXIT _scheme_exit
#define SCHEME_SYMBOL_TO_ADDRESS _scheme_symbol_to_address

extern long SCHEME_ENTRY(char *, char *, char *);
extern void SCHEME_EXIT(void);
extern long *SCHEME_SYMBOL_TO_ADDRESS(long);

/* locally defined functions */
static char *guarded_area(long n);
#ifdef __APPLE__
static void segv_handler(int signo, siginfo_t *info, void *ignore);
#endif
#ifdef __linux__
static void segv_handler(int signo, struct sigcontext sc);
#endif
static void bus_handler(int signo);
static void usage_error(char *who);
static void print(long x);

/* local stack/heap management variables */
static long pagesize;
static long original_heapsize;

static char *stack;
static long stacksize;

static char *heap;
static long heapsize;
static int heap_index;
static char *two_heap[2];

/* long gc_time; */

int main(int argc, char *argv[]) {
  struct sigaction action;
  sigset_t s_set;
  int n;

  if (setlocale(LC_CTYPE, "") == NULL) {
    fprintf(stderr, "setlocale failed: %s\n", strerror(errno));
    exit(4);
  }

  pagesize = sysconf(_SC_PAGESIZE);

  stacksize = stack_size * sizeof(void *);
  heapsize = heap_size * sizeof(void *);

  for (n = 1; n < argc; n++)
    if ((*argv[n] == '-') && (*(argv[n]+2) == 0))
      switch (*(argv[n]+1)) {
        case 'h': /* heap size option */
          argv[n] = (char *)NULL;
          if (++n == argc) usage_error(argv[0]);
          heapsize = atoi(argv[n]);
          if (heapsize <= 0) usage_error(argv[0]);
          break;
        case 's': /* stack size option */
          argv[n] = (char *)NULL;
          if (++n == argc) usage_error(argv[0]);
          stacksize = atoi(argv[n]);
          if (stacksize <= 0) usage_error(argv[0]);
          break;
        default:
          usage_error(argv[0]);
      }
    else
      usage_error(argv[0]);
   
 /* round stack and heap sizes to even pages */
  stacksize = ((stacksize + pagesize - 1) / pagesize) * pagesize;
  heapsize = ((heapsize + pagesize - 1) / pagesize) * pagesize;
  original_heapsize = heapsize;

  heap_index = 0;
  stack = guarded_area(stacksize);
  two_heap[0] = guarded_area(heapsize);
  two_heap[1] = guarded_area(heapsize);
  heap = two_heap[0];

 /* Set up segmentation fault signal handler to catch stack and heap
  * overflow and some memory faults */
  sigemptyset(&s_set);
#ifdef __linux__
  action.sa_handler = (void *)segv_handler;
  action.sa_flags = SA_RESETHAND;
#else
  action.sa_sigaction = segv_handler;
  action.sa_flags = SA_SIGINFO | SA_RESETHAND;
#endif
  action.sa_mask = s_set;
  if (sigaction(SIGSEGV, &action, NULL)) {
    fprintf(stderr, "sigaction failed: %s\n", strerror(errno));
    fprintf(stderr, "  overflow checking may not work\n");
  }

 /* Set up bus error signal handler to catch remaining memory faults */
  sigemptyset(&s_set);
  action.sa_handler = bus_handler;
  action.sa_mask = s_set;
  action.sa_flags = SA_RESETHAND;
  if (sigaction(SIGBUS, &action, NULL)) {
      fprintf(stderr, "sigaction failed: %s\n", strerror(errno));
  }

 /* run the Scheme program and print the result */
  print(SCHEME_ENTRY(stack, heap, heap + heapsize));
  wprintf(L"\n");

  /* fprintf(stderr, ";; GC time = %f s", (double)gc_time / 1e9); */

  return 0;
}

/* allocate a chunk of memory with a guard page on either end */
static char *guarded_area(long n) {  /* n must be page aligned */
  char *addr;

 /* allocate, leaving room for guard pages */
  addr = (char *)mmap(NULL,
                      (size_t)(n + 2 * pagesize),
                      PROT_READ | PROT_WRITE,
                      MAP_PRIVATE | MAP_ANON,
                      -1, 0);
  if (addr == (char *)-1) {
    fprintf(stderr, "mmap failed: %s\n", strerror(errno));
    exit(2);
  }

 /* remove access rights from the guard pages */
  if (mprotect(addr, (size_t)pagesize, PROT_NONE) ||
    mprotect(addr + pagesize + n, (size_t)pagesize, PROT_NONE)) {
    fprintf(stderr, "mprotect failed: %s\n", strerror(errno));
    exit(3);
  }
   
  return addr + pagesize;
}

/* Signal handler that traps SIGSEGV and checks if the violation
 * might have been caused by stack or heap overflow */
#ifdef __APPLE__
static void segv_handler(int signo, siginfo_t *info, void *ingore) {
#endif
#ifdef __linux__
static void segv_handler(int signo, struct sigcontext sc) {
#endif
  char *addr;

#ifdef __APPLE__
  addr = (char *)info->si_addr;
#endif
#ifdef __linux__
  addr = (char *)(sc.cr2);
#endif

  if (heap-pagesize <= addr && addr < heap) {
    fprintf(stderr,"invalid access just below the heap\n");
  } else if (heap+heapsize <= addr && addr <= heap+heapsize+pagesize) {
    fprintf(stderr,"invalid access just above the heap\n");
  } else if (stack-pagesize <= addr && addr < stack) {
    fprintf(stderr,"invalid access just below the stack\n");
  } else if (stack+stacksize <= addr && addr < stack+stacksize+pagesize) {
    fprintf(stderr,"invalid access just above the stack\n");
  } else {
    fprintf(stderr, "Segmentation violation\n");
  }

  exit(-1);
}

/* Signal handler for bus errors */
static void bus_handler(int signo) {
  fprintf(stderr, "Bus error\n");
  exit(-1);
}

static void usage_error(char *who) {
  fprintf(stderr, "usage: %s [-h <heap size>] [-s <stack size>]\n", who);
  fprintf(stderr, "   specify sizes in pages (base 10)\n");
  fprintf(stderr, "   page size is %ld bytes\n",pagesize);
  exit(1);
}

#define SCHEME_PRINTER

#ifdef SCHEME_PRINTER

/* generated from Scheme definitions */
#define word_size 8
#define object_alignment 8
#define shift_fixnum 3
#define mask_fixnum 7
#define tag_fixnum 0
#define mask_pair 7
#define tag_pair 1
#define size_pair 16
#define disp_car 0
#define disp_cdr 8
#define mask_vector 7
#define tag_vector 3
#define disp_vector_length 0
#define disp_vector_data 8
#define mask_procedure 7
#define tag_procedure 2
#define disp_procedure_code 0
#define disp_procedure_data 8
#define mask_boolean 247
#define tag_boolean 6
#define mask_symbol 7
#define tag_symbol 4
#define shift_char 8
#define mask_char 255
#define tag_char 254
#define _false 6
#define _true 14
#define _nil 22
#define _void 30

#define disp_closure_size (-8)
#define continuation_special_flag (-1)
#define disp_continuation_return_address (8)
#define disp_continuation_stack_size (16)
#define disp_continuation_stack (24)
#define disp_symbol_name_length (0)
#define disp_symbol_name (8)

typedef long ptr;

#define UNFIX(x) (x >> shift_fixnum)
#define TAG(x,mask) (x & mask)
#define UNTAG(x,tag) ((x)-tag)
#define CAR(x) (*(ptr *)(UNTAG(x,tag_pair) + disp_car))
#define CDR(x) (*(ptr *)(UNTAG(x,tag_pair) + disp_cdr))
#define VECTORLENGTH(x) (*(ptr *)(UNTAG(x,tag_vector) + disp_vector_length))
#define VECTORDATA(x) ((ptr *)(UNTAG(x,tag_vector) + disp_vector_data))
#define CHAR(x) (x >> shift_char)

#define PROCEDURECODE(x) (*(ptr *)(UNTAG(x,tag_procedure) + disp_procedure_code))
#define PROCEDUREDATA(x) ((ptr *)(UNTAG(x,tag_procedure) + disp_procedure_data))
#define CLOSURESIZE(x) (*(long *)((long)PROCEDURECODE(x) + disp_closure_size))
#define CONTINUATIONRETURNADDRESS(x) \
  (*(void **)(UNTAG(x,tag_procedure) + disp_continuation_return_address))
#define CONTINUATIONSTACKSIZE(x) \
  (*(long *)(UNTAG(x,tag_procedure) + disp_continuation_stack_size))
#define CONTINUATIONSTACK(x) \
  ((ptr *)(UNTAG(x,tag_procedure) + disp_continuation_stack))
#define SYMBOLLENGTH(addr) (*(long *)((char *)(addr) + disp_symbol_name_length))
#define SYMBOLNAME(addr) (ptr *)((char *)(addr) + disp_symbol_name)

#define MAXDEPTH 100
#define MAXLENGTH 1000

static void print_identifier(ptr *str, long length) {
  wchar_t c;
  long i;
  for (i = 0; i < length; i++, str++) {
    c = *str;
    if (/* unprintable or space */
        c <= 32 ||
        /* string */
        c == L'"' ||
        /* comments */
        c == L';' ||
        /* other delimiters */
        c == L'#' ||
        c == L'(' ||
        c == L')' ||
        c == L'[' ||
        c == L']' ||
        c == L'{' ||
        c == L'}' ||
        /* escape character */
        c == '\\' ||
        /* escape between | ... | */
        c == L'|' ||
        /* quote */
        c == L'\'' ||
        /* quasiquote */
        c == L'`' ||
        /* unquote */
        c == L',' ||
        /* numbers */
        c == L'@' && i == 0 ||
        /* exclude +, -,  ->... */
        c == L'-' && i == 0 && length != 1 && (wchar_t)str[1] != L'>' ||
        c == L'+' && i == 0 && length != 1 ||
        /* exclude `...' */
        c == L'.' && i == 0 && !(length == 3 &&
                                 (wchar_t)str[1] == L'.' &&
                                 (wchar_t)str[2] == L'.') ||
        L'0' <= c && c <= L'9' && i == 0
        )
      wprintf(L"\\x%x;", c);
    else
      wprintf(L"%C", c);
  }
}

static void print1(ptr x, int d) {
  if (TAG(x, mask_fixnum) == tag_fixnum) {
    wprintf(L"%ld", (long)UNFIX(x));
  } else if (TAG(x, mask_pair) == tag_pair) {
    int len = 0;
    ptr y;
    
    if (d > MAXDEPTH) {
      wprintf(L"(...)");
      return;
    }
    wprintf(L"(");
    print1(CAR(x), d+1);
    y = CDR(x);
    while (TAG(y, mask_pair) == tag_pair && (len < MAXLENGTH-1)) {
      wprintf(L" ");
      print1(CAR(y), d+1);
      y = CDR(y);
      len++;
    }
    if (y != _nil)
      if (len == MAXLENGTH-1)
        wprintf(L" ...");
      else {
        wprintf(L" . ");
        print1(y, d+1);
      }
    wprintf(L")");
  } else if (TAG(x, mask_vector) == tag_vector) {
    long i, n;
    ptr *p;
    if (d > MAXDEPTH) {
      wprintf(L"#(...)");
      return;
    }
    wprintf(L"#(");
    n = UNFIX(VECTORLENGTH(x));
    p = VECTORDATA(x);
    i = n > MAXLENGTH ? MAXLENGTH : n;
    if (i != 0) {
      print1(*p, d+1);
      while (--i) {
        wprintf(L" ");
        print1(*++p, d+1);
      }
    }
    if (n > MAXLENGTH) wprintf(L" ..."); 
    wprintf(L")");
  } else if (TAG(x, mask_procedure) == tag_procedure) {
    int n = CLOSURESIZE(x);
    if (n == continuation_special_flag)
      wprintf(L"#<continuation>");
    else
      wprintf(L"#<procedure>");
  } else if (x == _false) {
    wprintf(L"#f");
  } else if (x == _true) {
    wprintf(L"#t");
  } else if (x == _nil) {
    wprintf(L"()");
  } else if (x == _void) {
    wprintf(L"#<void>");
  } else if (TAG(x, mask_symbol) == tag_symbol) {
    ptr *addr = SCHEME_SYMBOL_TO_ADDRESS(x);
    print_identifier(SYMBOLNAME(addr), SYMBOLLENGTH(addr));
  } else if (TAG(x, mask_char) == tag_char) {
    wchar_t c = CHAR(x);
    if (c <= 32)
      wprintf(L"#\\x%x", c);
    else
      wprintf(L"#\\%C", c);
  }
}

static void print(ptr x) {
    print1(x, 0);
}

#else /* SCHEME_PRINTER */

static void print(long x) {
    wprintf(L"%ld", x);
} 

#endif /* SCHEME_PRINTER */

/* pre-defined procedure */

#define disp_frame_size (-8)
#define disp_live_mask_end (-8)

#define FRAME_SIZE(x) (*(long *)((long)x + disp_frame_size))
#define LIVE_MASK_END(x) ((char *)((long)x + disp_live_mask_end))
#define ITH_LIVE(s, i) (*(s + (i >> 3)) & (1 << (i & 0b111)))
#define MAXFRAME 100

static ptr walk(void *ra, ptr *top) {
  int d = 0;
  int has_fv = 0;

  while (d < MAXFRAME) {
    if (ra == SCHEME_EXIT || d > MAXFRAME) {
      wprintf(L";   #<system continuation>\n");
      return _void;
    } else {
      wprintf(L";   #<continuation>\n");
      has_fv = 0;
    }
    long size = FRAME_SIZE(ra);
    long nptr = UNFIX(size);
    long nbyte = ((nptr - 1) >> 3) + 1;
    char *mask_start = LIVE_MASK_END(ra) - nbyte;
    ptr *bot = top - nptr;
    for (long i = 1; i < nptr; i++)
      if (ITH_LIVE(mask_start, i)) {
        if (!has_fv) {
          has_fv = 1;
          wprintf(L";     frame variables:\n");
        }
        wprintf(L";     %ld.\t\t", i);
        print(*(bot + i));
        wprintf(L"\n");
      }
    ra = (void *)bot[0];
    top = bot;
    d++;
  }
  wprintf(L"...\n");
  return _void;
}

ptr inspect(ptr x) {
  if (TAG(x, mask_procedure) == tag_procedure) {
    int n = CLOSURESIZE(x);
    if (n == continuation_special_flag)
      return walk(CONTINUATIONRETURNADDRESS(x),
                  CONTINUATIONSTACK(x) + UNFIX(CONTINUATIONSTACKSIZE(x)));
  }
  wprintf(L";   ");
  print(x);
  wprintf(L"\n");
  return _void;
}

ptr write_ptr(ptr x) {
  print(x);
  return _void;
}

ptr display_ptr(ptr x) {
  if (TAG(x, mask_char) == tag_char) {
    wchar_t c = CHAR(x);
    wprintf(L"%C", c);
  } else
    print(x);
  return _void;
}

/* GC */
static ptr *new_heap;
static ptr *new_heap_end;
static ptr *alloc_ptr;

#define DOTAG(x, tag) ((ptr)(x) + (tag))
#define FORWARDED(x) (new_heap <= (ptr *)(x) && (ptr *)(x) < new_heap_end)

static void collect_stack(void *ra, ptr *top);

static ptr *collect_one(ptr *p) {
  ptr x = *p;
  if (TAG(x, mask_pair) == tag_pair) {
    ptr car = CAR(x);
    if (TAG(car, mask_pair) == tag_pair &&
        FORWARDED(UNTAG(car, tag_pair))) {
      *p = car;
      return p + 1;
    } else {
      *p = CAR(x) = DOTAG(alloc_ptr, tag_pair);
      *(alloc_ptr++) = car;
      *(alloc_ptr++) = CDR(x);
      return p + 1;
    }
  } else if (TAG(x, mask_symbol) == tag_symbol) {
    return p + 1;
  } else if (x == _nil) {
    return p + 1;
  } else if (TAG(x, mask_procedure) == tag_procedure) {
    if (FORWARDED(UNTAG(x, tag_procedure)) ||
        (heap <= (char *)(UNTAG(x, tag_procedure)) &&
         (char *)(UNTAG(x, tag_procedure)) < heap + heapsize)) {
      ptr code = PROCEDURECODE(x);
      if (FORWARDED(UNTAG(code, tag_procedure))) {
        *p = code;
        return p + 1;
      } else {
        long size = CLOSURESIZE(x);
        if (size == continuation_special_flag) {
          long size = CONTINUATIONSTACKSIZE(x);
          *p = PROCEDURECODE(x) = DOTAG(alloc_ptr, tag_procedure);
          *(alloc_ptr++) = code;
          *(alloc_ptr++) = (ptr)CONTINUATIONRETURNADDRESS(x);
          memcpy(alloc_ptr, CONTINUATIONSTACK(x), size);
          alloc_ptr += UNFIX(size);
          return p + 1;
        } else {
          ptr *data = PROCEDUREDATA(x);
          *p = PROCEDURECODE(x) = DOTAG(alloc_ptr, tag_procedure);
          *(alloc_ptr++) = code;
          memcpy(alloc_ptr, data, size);
          alloc_ptr += UNFIX(size);
          return p + 1;
        }
      }
    } else {
      ptr me = DOTAG(p, tag_procedure);
      long size = CLOSURESIZE(me);
      ptr *end = CONTINUATIONSTACK(me) + UNFIX(CONTINUATIONSTACKSIZE(me));
      if (size == continuation_special_flag) {
        collect_stack(CONTINUATIONRETURNADDRESS(me), end);
        return end;
      } else
        return p + 1;
    }
  } else if (TAG(x, mask_fixnum) == tag_fixnum) {
    return p + 1;
  } else if (x == _false) {
    return p + 1;
  } else if (x == _true) {
    return p + 1;
  } else if (TAG(x, mask_char) == tag_char) {
    return p + 1;
  } else if (TAG(x, mask_vector) == tag_vector) {
    long length = VECTORLENGTH(x);
    /* forwarded? */
    if (TAG(length, mask_vector) == tag_vector) {
      *p = length;
      return p + 1;
    } else {
      *p = VECTORLENGTH(x) = DOTAG(alloc_ptr, tag_vector);
      ptr *data = VECTORDATA(x);
      *(alloc_ptr++) = length;
      memcpy(alloc_ptr, data, length);
      alloc_ptr += UNFIX(length);
      return p + 1;
    }
  } else if (x == _void) {
    return p + 1;
  } else {
    fprintf(stderr, "unrecognized object %ld during collection\n", x);
    exit(5);
  }
}

static void collect_stack(void *ra, ptr *top) {
  while (ra != SCHEME_EXIT) {
    long size = FRAME_SIZE(ra);
    long nptr = UNFIX(size);
    long nbyte = ((nptr - 1) >> 3) + 1;
    char *mask_start = LIVE_MASK_END(ra) - nbyte;
    ptr *bot = top - nptr;
    for (long i = 1; i < nptr; i++)
      if (ITH_LIVE(mask_start, i))
        collect_one(bot + i);
    ra = (void*)bot[0];
    top = bot;
  }
}

static void free_area(char *addr, long n) {
  munmap(addr - pagesize, n + 2 * pagesize);
}

/* static char *unguarded_area(long n) { */
/*   char *addr; */

/*  /\* allocate, leaving room for guard pages *\/ */
/*   addr = (char *)mmap(NULL, */
/*                       (size_t)(n + 2 * pagesize), */
/*                       PROT_READ | PROT_WRITE, */
/*                       MAP_PRIVATE | MAP_ANON, */
/*                       -1, 0); */
/*   if (addr == (char *)-1) { */
/*     fprintf(stderr, "mmap failed: %s\n", strerror(errno)); */
/*     exit(2); */
/*   } */

/*   return addr + pagesize; */
/* } */

/* static void fit_heap(char *addr, long new, long n) { */
/*   addr = mremap(addr - pagesize, */
/*                 (size_t)(new + 2 * pagesize), */
/*                 (size_t)(n + 2 * pagesize), */
/*                 0); */
/*   if (addr == (char *)-1) { */
/*     fprintf(stderr, "mremap failed: %s\n", strerror(errno)); */
/*     exit(2); */
/*   } */
/* } */

/* static void guard_area(char *addr, long n) { */
/*  /\* remove access rights from the guard pages *\/ */
/*   if (mprotect(addr - pagesize, (size_t)pagesize, PROT_NONE) || */
/*     mprotect(addr + n, (size_t)pagesize, PROT_NONE)) { */
/*     fprintf(stderr, "mprotect failed: %s\n", strerror(errno)); */
/*     exit(3); */
/*   } */
/* } */

/* #include <time.h> */

ptr collect(ptr **allocation_pointer, ptr **end_of_allocation,
            void *ra, ptr *top, ptr tagged, char *exceeded_ap) {
  /* struct timespec begin, end; */
  /* clock_gettime(CLOCK_REALTIME, &begin); */

  /* static int n = 0; */
  /* n += 1; */
  /* fprintf(stderr, "%d-th garbage collection. asking for %ld extra bytes\n", n, extra); */
  /* fprintf(stderr, "  old heap size: %ld \tbytes\n", heap_end - heap); */

  long tag = TAG(tagged, mask_pair); /* all objects share this tag */
  long extra = (long)exceeded_ap - UNTAG(tagged, tag);

  new_heap = (ptr *)two_heap[1 - heap_index];
  new_heap_end = (ptr *)((char *)new_heap + heapsize);
  alloc_ptr = new_heap;
  ptr *scan_ptr = new_heap;
  collect_stack(ra, top);
  while (scan_ptr < alloc_ptr)
    scan_ptr = collect_one(scan_ptr);

  long used = (char *)alloc_ptr - (char *)new_heap;
  long new_heapsize = heapsize;
  while (new_heapsize < 2 * (used + extra))
    new_heapsize *= 2;
  if (new_heapsize > heapsize) {
    free_area(heap, heapsize);
    two_heap[heap_index] = guarded_area(new_heapsize);

    heap = (char *)new_heap;
    new_heap = (ptr *)two_heap[heap_index];
    new_heap_end = (ptr *)((char *)new_heap + new_heapsize);
    alloc_ptr = new_heap;
    ptr *scan_ptr = new_heap;
    collect_stack(ra, top);
    while (scan_ptr < alloc_ptr)
      scan_ptr = collect_one(scan_ptr);

    free_area(heap, heapsize);
    two_heap[1 - heap_index] = guarded_area(new_heapsize);
    heapsize = new_heapsize;
  } else {
    heap_index = 1 - heap_index;
  }

  heap = (char *)new_heap;
  *allocation_pointer = (ptr *)((char *)alloc_ptr + extra);
  *end_of_allocation = (ptr *)((char *)heap + heapsize);

  /* clock_gettime(CLOCK_REALTIME, &end); */
  /* long nanosec = (end.tv_sec - begin.tv_sec) * 1e9 + end.tv_nsec - begin.tv_nsec; */
  /* gc_time += nanosec; */

  return (long)alloc_ptr + tag;
}
