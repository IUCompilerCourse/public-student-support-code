#include <inttypes.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include "runtime.h"

// To do: we need to account for the "any" type. -Jeremy

// Often misunderstood: static global variables in C are not
// accessible to code outside of the module.
// No one besides the collector ever needs to know tospace exists.
static int64_t* tospace_begin;
static int64_t* tospace_end;

// initialized it set during initialization of the heap, and can be
// checked in order to ensure that initialization has occurred.
static int initialized = 0;

/*
  Tuple Tag (64 bits)
  #b|- 7 bit unused -|- 50 bit field [50, 0] -| 6 bits length -| 1 bit isNotForwarding Pointer
  * If the bottom-most bit is zero, the tag is really a forwarding pointer.
  * Otherwise, its an object tag. In that case, the next
    6 bits give the length of the object (max of 50 64-bit words).
    The next 50 bits say where there are pointers.
    A '1' is a pointer, a '0' is not a pointer.
*/
static const int TAG_IS_NOT_FORWARD_MASK = 1;
static const int TAG_LENGTH_MASK = 126; // 1111110
static const int TAG_LENGTH_RSHIFT = 1;
static const int TAG_PTR_BITFIELD_RSHIFT = 7;

// cheney implements cheney's copying collection algorithm
// There is a stub and explaination below.
static void cheney(int64_t** rootstack_ptr);


// Check to see if a tag is actually a forwarding pointer.
static inline int is_forwarding(int64_t tag) {
  return !(tag & TAG_IS_NOT_FORWARD_MASK);
}

// Get the length field out of a tag.
static inline int get_length(int64_t tag){
  return (tag & TAG_LENGTH_MASK) >> TAG_LENGTH_RSHIFT;
}

// Get the "is pointer bitfield" out of a tag.
static inline int64_t get_ptr_bitfield(int64_t tag){
  return tag >> TAG_PTR_BITFIELD_RSHIFT;
}

// The following needs to stay in sync with the any-tag function
// in dynamic-typing.rkt.  -Jeremy
#define ANY_TAG_MASK 7
#define ANY_TAG_LEN 3
#define ANY_TAG_INT 1    // 001
#define ANY_TAG_BOOL 4   // 100
#define ANY_TAG_VEC 2    // 010
#define ANY_TAG_FUN 3    // 011
#define ANY_TAG_VOID 5   // 101
#define ANY_TAG_PTR 0 // not an any, a raw pointer

int any_tag(int64_t any) {
  return any & ANY_TAG_MASK;
}

int is_ptr(int64_t* p) {
  int64_t q = (int64_t)p;
  if (q == 0) {
    return 0;
  } else {
    int t = any_tag(q);
    return t == ANY_TAG_PTR || t == ANY_TAG_VEC;
  }
}

int64_t* to_ptr(int64_t* p) {
  int64_t q = (int64_t)p;
  if (any_tag(q) == ANY_TAG_PTR)
    return p;
  else
    return (int64_t*)(q & ~ANY_TAG_MASK);
}


// initialize the state of the collector so that allocations can occur
void initialize(uint64_t rootstack_size, uint64_t heap_size)
{
  // 1. Check to make sure that our assumptions about the world are correct.
  assert(sizeof(int64_t) == sizeof(int64_t*));
  assert((heap_size % sizeof(int64_t)) == 0);
  assert((rootstack_size % sizeof(int64_t)) == 0);

  // 2. Allocate memory (You should always check if malloc gave you memory)
  if (!(fromspace_begin = malloc(heap_size))) {
    printf("Failed to malloc %" PRIu64 " byte fromspace\n", heap_size);
    exit(EXIT_FAILURE);
  }

  if (!(tospace_begin = malloc(heap_size))) {
    printf("Failed to malloc %" PRIu64 " byte tospace\n", heap_size);
    exit(EXIT_FAILURE);
  }

  if (!(rootstack_begin = malloc(rootstack_size))) {
    printf("Failed to malloc %" PRIu64 " byte rootstack", rootstack_size);
    exit(EXIT_FAILURE);
  }

  // 2.5 Calculate the ends memory we are using.
  // Note: the pointers are for a half open interval [begin, end)
  fromspace_end = fromspace_begin + (heap_size / sizeof(int64_t));
  tospace_end = tospace_begin + (heap_size / sizeof(int64_t));
  rootstack_end = rootstack_begin + (rootstack_size / sizeof(int64_t));

  // 3 Initialize the global free pointer
  free_ptr = fromspace_begin;

  // Useful for debugging
  initialized = 1;

}

void collect(int64_t** rootstack_ptr, uint64_t bytes_requested)
{
#if 0
  printf("collecting, need %ld\n", bytes_requested);
  print_heap(rootstack_ptr);
#endif

  // 1. Check our assumptions about the world
  assert(initialized);
  assert(rootstack_ptr >= rootstack_begin);
  assert(rootstack_ptr < rootstack_end);

#ifndef NDEBUG
  // All pointers in the rootstack point to fromspace
  for (unsigned int i = 0; rootstack_begin + i < rootstack_ptr; i++){
    int64_t* root = rootstack_begin[i];
    if (is_ptr(root)) {
      int64_t* a_root = to_ptr(root);
      assert(fromspace_begin <= a_root && a_root < fromspace_end);
    }
  }
#endif

  // 2. Perform collection
  cheney(rootstack_ptr);

  // 3. Check if collection freed enough space in order to allocate
  if (sizeof(int64_t) * (fromspace_end - free_ptr) < bytes_requested){
    //printf("resizing the heap\n");
    /*
       If there is not enough room left for the bytes_requested,
       allocate larger tospace and fromspace.

       In order to determine the new size of the heap double the
       heap size until it is bigger than the occupied portion of
       the heap plus the bytes requested.

       This covers the corner case of heaps objects that are
       more than half the size of the heap. No a very likely
       scenario but slightly more robust.

       One corner case that isn't handled is if the heap is size
       zero. My thought is that malloc probably wouldn't give
       back a pointer if you asked for 0 bytes. Thus initialize
       would fail, but our runtime-config.rkt file has a contract
       on the heap_size parameter that the code generator uses
       to determine initial heap size to this is a non-issue
       in reality.
    */

    unsigned long occupied_bytes = (free_ptr - fromspace_begin) * sizeof(int64_t);
    unsigned long needed_bytes = occupied_bytes + bytes_requested;
    unsigned long old_len = fromspace_end - fromspace_begin;
    unsigned long old_bytes = old_len * sizeof(int64_t);
    unsigned long new_bytes = old_bytes;

#if 0
    // this version is good for debugging purposes -Jeremy
    new_bytes = needed_bytes;
#else
    while (new_bytes <= needed_bytes) {
      new_bytes = 2 * new_bytes;
    }
#endif

    // Free and allocate a new tospace of size new_bytes
    free(tospace_begin);

    if (!(tospace_begin = malloc(new_bytes))) {
      printf("failed to malloc %ld byte fromspace", new_bytes);
      exit(EXIT_FAILURE);
    }

    tospace_end = tospace_begin + new_bytes / (sizeof(int64_t));

    // The pointers on the stack and in the heap must be updated,
    // so this cannot be just a memcopy of the heap.
    // Performing cheney's algorithm again will have the correct
    // effect, and we have already implemented it.
    cheney(rootstack_ptr);


    // Cheney flips tospace and fromspace. Thus, we allocate another
    // tospace not fromspace as we might expect.
    free(tospace_begin);

    if (!(tospace_begin = malloc(new_bytes))) {
      printf("failed to malloc %ld byte tospace", new_bytes);
      exit(EXIT_FAILURE);
    }

    tospace_end = tospace_begin + new_bytes / (sizeof(int64_t));
  }

  assert(free_ptr < fromspace_end);
  assert(free_ptr >= fromspace_begin);
#ifndef NDEBUG
  // All pointers in the rootstack point to fromspace
  for (unsigned long i = 0; rootstack_begin + i < rootstack_ptr; i++){
    int64_t* root = rootstack_begin[i];
    if (is_ptr(root)) {
      int64_t* a_root = to_ptr(root);
      assert(fromspace_begin <= a_root && a_root < fromspace_end);
    }
  }
  // All pointers in fromspace point to fromspace
  /*printf("validating pointers in fromspace [%lld, %lld)\n",
    (int64_t)fromspace_begin, (int64_t)fromspace_end);*/
  int64_t* scan_ptr = fromspace_begin;
  while (scan_ptr != free_ptr){
    int64_t tag = *scan_ptr;
    unsigned char len = get_length(tag);
    //printf("\tlen %d\n", (int)len);
    int64_t isPtrBits = get_ptr_bitfield(tag);
    int64_t* data = scan_ptr + 1;
    scan_ptr = scan_ptr + len + 1;
    for (unsigned char i = 0; i != len; i++){
      if ((isPtrBits >> i) & 1){
        int64_t* ptr = (int64_t*) data[i];
    if (is_ptr(ptr)) {
      /*printf("\ti: %d, ptr: %lld, to_ptr: %lld", i,
         (int64_t)ptr,
         (int64_t)to_ptr(ptr));*/
      int64_t* real_ptr = to_ptr(ptr);
      assert(real_ptr < fromspace_end);
      assert(real_ptr >= fromspace_begin);
    }
      }
    }
  }
#endif

#if 0
  printf("finished collecting\n");
  print_heap(rootstack_ptr);
  printf("---------------------------------------\n");
#endif
} // collect

// copy_vector is responsible for doing a pointer oblivious
// move of vector data and updating the vector pointer with
// the new address of the data.
// There is a stub and explaination for copy_vector below.
static void copy_vector(int64_t** vector_ptr_loc);

/*
  The cheney algorithm takes a pointer to the top of the rootstack.
  It resets the free pointer to be at the begining of tospace, copies
  (or reallocates) the data pointed to by the roots into tospace and
  replaces the pointers in the rootset with pointers to the
  copies. (See the description of copy_vector below).

  While this initial copying of root vectors is occuring the free_ptr
  has been maintained to remain at the next free memory location in
  tospace. Cheney's algorithm then scans a vector at a time until it
  reaches the free_ptr.

  At each vector we use the meta information stored in the vector tag
  to find the length of the vector and tell which fields inside the
  vector are vector pointers. Each new vector pointer must have its
  data copied and every vector pointer must be updated to to point to
  the copied data. (The description of copy_vector will help keep this
  organized.

  This process is a breadth first graph traversal. Copying a vector
  places its contents at the end of a Fifo queue and scanning a vector
  removes it. Eventually the graph traversal will run out of unseen
  nodes "catch up" to the free pointer. When this occurs we know that
  all live data in the program is contained by tospace, and that
  everything left in fromspace is unreachable by the program.

  After this point the free pointer will be pointing into what until
  now we considered tospace. This means the program will allocate
  object here. In order to keep track of the we "flip" fromspace and
  tospace by making the fromspace pointers point to tospace and vice
  versa.
*/

void cheney(int64_t** rootstack_ptr)
{
  // printf("cheney: starting copy, rootstack=%p\n", rootstack_ptr);
  int64_t* scan_ptr = tospace_begin;
  free_ptr = tospace_begin;

  /* traverse the root set to create the initial queue */
  for (int64_t** root_loc = rootstack_begin;
       root_loc != rootstack_ptr;
       ++root_loc) {
    /*
      We pass copy vector the pointer to the pointer to the vector.
      This is because we need to be able to rewrite the pointer after
      the object has been moved.
    */
    copy_vector(root_loc);
  }

  /*
     Here we need to scan tospace until we reach the free_ptr pointer.
     This will end up being a breadth first search of the pointers in
     from space.
  */
  while (scan_ptr != free_ptr) {
    /*
       I inlined this to leave maniuplation of scan_ptr to a single
       function. This can be accomplished by passing the location
       of the pointer into helper, but let's not make reasoning
       through the algorithm any harder.

       The invarient of the outer loop is that scan_ptr is either
       at the front of a vector, or == to free_ptr.
    */

    // Since this tag is already in tospace we know that it isn't
    // a forwarding pointer.
    int64_t tag = *scan_ptr;

    // the length of the vector is contained in bits [1,6]
    int len = get_length(tag);

    // Find the next vector or the next free_ptr;
    // with is len + 1 away from the current;
    int64_t* next_ptr = scan_ptr + len + 1;

    // each bit low to high says if the next index is a ptr
    int64_t isPointerBits = get_ptr_bitfield(tag);

    // Advance the scan_ptr then check to
    // see if we have arrived at the beginning of the next array.
    scan_ptr += 1;
    while(scan_ptr != next_ptr){
      if ((isPointerBits & 1) == 1) {
        // since the tag says that the scan ptr in question is a
        // ptr* we known that scan_ptr currently points to a ptr*
        // and must be a ptr** itself.
        copy_vector((int64_t**)scan_ptr);
      }
      // Advance the tag so the next check is for the next scan ptr
      isPointerBits = isPointerBits >> 1;
      scan_ptr += 1;
    }

  }

  /* swap the tospace and fromspace */
  int64_t* tmp_begin = tospace_begin;
  int64_t* tmp_end = tospace_end;
  tospace_begin = fromspace_begin;
  tospace_end = fromspace_end;
  fromspace_begin = tmp_begin;
  fromspace_end = tmp_end;
  //printf("cheney: finished copy\n");
}


/*
 copy_vector takes a pointer, (`location`) to a vector pointer,
 copies the vector data from fromspace into tospace, and updates the
 vector pointer so that it points to the the data's new address in
 tospace.

  Precondition:
    *  original vector pointer location
    |
    V
   [*] old vector pointer
    |
    +-> [tag or forwarding pointer | ? | ? | ? | ...] old vector data

 Postcondition:
    * original vector pointer location
    |
    V
   [*] new vector pointer
    |
    |   [ * forwarding pointer | ? | ? | ? | ...] old vector data
    |     |
    |     V
    +---->[tag | ? | ? | ? | ...] new vector data

 Since multiple pointers to the same vector can exist within the
 memory of the program this may or may not be the first time
 we called `copy_vector` on a location that contains this old vector
 pointer. In order to tell if we have copied the old vector data previously we
 check the vector information tag (`tag = old_vector_pointer[0]`).

 If the forwarding bit is set, then is_forwarding(tag) will return
 false and we know we haven't already copied the data. In order to
 figure out how much data to copy we can inspect the tag's length
 field. The length field indicates the number of 64-bit words the
 array is storing for the user, so we need to copy `length + 1` words
 in total, including the tag. After performing the
 copy we need to leave a forwarding pointer in old data's tag field
 to indicate the new address to subsequent copy_vector calls for this
 vector pointer. Furthermore, we need to store the new vector's pointer
 at the location where where we found the old vector pointer.

 If the tag is a forwarding pointer, the `is_forwarding(tag) will return
 true and we need to update the location storing the old vector pointer to
 point to the new data instead).

 As a side note any time you are allocating new data you must maintain
 the invariant that the free_ptr points to the next free memory address.

*/
void copy_vector(int64_t** vector_ptr_loc)
{

  int64_t* old_vector_ptr = *vector_ptr_loc;
  int old_tag = any_tag((int64_t)old_vector_ptr);

  if (! is_ptr(old_vector_ptr))
    return;
  old_vector_ptr = to_ptr(old_vector_ptr);
#if 0
  printf("copy_vector %ll\n", (int64_t)old_vector_ptr);
#endif

  int64_t tag = old_vector_ptr[0];

  // If our search has already moved the vector then we
  //  would have left a forwarding pointer.

  if (is_forwarding(tag)) {
    //printf("\talready copied to %lld\n", tag);
    // Since we left a forwarding pointer, we have already
    // moved this vector. All we need to do is update the pointer
    // that was pointing to the old vector. The
    // forwarding pointer says where the new copy is.
    *vector_ptr_loc = (int64_t*) (tag | old_tag);

  } else {
#if 0
    printf("\tfirst time copy\n");
#endif
    // This is the first time we have followed this pointer.

    // Since we are about to jumble all the pointers around lets
    // set up some structure to the world.

    // The tag we grabbed earlier contains some usefull info for
    // forwarding copying the vector.
    int length = get_length(tag);
#if 0
    printf("\tlen: %d\n", length);
#endif

    // The new vector is going to be where the free int64_t pointer
    // currently points.
    int64_t* new_vector_ptr = free_ptr;
#if 0
    printf("\tto address: %ld\n", (int64_t)new_vector_ptr);
#endif

    // Copy the old vector to the new one.
    // The "length" is the number of elements, so to include the
    // tag, we need to iterate from 0 to length + 1;
    for (int i = 0; i != length + 1; i++){
      new_vector_ptr[i] = old_vector_ptr[i];
    }

    // the free ptr can be updated to point to the next free ptr.
    free_ptr = free_ptr + length + 1;

    // We need to set the forwarding pointer in the old_vector
    old_vector_ptr[0] = (int64_t) new_vector_ptr;

    // And where we found the old vector we need to update the
    // pointer to point to the new vector
    new_vector_ptr = (int64_t*)((int64_t)new_vector_ptr | old_tag);
    *vector_ptr_loc = new_vector_ptr;
  }
}



// Read an integer from stdin
int64_t read_int() {
  int64_t i;
  scanf("%" SCNd64, &i);
  return i;
}

// print an integer to stdout
void print_int(int64_t x) {
  printf("%" PRId64, x);
}

// print a bool to stdout
void print_bool(int64_t x) {
  if (x){
    printf("#t");
  } else {
    printf("#f");
  }
}

void print_void() {
  printf("#<void>");
}

void print_vecbegin() {
  printf("#(");
}

void print_space() {
  printf(" ");
}

void print_vecend() {
  printf(")");
}

void print_ellipsis() {
  printf("#(...)");
}

/* to do: need to cycle detection. -Jeremy */
void print_any(int64_t any) {
  switch (any_tag(any)) {
  case ANY_TAG_INT:
    printf("%" PRId64, any >> ANY_TAG_LEN);
    break;
  case ANY_TAG_BOOL:
    if (any >> ANY_TAG_LEN) {
      printf("#t");
    } else {
      printf("#f");
    }
    break;
  case ANY_TAG_VEC: {
    int64_t* vector_ptr = (int64_t*) (any & ~ANY_TAG_MASK);
    int64_t tag = vector_ptr[0];
    unsigned char len = get_length(tag);
    printf("#(");
    for (int i = 0; i != len; ++i) {
      print_any(vector_ptr[i + 1]); // this is wrong -Jeremy
    }
    printf(")");
    break;
  }
  case ANY_TAG_FUN:
    printf("#<procedure>");
    break;
  case ANY_TAG_VOID:
    printf("#<void>");
    break;
  default:
    printf("unrecognized!");
    exit(-1);
  }
}

void print_heap(int64_t** rootstack_ptr)
{
  printf("rootstack len = %ld\n", rootstack_ptr - rootstack_begin);
  for (int64_t** root_loc = rootstack_begin;
       root_loc != rootstack_ptr;
       ++root_loc) {
    if (is_ptr(*root_loc)) {
      print_vector(to_ptr(*root_loc));
    } else {
      printf("%lld", (int64_t)*root_loc);
    }
    printf("\n");
  }
  printf("\n");
}

void print_vector(int64_t* vector_ptr)
{
  int64_t tag = vector_ptr[0];
  unsigned char len = get_length(tag);
  int64_t* scan_ptr = vector_ptr;
  int64_t* next_ptr = vector_ptr + len + 1;

  printf("%lld=#(", (int64_t)vector_ptr);
  scan_ptr += 1;
  int64_t isPointerBits = get_ptr_bitfield(tag);
  while (scan_ptr != next_ptr) {
    if ((isPointerBits & 1) == 1 && is_ptr((int64_t*)*scan_ptr)) {
      print_vector(to_ptr((int64_t*)*scan_ptr));
    } else {
      printf("%lld", (int64_t)*scan_ptr);
    }
    isPointerBits = isPointerBits >> 1;
    scan_ptr += 1;
    if (scan_ptr != next_ptr) {
      printf(", ");
    }
  }
  printf(")");
}

