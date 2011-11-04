#define _XOPEN_SOURCE 500

#include <unistd.h>
#include <stdio.h>
#include <stdint.h>
#include <endian.h>
#include <linux/nbd.h>
#include <string.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <caml/alloc.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/bigarray.h>
#include <caml/unixsupport.h>

CAMLprim value caml_maid_pread(value ml_fd, value ml_buffer, value ml_off) {
    CAMLparam3(ml_fd, ml_buffer, ml_off);
//    fprintf(stderr, "### caml_maid_pread()\n");
    int fd = Int_val(ml_fd);
    struct caml_ba_array *array = Caml_ba_array_val(ml_buffer);
    size_t len = caml_ba_byte_size(array);
    uint8_t *buf = Caml_ba_data_val(ml_buffer);
    off_t off = Int64_val(ml_off);

    ssize_t res = pread(fd, buf, len, off);

    // FIXME: throw exception on error?
    // Return -1 on EOF and 0 if there is nothing to read
    if (res == -1 && (errno == EAGAIN || errno == EWOULDBLOCK)) {
        res = 0;
    } else if (res == 0) {
        res = -1;
    }
//    fprintf(stderr, "      res = %d\n", (int)res);
    CAMLreturn(Val_int(res));
}
