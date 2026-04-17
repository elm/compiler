/* Shim for musl libc: snap-server references sendfile64 (a glibc extension).
 * On musl, off_t is always 64-bit, so sendfile and sendfile64 are identical. */
#include <sys/sendfile.h>

ssize_t sendfile64(int out_fd, int in_fd, off_t *offset, size_t count)
{
    return sendfile(out_fd, in_fd, offset, count);
}
