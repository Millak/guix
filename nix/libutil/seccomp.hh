#pragma once

#include "util.hh"
#include <cstdint>
#include <linux/audit.h> /* For AUDIT_ARCH_* */
#include <linux/seccomp.h>
#include <linux/filter.h>


/* This file provides two preprocessor macros (among other things):
   1. AUDIT_ARCH_NATIVE, which evaluates to whichever of the AUDIT_ARCH_*
      values best represents the target system.  Linux's internal headers have
      a SECCOMP_ARCH_NATIVE since 2020, but it's not user-visible.  Detection
      of this is based on src/arch.c in libseccomp.
   2. NATIVE_SYSCALL_RANGES, an array initializer for an array of two-element
      objects, the first of which is an integral number representing the
      start (inclusive) of a range of valid syscall numbers, and the second
      of which is an integral number representing the end (inclusive) of that
      range of valid syscall numbers.  The ranges provided are all
      non-overlapping and strictly ascending (that is, the start of a range is
      strictly higher than any of the numbers in any of the ranges that
      precede it).  All numbers involved fit into a long.

      These ranges were generated from the various syscall.tbl,
      syscall_32.tbl, and syscall_64.tbl files lying around in the linux
      kernel source.  Some were derived from
      include/uapi/asm-generic/unistd.h.  The kernel source used was commit
      b3ee1e460951 of https://github.com/torvalds/linux.git, read on
      2025-04-23.  Not all of the gaps in the files have any comments pointing
      them out, so I recommend using build-aux/extract-syscall-ranges.sh for
      the *.tbl files.

      The intent behind saving these ranges is to be able to use a
      default-allow seccomp policy that nevertheless disallows future
      syscalls.  This ensures that our security analysis can work with a
      static, well-defined set of system calls that won't grow in the future
      unless someone explicitly revisits the system call tables to consider
      the implications of the new additions. */

/* Both ends are inclusive.  Some of the .tbl files use strange entries for
 * the "abi" field, check arch/$ARCH/kernel/Makefile.syscalls to see what it
 * specifies for syscall_abis_32 and syscall_abis_64 in addition to 32 or 64
 * and "common" (added in Makefile.asm-headers).  Also check what, if
 * anything, the makefile uses as the --offset flag to syscallhdr.sh.  And
 * look at arch/$ARCH/include/uapi/asm/unistd.h to see what value the offset
 * takes in what configurations. */

#ifndef AUDIT_ARCH_NATIVE

#if __i386__
#define AUDIT_ARCH_NATIVE AUDIT_ARCH_I386
#define NATIVE_SYSCALL_RANGES { {0, 221}, {224, 250}, {252, 284}, {286, 386},  \
                         {393, 414}, {416, 466} }
#elif __x86_64__
#define AUDIT_ARCH_NATIVE AUDIT_ARCH_X86_64
#ifdef __ILP32__
#include <asm/unistd.h>
#define X32RANGE(low, high)   { (low | __X32_SYSCALL_BIT), (high | __X32_SYSCALL_BIT) }
#define NATIVE_SYSCALL_RANGES \
  { X32RANGE(0, 12), X32RANGE(14, 14), X32RANGE(17, 18), X32RANGE(21, 44), X32RANGE(48, 53), \
    X32RANGE(56, 58), X32RANGE(60, 100), X32RANGE(102, 126), X32RANGE(130, 130), \
    X32RANGE(132, 133), X32RANGE(135, 155), X32RANGE(157, 173), X32RANGE(175, 176), \
    X32RANGE(179, 179), X32RANGE(181, 204), X32RANGE(207, 208), X32RANGE(210, 210), \
    X32RANGE(212, 213), X32RANGE(216, 221), X32RANGE(223, 235), X32RANGE(237, 243), \
    X32RANGE(245, 245), X32RANGE(248, 272), X32RANGE(275, 277), X32RANGE(280, 294), \
    X32RANGE(298, 298), X32RANGE(300, 306), X32RANGE(308, 309), X32RANGE(312, 321), \
    X32RANGE(323, 326), X32RANGE(329, 335), X32RANGE(424, 466), X32RANGE(512, 547) }
#else
#define NATIVE_SYSCALL_RANGES { {0, 335}, {424, 466} }
#endif
#elif __arm__
#define AUDIT_ARCH_NATIVE AUDIT_ARCH_ARM
/* Note: there are at present 6 extra ARM syscall numbers not listed in
   arch/arm/tools/syscall.tbl, namely __ARM_NR_breakpoint through
   __ARM_NR_get_tls. */
#ifdef __ARM_EABI__
#include <asm/unistd.h>
#define NATIVE_SYSCALL_RANGES \
  { {0, 6}, {8, 12}, {14, 16}, {19, 21}, {23, 24}, {26, 26}, {29, 29},  \
    {33, 34}, {36, 43}, {45, 47}, {49, 52}, {54, 55}, {57, 57}, {60, 67}, \
    {70, 75}, {77, 81}, {83, 83}, {85, 88}, {91, 97}, {99, 100}, {103, 108}, \
    {111, 111}, {114, 116}, {118, 122}, {124, 126}, {128, 129}, {131, 136}, \
    {138, 165}, {168, 187}, {190, 221}, {224, 253}, {256, 401}, {403, 414}, \
    {416, 446}, {448, 466}, {(__ARM_NR_BASE + 1), (__ARM_NR_BASE + 6)} }
#else
#include <asm/unistd.h>
#define OABIRANGE(low, high)  { (low | __NR_OABI_SYSCALL_BASE), (high | __NR_OABI_SYSCALL_BASE) }
#define NATIVE_SYSCALL_RANGES \
  { OABIRANGE(0, 6), OABIRANGE(8, 16), OABIRANGE(19, 27), OABIRANGE(29, 30), \
    OABIRANGE(33, 34), OABIRANGE(36, 43), OABIRANGE(45, 47), OABIRANGE(49, 52), \
    OABIRANGE(54, 55), OABIRANGE(57, 57), OABIRANGE(60, 67), OABIRANGE(70, 83), \
    OABIRANGE(85, 97), OABIRANGE(99, 100), OABIRANGE(102, 108), \
    OABIRANGE(111, 111), OABIRANGE(113, 122), OABIRANGE(124, 126), \
    OABIRANGE(128, 129), OABIRANGE(131, 136), OABIRANGE(138, 165), \
    OABIRANGE(168, 187), OABIRANGE(190, 221), OABIRANGE(224, 253), \
    OABIRANGE(256, 401), OABIRANGE(403, 414), OABIRANGE(416, 446), \
    OABIRANGE(448, 466), {(__ARM_NR_BASE + 1), (__ARM_NR_BASE + 6)} }
#endif
#elif __aarch64__
#define AUDIT_ARCH_NATIVE AUDIT_ARCH_AARCH64
/* extract-syscall-ranges.sh $LINUXSOURCE/arch/arm64/tools/syscall_64.tbl \
   '64|renameat|rlimit|memfd_secret'

   the extra ABIs are taken from arch/arm64/kernel/Makefile.syscalls and
   scripts/Makefile.asm-headers */
#define NATIVE_SYSCALL_RANGES { {0, 243}, {260, 294}, {424, 466} }
/* To my knowledge there is no x32 equivalent for aarch64 in mainline linux */
#elif __mips__ && _MIPS_SIM == _MIPS_SIM_ABI32
/* o32 abi in both endianness cases */
#include <asm/unistd.h>
#define SYSRANGE(low, high) {(low) + __NR_Linux, (high) + __NR_Linux}
#define NATIVE_SYSCALL_RANGES \
  { SYSRANGE(0, 278), SYSRANGE(280, 368), SYSRANGE(393, 414), \
    SYSRANGE(416, 446), SYSRANGE(448, 466) }
#if __MIPSEB__
#define AUDIT_ARCH_NATIVE AUDIT_ARCH_MIPS;
#elif __MIPSEL__
#define AUDIT_ARCH_NATIVE AUDIT_ARCH_MIPSEL
#endif
#elif __mips__ && _MIPS_SIM == _MIPS_SIM_ABI64
/* n64 abi in both endianness cases */
#include <asm/unistd.h>
#define SYSRANGE(low, high) {(low) + __NR_Linux, (high) + __NR_Linux}
#define NATIVE_SYSCALL_RANGES \
  { SYSRANGE(0, 237), SYSRANGE(239, 328), SYSRANGE(424, 446), SYSRANGE(448, 466) }
#if __MIPSEB__
#define AUDIT_ARCH_NATIVE AUDIT_ARCH_MIPS64
#elif __MIPSEL__
#define AUDIT_ARCH_NATIVE AUDIT_ARCH_MIPSEL64
#endif /* _MIPS_SIM_ABI64 */
#elif __mips__ && _MIPS_SIM == _MIPS_SIM_NABI32
/* n32 abi in both endianness cases */
#include <asm/unistd.h>
#define SYSRANGE(low, high) {(low) + __NR_Linux, (high) + __NR_Linux}
#define NATIVE_SYSCALL_RANGES \
  { SYSRANGE(0, 241), SYSRANGE(243, 332), SYSRANGE(403, 414), \
    SYSRANGE(416, 446), SYSRANGE(448, 466) }
#if __MIPSEB__
#define AUDIT_ARCH_NATIVE AUDIT_ARCH_MIPS64N32
#elif __MIPSEL__
#define AUDIT_ARCH_NATIVE AUDIT_ARCH_MIPSEL64N32
#endif /* _MIPS_SIM_NABI32 */
#elif __hppa64__ /* hppa64 must be checked before hppa */
#define NATIVE_SYSCALL_RANGES \
  { {0, 101}, {103, 126}, {128, 129}, {131, 136}, {138, 166}, \
    {168, 168}, {170, 195}, {198, 202}, {206, 212}, {215, 219}, \
    {222, 262}, {264, 302}, {304, 356}, {424, 446}, {448, 466} }
#define AUDIT_ARCH_NATIVE AUDIT_ARCH_PARISC64
#elif __hppa__
#define NATIVE_SYSCALL_RANGES \
  { {0, 101}, {103, 126}, {128, 129}, {131, 136}, {138, 166}, \
    {168, 168}, {170, 195}, {198, 202}, {206, 212}, {215, 219}, \
    {222, 262}, {264, 302}, {304, 356}, {403, 414}, {416, 446}, {448, 466} }
#define AUDIT_ARCH_NATIVE AUDIT_ARCH_PARISC
#elif __PPC64__
#define NATIVE_SYSCALL_RANGES \
  { {0, 191}, {198, 203}, {205, 223}, {225, 225}, {227, 253}, \
    {255, 256}, {258, 365}, {378, 388}, {392, 402}, {424, 446}, {448, 466} }
#ifdef __BIG_ENDIAN__
#define AUDIT_ARCH_NATIVE AUDIT_ARCH_PPC64
#else
#define AUDIT_ARCH_NATIVE AUDIT_ARCH_PPC64LE
#endif
#elif __PPC__
#define NATIVE_SYSCALL_RANGES \
  { {0, 223}, {225, 256}, {258, 365}, {378, 388}, {393, 414}, \
    {416, 446}, {448, 466} }
#define AUDIT_ARCH_NATIVE AUDIT_ARCH_PPC
#elif __s390x__ /* s390x must be checked before s390 */
#define NATIVE_SYSCALL_RANGES \
  { {1, 12}, {14, 15}, {19, 22}, {26, 27}, {29, 30}, {33, 34}, \
    {36, 43}, {45, 45}, {48, 48}, {51, 52}, {54, 55}, {57, 57}, \
    {60, 67}, {72, 75}, {77, 79}, {83, 83}, {85, 94}, {96, 97}, \
    {99, 100}, {102, 108}, {110, 112}, {114, 122}, {124, 137}, \
    {141, 163}, {167, 169}, {172, 181}, {183, 191}, {198, 220}, \
    {222, 222}, {224, 241}, {243, 262}, {265, 386}, {392, 402}, {424, 466} }
#define AUDIT_ARCH_NATIVE AUDIT_ARCH_S390X
#elif __s390__
#define NATIVE_SYSCALL_RANGES \
  { {1, 16}, {19, 27}, {29, 30}, {33, 34}, {36, 43}, {45, 52}, \
    {54, 55}, {57, 57}, {60, 67}, {70, 81}, {83, 83}, {85, 97}, \
    {99, 108}, {110, 112}, {114, 122}, {124, 165}, {167, 241}, \
    {243, 262}, {264, 386}, {393, 414}, {416, 466} }
#define AUDIT_ARCH_NATIVE AUDIT_ARCH_S390
#elif __riscv && __riscv_xlen == 64
#define NATIVE_SYSCALL_RANGES { {0, 37}, {39, 243}, {258, 294}, {424, 466} }
#define AUDIT_ARCH_NATIVE AUDIT_ARCH_RISCV64
#elif __riscv && __riscv_xlen == 32
#define NATIVE_SYSCALL_RANGES \
  { {0, 3}, {5, 37}, {39, 71}, {74, 78}, {81, 85}, {89, 97}, {99, 100}, \
    {102, 107}, {109, 109}, {111, 111}, {116, 126}, {128, 136}, {138, 162}, \
    {165, 168}, {172, 181}, {184, 191}, {193, 242}, {258, 259}, {261, 265}, \
    {267, 291}, {293, 294}, {403, 414}, {416, 466} }
#define AUDIT_ARCH_NATIVE AUDIT_ARCH_RISCV32
#else
#error cannot determine which AUDIT_ARCH_* value to use for AUDIT_ARCH_NATIVE
#endif

#else
#ifndef NATIVE_SYSCALL_RANGES
/* Fall back to default-allow if the user specified (with
   -DAUDIT_ARCH_NATIVE=...) an arch but not NATIVE_SYSCALL_RANGES */
#define NATIVE_SYSCALL_RANGES {}
#endif
#endif /* #ifndef AUDIT_ARCH_NATIVE */

namespace nix {

struct Uint32RangeAction {
    uint32_t low; /* inclusive */
    uint32_t high; /* inclusive */
    std::vector<struct sock_filter> instructions;
};

std::vector<struct sock_filter> rangeActionsToFilter(std::vector<Uint32RangeAction> & ranges);


std::vector<struct sock_filter>
seccompMatchu64(std::vector<struct sock_filter> & out,
                uint64_t value,
                std::vector<struct sock_filter> instructions,
                uint32_t offset);
}
