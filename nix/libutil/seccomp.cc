#if __linux__
#include <util.hh>
#include <seccomp.hh>
#include <algorithm>

namespace nix {

struct FilterInstruction {
    struct sock_filter instruction;
    bool fallthroughJt = false;
    bool fallthroughJf = false;
    bool fallthroughK = false;
};

/* Note: instructions in "out" should have already verified that sysno is
 * >= ranges[lowIndex].low.  The value to compare against should already be
 * in the accumulator. */
static void
rangeActionsToFilter(std::vector<Uint32RangeAction> & ranges,
                     size_t lowIndex, /* Inclusive */
                     size_t end, /* Exclusive */
                     std::vector<FilterInstruction> & out)
{
    if(lowIndex >= end) return;

    if(end == lowIndex + 1) {
        FilterInstruction branch;
        Uint32RangeAction range = ranges.at(lowIndex);
        branch.instruction = BPF_JUMP(BPF_JMP | BPF_JGT | BPF_K,
                                      range.high,
                                      /* To be fixed up */
                                      0,
                                      0);
        branch.fallthroughJt = true;
        out.push_back(branch);
        for(auto & i : range.instructions) {
            FilterInstruction f;
            f.instruction = i;
            out.push_back(f);
        }
        FilterInstruction fallthroughBranch;
        fallthroughBranch.instruction = BPF_JUMP(BPF_JMP | BPF_JA | BPF_K,
                                                 /* To be fixed up */
                                                 0,
                                                 0,
                                                 0);
        fallthroughBranch.fallthroughK = true;
        out.push_back(fallthroughBranch);
        return;
    }

    size_t middle = lowIndex + ((end - lowIndex) / 2);
    Uint32RangeAction range = ranges.at(middle);
    FilterInstruction branch;
    size_t branchIndex = out.size();
    branch.instruction = BPF_JUMP(BPF_JMP | BPF_JGE | BPF_K,
                                  range.low,
                                  0,
                                  /* To be fixed up a little farther down */
                                  0);
    out.push_back(branch);
    rangeActionsToFilter(ranges, middle, end, out);
    size_t elseIndex = out.size();
    out[branchIndex].instruction.jf = (elseIndex - branchIndex - 1);
    rangeActionsToFilter(ranges, lowIndex, middle, out);
}


static bool compareRanges(Uint32RangeAction a, Uint32RangeAction b)
{
    return (a.low < b.low);
}


/* Produce a loop-unrolled binary search of RANGES for the u32 currently in
 * the accumulator.  If the binary search finds a range that contains it, it
 * will execute the corresponding instructions.  If these instructions fall
 * through, or if no containing range is found, control resumes after the last
 * instruction in the returned sequence. */
std::vector<struct sock_filter>
rangeActionsToFilter(std::vector<Uint32RangeAction> & ranges)
{
    if(ranges.size() == 0) return {};
    std::sort(ranges.begin(), ranges.end(), compareRanges);
    if(ranges.size() > 1) {
        for(auto & i : ranges)
            if(i.low > i.high)
                throw Error("Invalid range in rangeActionsToFilter");
        for(size_t j = 1; j < ranges.size(); j++)
            if(ranges[j].low <= ranges[j - 1].high)
                throw Error("Overlapping ranges in rangeActionsToFilter");
    }
    std::vector<FilterInstruction> out;
    Uint32RangeAction first = ranges.at(0);
    FilterInstruction branch;
    /* Verify accumulator value is >= first.low, to satisfy initial invariant */
    branch.instruction = BPF_JUMP(BPF_JMP | BPF_JGE | BPF_K,
                                  first.low,
                                  0,
                                  /* to be fixed up */
                                  0);
    branch.fallthroughJf = true;
    out.push_back(branch);
    rangeActionsToFilter(ranges, 0, ranges.size(), out);
    size_t fallthrough = out.size();
    std::vector<struct sock_filter> out2;
    for(size_t j = 0; j < out.size(); j++) {
        if(out[j].fallthroughJt) out[j].instruction.jt = (fallthrough - j - 1);
        if(out[j].fallthroughJf) out[j].instruction.jf = (fallthrough - j - 1);
        if(out[j].fallthroughK) out[j].instruction.k = (fallthrough - j - 1);
        out2.push_back(out[j].instruction);
    }
    return out2;
}


/* If the uint64 at offset OFFSET has value VALUE, run INSTRUCTIONS.
 * Otherwise, or if INSTRUCTIONS falls through, continue past the last
 * instruction of OUT at the time seccompMatchu64 returns.  Clobbers
 * accumulator! */
std::vector<struct sock_filter> seccompMatchu64(std::vector<struct sock_filter> & out,
                                                uint64_t value,
                                                std::vector<struct sock_filter> instructions,
                                                uint32_t offset)
{
    /* Note: this only works where the order of bytes in uint64 is big or
     * little endian, and the same order holds for uint32. */
    /* Load lower-addressed 32 bits */
    out.push_back(BPF_STMT(BPF_LD | BPF_W | BPF_ABS, offset));
    size_t jmp1Index = out.size();

    out.push_back(BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K,
#ifdef WORDS_BIGENDIAN
                           (uint32_t)((value >> 32) & 0xffffffff),
#else
                           (uint32_t)(value & 0xffffffff),
#endif
                           0,
                           /* To be fixed up */
                           0));
    /* Load higher-addressed 32 bits */
    out.push_back(BPF_STMT(BPF_LD | BPF_W | BPF_ABS, offset + (uint32_t)sizeof(uint32_t)));
    size_t jmp2Index = out.size();
    out.push_back(BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K,
#ifdef WORDS_BIGENDIAN
                           (uint32_t)(value & 0xffffffff),
#else
                           (uint32_t)((value >> 32) & 0xffffffff),
#endif
                           0,
                           /* To be fixed up */
                           0));

    out.insert(out.end(), instructions.begin(), instructions.end());
    out[jmp1Index].jf = (out.size() - jmp1Index - 1);
    out[jmp2Index].jf = (out.size() - jmp2Index - 1);
    return out;
}

}

#endif
