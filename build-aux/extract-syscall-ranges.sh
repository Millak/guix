#!/bin/sh

if test "$#" -lt 1 || test "$#" -gt 2
then
    echo "Usage: extract-syscall-ranges.sh FILENAME [abiname_regex]"
    exit 1
fi

numbers_to_ranges()
{
    if ! read number
    then
        printf '{}\n'
        return
    fi
    low="$number"
    high="$number"
    while true
    do
        if read number
        then
            if test "$number" -eq "$((high + 1))"
            then
                high="$number"
            else
                break
            fi
        else
            printf '{ {%d, %d} }\n' "$low" "$high"
            return
        fi
    done
    printf '{ {%d, %d}' "$low" "$high"
    low="$number"
    high="$number"
    while true
    do
        if read number
        then
            if test "$number" -eq "$((high + 1))"
            then
                high="$number"
            else
                printf ', {%d, %d}' "$low" "$high"
                low="$number"
                high="$number"
            fi
        else
            printf ', {%d, %d} }\n' "$low" "$high"
            return
        fi
    done
}

if test "$#" -eq 2
then
    abi_regex="$2"
    getnumbers()
    {
        # delete comment lines and space-only lines
        sed -e '/^[[:space:]]*#/d ; /^[[:space:]]*$/d' |
            # filter to only include lines with target abi or "common"
            grep -E "^[0-9]+[[:space:]]+(common|(${abi_regex}))[[:space:]]" |
            # limit to only syscall number
            sed -e 's/\([0-9]\+\).*/\1/g'
    }
else
    getnumbers()
    {
        # delete comment lines and space-only lines and limit to syscall number
        sed -e '/^[[:space:]]*#/d ; /^[[:space:]]*$/d ; s/\([0-9]\+\).*/\1/g'
    }
fi

getnumbers < "$1" |
    sort -n |
    uniq | # Yes, there are duplicate syscall entries...
    numbers_to_ranges


