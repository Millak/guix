#include "config.h"

#include <iostream>
#include <cstring>
#include <cassert>
#include <string_view>
#include <format>

#include "hash.hh"
#include "archive.hh"
#include "util.hh"

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>


namespace nix {


Hash::Hash()
{
    type = htUnknown;
    hashSize = 0;
    memset(hash, 0, maxHashSize);
}


Hash::Hash(HashType type)
{
    this->type = type;
    hashSize = gcry_md_get_algo_dlen(type);

    if (hashSize == 0) throw Error("unknown hash type");
    assert(hashSize <= maxHashSize);
    memset(hash, 0, maxHashSize);
}


bool Hash::operator == (const Hash & h2) const
{
    if (hashSize != h2.hashSize) return false;
    for (unsigned int i = 0; i < hashSize; i++)
        if (hash[i] != h2.hash[i]) return false;
    return true;
}


bool Hash::operator != (const Hash & h2) const
{
    return !(*this == h2);
}


bool Hash::operator < (const Hash & h) const
{
    for (unsigned int i = 0; i < hashSize; i++) {
        if (hash[i] < h.hash[i]) return true;
        if (hash[i] > h.hash[i]) return false;
    }
    return false;
}


const string base16Chars = "0123456789abcdef";


string printHash(const Hash & hash)
{
    std::vector<char> buf(hash.hashSize * 2);
    for (unsigned int i = 0; i < hash.hashSize; i++) {
        buf[i * 2] = base16Chars[hash.hash[i] >> 4];
        buf[i * 2 + 1] = base16Chars[hash.hash[i] & 0x0f];
    }
    return string(buf.begin(), buf.end());
}


Hash parseHash(HashType ht, std::string_view s)
{
    Hash hash(ht);
    if (s.length() != hash.hashSize * 2) {
	string algo = gcry_md_algo_name(ht);
        throw Error(std::format("invalid {} hash '{}' ({} bytes but expected {})",
		    algo, s, (s.length() / 2), hash.hashSize));
    }
    for (unsigned int i = 0; i < hash.hashSize; i++) {
        string s2(s, i * 2, 2);
        if (!isxdigit(s2[0]) || !isxdigit(s2[1]))
            throw Error(std::format("invalid hash `{}'", s));
        std::istringstream str(s2);
        int n;
        str >> std::hex >> n;
        hash.hash[i] = n;
    }
    return hash;
}


unsigned int hashLength32(const Hash & hash)
{
    return (hash.hashSize * 8 - 1) / 5 + 1;
}


// omitted: E O U T
const string base32Chars = "0123456789abcdfghijklmnpqrsvwxyz";


string printHash32(const Hash & hash)
{
    Hash hash2(hash);
    unsigned int len = hashLength32(hash);

    string s;
    s.reserve(len);

    for (int n = len - 1; n >= 0; n--) {
        unsigned int b = n * 5;
        unsigned int i = b / 8;
        unsigned int j = b % 8;
        unsigned char c =
            (hash.hash[i] >> j)
            | (i >= hash.hashSize - 1 ? 0 : hash.hash[i + 1] << (8 - j));
        s.push_back(base32Chars[c & 0x1f]);
    }

    return s;
}


string printHash16or32(const Hash & hash)
{
    return hash.type == htMD5 ? printHash(hash) : printHash32(hash);
}


Hash parseHash32(HashType ht, std::string_view s)
{
    Hash hash(ht);
    unsigned int len = hashLength32(ht);
    assert(s.size() == len);

    for (unsigned int n = 0; n < len; ++n) {
        char c = s[len - n - 1];
        unsigned char digit;
        for (digit = 0; digit < base32Chars.size(); ++digit) /* !!! slow */
            if (base32Chars[digit] == c) break;
        if (digit >= 32)
            throw Error(std::format("invalid base-32 hash '{}'", s));
        unsigned int b = n * 5;
        unsigned int i = b / 8;
        unsigned int j = b % 8;
        hash.hash[i] |= digit << j;
        if (i < hash.hashSize - 1) hash.hash[i + 1] |= digit >> (8 - j);
    }

    return hash;
}


Hash parseHash16or32(HashType ht, std::string_view s)
{
    Hash hash(ht);
    if (s.size() == hash.hashSize * 2)
        /* hexadecimal representation */
        hash = parseHash(ht, s);
    else if (s.size() == hashLength32(hash))
        /* base-32 representation */
        hash = parseHash32(ht, s);
    else
        throw Error(std::format("hash `{}' has wrong length for hash type `{}'",
            s, printHashType(ht)));
    return hash;
}


bool isHash(std::string_view s)
{
    if (s.length() != 32) return false;
    for (int i = 0; i < 32; i++) {
        char c = s[i];
        if (!((c >= '0' && c <= '9') ||
              (c >= 'a' && c <= 'f')))
            return false;
    }
    return true;
}

/* The "hash context".  */
struct Ctx
{
  /* This copy constructor is needed in 'HashSink::currentHash()' where we
     expect the copy of a 'Ctx' object to yield a truly different context.  */
  Ctx(Ctx &ref)
  {
    if (ref.md_handle == NULL)
      md_handle = NULL;
    else
      gcry_md_copy (&md_handle, ref.md_handle);
  }

  /* Make sure 'md_handle' is always initialized.  */
  Ctx(): md_handle (NULL) { };

  gcry_md_hd_t md_handle;
};


static void start(HashType ht, Ctx & ctx)
{
    gcry_error_t err;

    err = gcry_md_open (&ctx.md_handle, ht, 0);
    assert (err == GPG_ERR_NO_ERROR);
}


static void update(HashType ht, Ctx & ctx,
    const unsigned char * bytes, unsigned int len)
{
    gcry_md_write (ctx.md_handle, bytes, len);
}


static void finish(HashType ht, Ctx & ctx, unsigned char * hash)
{
    memcpy (hash, gcry_md_read (ctx.md_handle, ht),
	    gcry_md_get_algo_dlen (ht));
    gcry_md_close (ctx.md_handle);
    ctx.md_handle = NULL;
}


Hash hashString(HashType ht, const string & s)
{
    Ctx ctx;
    Hash hash(ht);
    start(ht, ctx);
    update(ht, ctx, (const unsigned char *) s.data(), s.length());
    finish(ht, ctx, hash.hash);
    return hash;
}


Hash hashFile(HashType ht, const Path & path)
{
    Ctx ctx;
    Hash hash(ht);
    start(ht, ctx);

    AutoCloseFD fd = open(path.c_str(), O_RDONLY);
    if (fd == -1) throw SysError(std::format("computing hash of file `{}'", path));

    unsigned char buf[8192];
    ssize_t n;
    while ((n = read(fd, buf, sizeof(buf)))) {
        checkInterrupt();
        if (n == -1) throw SysError(std::format("reading file `{}'", path));
        update(ht, ctx, buf, n);
    }

    finish(ht, ctx, hash.hash);
    return hash;
}


HashSink::HashSink(HashType ht) : ht(ht)
{
    ctx = new Ctx;
    bytes = 0;
    start(ht, *ctx);
}

HashSink::~HashSink()
{
    bufPos = 0;
    delete ctx;
}

void HashSink::write(const unsigned char * data, size_t len)
{
    bytes += len;
    update(ht, *ctx, data, len);
}

HashResult HashSink::finish()
{
    flush();
    Hash hash(ht);
    nix::finish(ht, *ctx, hash.hash);
    return HashResult(hash, bytes);
}

HashResult HashSink::currentHash()
{
    flush();
    Ctx ctx2 = *ctx;
    Hash hash(ht);
    nix::finish(ht, ctx2, hash.hash);
    return HashResult(hash, bytes);
}


HashResult hashPath(
    HashType ht, const Path & path, PathFilter & filter)
{
    HashSink sink(ht);
    dumpPath(path, sink, filter);
    return sink.finish();
}


Hash compressHash(const Hash & hash, unsigned int newSize)
{
    Hash h;
    h.hashSize = newSize;
    for (unsigned int i = 0; i < hash.hashSize; ++i)
        h.hash[i % newSize] ^= hash.hash[i];
    return h;
}


HashType parseHashType(const string & s)
{
    if (s == "md5") return htMD5;
    else if (s == "sha1") return htSHA1;
    else if (s == "sha256") return htSHA256;
    else if (s == "sha512") return htSHA512;
    else if (s == "sha3-256") return htSHA3_256;
    else if (s == "sha3-512") return htSHA3_512;
    else if (s == "blake2s-256") return htBLAKE2s_256;
    else return htUnknown;
}


string printHashType(HashType ht)
{
    if (ht == htMD5) return "md5";
    else if (ht == htSHA1) return "sha1";
    else if (ht == htSHA256) return "sha256";
    else if (ht == htSHA512) return "sha512";
    else if (ht == htSHA3_256) return "sha3-256";
    else if (ht == htSHA3_512) return "sha3-512";
    else if (ht == htBLAKE2s_256) return "blake2s-256";
    else throw Error("cannot print unknown hash type");
}


}
